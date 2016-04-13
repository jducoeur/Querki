package querki.photos

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.xml.NodeSeq

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import scalatags.Text.all.{span => htmlSpan, _}

import models.{Collection, DelegatingType, DisplayPropVal, FullInputRendering, HtmlWikitext, Kind, Property, PropertyBundle, PType, PTypeBuilder, ThingState, Wikitext}
import models.MIMEType.MIMEType

import querki.basic.PlainText
import querki.ecology._
import querki.time.DateTime
import querki.types.{ModeledPropertyBundle, ModelTypeDefiner, SimplePropertyBundle}
import querki.util.{Config,QLog, XmlHelpers}
import querki.values.{ElemValue, QLContext, SpaceState}

private [photos] object MOIDs extends EcotIds(52) {
  val PhotoTypeOID = moid(1)
  val PhotoModelOID = moid(2)
  val ImageHeightOID = moid(3)
  val ImageWidthOID = moid(4)
  val ImageMIMETypeOID = moid(5)
  val ImageFilenameOID = moid(6)
  val ImageTimestampOID = moid(7)
  val ImageSizeOID = moid(8)
  val ImageThumbnailFilenameOID = moid(9)
  val ImageThumbnailHeightOID = moid(10)
  val ImageThumbnailWidthOID = moid(11)
  val ThumbnailFuncOID = moid(12)
  val PreferredImageSizeOID = moid(13)
  val PhotoTargetFuncOID = moid(14)
  val ImageCaptionOID = moid(15)
}

private [photos] trait PhotosInternal extends EcologyInterface {
  def PhotoType:PType[ModeledPropertyBundle] with PTypeBuilder[ModeledPropertyBundle, SimplePropertyBundle]
  
  def ImageHeightProp:Property[Int,Int]
  def ImageWidthProp:Property[Int,Int]
  def ImageMIMETypeProp:Property[PlainText,String]
  def ImageFilenameProp:Property[PlainText,String]
  def ImageTimestampProp:Property[DateTime,DateTime]
  def ImageSizeProp:Property[Int,Int]
  def ImageThumbnailFilenameProp:Property[PlainText,String]
  def ImageThumbnailHeightProp:Property[Int,Int]
  def ImageThumbnailWidthProp:Property[Int,Int]
  def PreferredImageSizeProp:Property[Int,Int]
}

class PhotoEcot(e:Ecology) extends QuerkiEcot(e) with ModelTypeDefiner with EcologyMember with querki.core.MethodDefs
  with PhotosInternal 
{
  import MOIDs._
  import PhotoUploadActor._
  
  val Basic = initRequires[querki.basic.Basic]
  // Note that this needs to be named something other than QL, because the inherited QL is lazy, and this needs
  // to not be.
  val RQL = initRequires[querki.ql.QL]
  val Time = initRequires[querki.time.Time]
  val Types = initRequires[querki.types.Types]
  
  lazy val SystemOnly = Basic.SystemOnlyProp(true)

  lazy val bucketUrl = Config.getString("querki.aws.bucketUrl")
  
  val PhotosTag = "Photos and Images"
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val ImageHeightProp = new SystemProperty(ImageHeightOID, IntType, Optional,
    toProps(
      setName("Image Height"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property),
      Categories(PhotosTag),
      Summary("The height of this Image")))
  
  lazy val ImageWidthProp = new SystemProperty(ImageWidthOID, IntType, Optional,
    toProps(
      setName("Image Width"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property),
      Categories(PhotosTag),
      Summary("The width of this Image")))
  
  lazy val ImageMIMETypeProp = new SystemProperty(ImageMIMETypeOID, Basic.PlainTextType, Optional,
    toProps(
      setName("Image MIME Type"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property),
      Categories(PhotosTag),
      Summary("The type of Image this is (eg, JPEG, PNG)")))
  
  lazy val ImageFilenameProp = new SystemProperty(ImageFilenameOID, Basic.PlainTextType, Optional,
    toProps(
      setName("Image Filename"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property),
      Categories(PhotosTag),
      Summary("The name of this Image's file, which is used to fetch it"))) 
  
  lazy val ImageTimestampProp = new SystemProperty(ImageTimestampOID, Time.QDateTime, Optional,
    toProps(
      setName("Image Timestamp"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property),
      Categories(PhotosTag),
      Summary("When this Image was loaded")))
  
  lazy val ImageSizeProp = new SystemProperty(ImageSizeOID, IntType, Optional,
    toProps(
      setName("Image Size"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property),
      Categories(PhotosTag),
      Summary("The size of this image on disc")))
  
  lazy val ImageThumbnailFilenameProp = new SystemProperty(ImageThumbnailFilenameOID, Basic.PlainTextType, Optional,
    toProps(
      setName("Image Thumbnail Filename"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property),
      Categories(PhotosTag),
      Summary("The name of this Image's thumbnail file")))
  
  lazy val ImageThumbnailHeightProp = new SystemProperty(ImageThumbnailHeightOID, IntType, Optional,
    toProps(
      setName("Image Thumbnail Height"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property),
      Categories(PhotosTag),
      Summary("The height of this Image's thumbnail")))
  
  lazy val ImageThumbnailWidthProp = new SystemProperty(ImageThumbnailWidthOID, IntType, Optional,
    toProps(
      setName("Image Thumbnail Width"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property),
      Categories(PhotosTag),
      Summary("The width of this Image's thumbnail")))
  
  lazy val PreferredImageSizeProp = new SystemProperty(PreferredImageSizeOID, IntType, Optional,
    toProps(
      setName("Preferred Image Size"),
      Types.MaxIntValueProp(1000),
      Core.AppliesToKindProp(Kind.Property),
      Types.AppliesToTypesProp(PhotoType),
      Categories(PhotosTag),
      Summary("Add this to a Photo Property in order to say how big photos should come out"),
      Details("""Querki is designed for "web resolution" photographs -- that is, photos should fit on a webpage.
          |To that end, we limit photos to no bigger than 1000 pixels on a side. (1 "Megapixel") But that is still
          |pretty large, filling much of a computer screen, and it is bigger than you want for some purposes. If
          |you would like your photos to be smaller than 1000 pixels on a side, set this on your Photo Property, to
          |say what the size should be.
          |
          |Setting this value says how big a photo can be created. It has no effect on your existing photos; it just
          |controls the ones you create after changing this value.
          |
          |Note that you can not set this number to be larger than 1000 at this time. This may change in the future,
    	  |but photo storage costs real money, and we have to keep things limited until we have paid memberships. Sorry.""".stripMargin)))
  
  lazy val ImageCaptionProp = new SystemProperty(ImageCaptionOID, RQL.ParsedTextType, Optional,
    toProps(
      setName("Image Caption"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property)))
    
  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  lazy val ThumbnailFunction = new InternalMethod(ThumbnailFuncOID,
  	toProps(
  	  setName("_thumbnail"),
      Categories(PhotosTag),
  	  Summary("Show the thumbnail version of the received photos"),
  	  Details("""```
              |PHOTOS -> _thumbnail -> SMALL PHOTOS
              |```
  	          |
  	          |Each photo in Querki automatically has a "thumbnail" version, which is much smaller and quicker
  	          |to load. It is usually appropriate to show the thumbnail when you are listing a bunch of photos,
  	          |instead of each full-sized one.""".stripMargin)))
  {
  	override def qlApply(inv:Invocation):QFut = {
  	  for {
  	    dummy <- inv.returnsType(PhotoType)
  	    bundle <- inv.contextAllBundles
  	  }
  	    yield ExactlyOne(ElemValue(bundle, ThumbnailType))
  	}
  }
  
  lazy val PhotoTargetFunction = new InternalMethod(PhotoTargetFuncOID,
    toProps(
      setName("_photoTarget"),
      Categories(PhotosTag),
      Summary("The target location to show one of a List of Photos"),
      Details("""```
          |LIST OF PHOTOS PROPERTY -> _photoTarget
          |```
          |
          |When you have a List of Photos, you usually don't want to show all of them full-sized on the page. Instead,
          |you usually want to show one full-sized Photo somewhere and some thumbnails; when you click on a thumbnail, it
          |should change the full-sized Photo. That is what _photoTarget is for.
          |
          |When you pass a List of Photos into _photoTarget, it displays only the *first* Photo in that list. But if you
          |display the thumbnails somewhere else on the page like this --
          |```
          |LIST OF PHOTOS PROPERTY -> _thumbnail
          |```
          |Then whenever a user clicks on a thumbnail, it will replace the full-sized picture. This way, you can have a
          |lot of Photos, but only take up a bit of display space on the page.
          |
          |It is legal, but usually pointless, to use this function with Exactly One or Optional Photo.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        dummy <- inv.returnsType(PhotoType)
        // Note that we intentionally only use the first one, but we need to guard against the received value
        // being empty:
        rawValue <- inv.contextValue
        if (rawValue.size > 0)
        bundle <- inv.contextFirstAs(PhotoType)
      }
        yield ExactlyOne(ElemValue(bundle, TargetType))
    }
  }
  
  override lazy val props = Seq(
    ImageHeightProp,
    ImageWidthProp,
    ImageMIMETypeProp,
    ImageFilenameProp,
    ImageTimestampProp,
    ImageSizeProp,
    ImageThumbnailFilenameProp,
    ImageThumbnailHeightProp,
    ImageThumbnailWidthProp,
    PreferredImageSizeProp,
    ImageCaptionProp,
    
    ThumbnailFunction,
    PhotoTargetFunction
  )

  /******************************************
   * TYPES
   ******************************************/
  
  lazy val PhotoModel = ThingState(PhotoModelOID, systemOID, RootOID,
    toProps(
      setName("Photo Model"),
      setInternal,
      Categories(PhotosTag)))
      
  override lazy val things = Seq(
    PhotoModel
  )

  lazy val PhotoType = new ModelType(PhotoTypeOID, PhotoModelOID,
    toProps(
      setName("Photo Type"),
      Basic.ExplicitProp(true),
      Categories(PhotosTag),
      Summary("A single Photograph in Querki"))) with FullInputRendering
  {
    override def doWikify(context:QLContext)(v:ModeledPropertyBundle, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
      implicit val s = context.state
      val result = for {
        filename <- v.getFirstOpt(ImageFilenameProp)
        width <- v.getFirstOpt(ImageWidthProp)
        height <- v.getFirstOpt(ImageHeightProp)
        caption = v.getFirstOpt(ImageCaptionProp) match {
          case Some(cap) => Wikitext(s"\n{{carousel-caption:\n") + cap + Wikitext("\n}}")
          case None => Wikitext.empty
        }
        image = HtmlWikitext(s"""<img class="img-responsive center-block" src="$bucketUrl/${filename.raw}" alt="${filename.raw}" />""")
      }
        yield Wikitext(s"\n{{item:\n") + image + caption + Wikitext("\n}}") 
        
      Future.successful(result.getOrElse(Wikitext("")))
    }
    
    /**
     * We wrap List or Sets of Photos up specially, so that the Client can render them nicely.
     */
    override def fullWikify(context:QLContext, qv:QValue, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Option[Future[Wikitext]] = {
      qv.cType match {
        case QList | QSet => {
          Some(qv.cType.doWikify(context)(qv.cv, this, displayOpt, lexicalThing) map { guts =>
            Wikitext("{{_photoList:\n") + guts + Wikitext("\n}}")
          })
        }
        case _ => None
      }
    }
    
    override def doComp(context:QLContext)(left:ModeledPropertyBundle, right:ModeledPropertyBundle):Boolean = {
      // TODO: define a standard meta-Property named Sort Order Property -- you can define this on a Model Type to say
      // which Property to sort on.
      implicit val s = context.state
      left.getFirstOpt(ImageTimestampProp) match {
        case Some(leftTime) => {
          right.getFirstOpt(ImageTimestampProp) match {
            case Some(rightTime) => leftTime isBefore rightTime
            case None => true
          }
        }
        case None => false
      }
    }

    /**
     * All Collections of Photos currently edit pretty much the same way for now.
     */
    def renderInputFull(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal):NodeSeq = {
      val currentPhotosOpt = currentValue.effectiveV
      val tag =
        htmlSpan(
          currentPhotosOpt.map { currentPhotos =>
            for {
              v <- currentPhotos.rawList(this)
              text = thumbnailText(context, v)
            }
              yield raw(text.getOrElse(""))
          },
          div(cls:="_photoThumbnailFrame _photoEdit _photoAddButton")
        )
        
      XmlHelpers.toNodes(tag)
    }
    
    override def editorSpan(prop:Property[_,_]):Int = 12
  }
  
  def fromPropStr(context:QLContext) = context.fromPropertyOfType(PhotoType).map(prop => s""" data-fromprop="${prop.id.toString}" """).getOrElse("")
  def thumbnailText(context:QLContext, v:ModeledPropertyBundle):Option[String] = {
      implicit val s = context.state
      
      val fromProp = fromPropStr(context)
      
      for {
        filename <- v.getFirstOpt(ImageThumbnailFilenameProp)
        width <- v.getFirstOpt(ImageThumbnailWidthProp)
        height <- v.getFirstOpt(ImageThumbnailHeightProp)
        fullFilename <- v.getFirstOpt(ImageFilenameProp)
        fullWidth <- v.getFirstOpt(ImageWidthProp)
        fullHeight <- v.getFirstOpt(ImageHeightProp)
        fromPropOpt = context.fromPropertyOfType(PhotoType)
      }
        // IMPORTANT: the tags are split across lines so as not to introduce any whitespace between them!
        yield s"""<div class="_photoThumbnailFrame"><span class="_photoThumbnailHelper"></span><img 
            |     src="$bucketUrl/${filename.raw}" class="img-polaroid _photoThumbnail" width="$width" height="$height"
            |     data-fullsrc="$bucketUrl/${fullFilename.raw}" data-fullwidth="$fullWidth" data-fullheight="$fullHeight" alt="${filename.raw}" 
            |     $fromProp/></div>""".stripMargin 
  }
  
  /**
   * A variant of PhotoType, which differs only in that it renders as the thumbnail instead of the actual
   * image. This is what comes out of the _thumbnail function.
   */
  lazy val ThumbnailType = new DelegatingType(PhotoType) {
    override def doWikify(context:QLContext)(v:ModeledPropertyBundle, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
      Future.successful(thumbnailText(context, v).map(HtmlWikitext(_)).getOrElse(Wikitext("")))
    }
  }
  
  /**
   * A variant of PhotoType, which comes from the _photoTarget function. Basically the same as PhotoType, but marks itself as
   * the target for thumbnails.
   */
  lazy val TargetType = new DelegatingType(PhotoType) {
    override def doWikify(context:QLContext)(v:ModeledPropertyBundle, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
      implicit val s = context.state
      val fromProp = fromPropStr(context)
      val result = for {
        filename <- v.getFirstOpt(ImageFilenameProp)
        width <- v.getFirstOpt(ImageWidthProp)
        height <- v.getFirstOpt(ImageHeightProp)
      }
        yield HtmlWikitext(s"""<img class="_photoTarget img-responsive" src="$bucketUrl/${filename.raw}" alt="${filename.raw}" $fromProp/>""")
        
      Future.successful(result.getOrElse(Wikitext("")))
    }    
  }
  
  override lazy val types = Seq(
    PhotoType
  )
}