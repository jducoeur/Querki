package querki.photos

import scala.concurrent.duration._
import scala.concurrent.Future

import scala.xml.NodeSeq

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import models.{Collection, DelegatingType, DisplayPropVal, FullInputRendering, HtmlWikitext, Kind, Property, PropertyBundle, PType, PTypeBuilder, ThingState, Wikitext}
import models.MIMEType.MIMEType

import querki.basic.PlainText
import querki.ecology._
import querki.time.DateTime
import querki.types.{ModeledPropertyBundle, ModelTypeDefiner, SimplePropertyBundle}
import querki.util.{Config,QLog}
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
  with Photos with PhotosInternal 
{
  import MOIDs._
  import PhotoUploadActor._
  
  val Basic = initRequires[querki.basic.Basic]
  val Time = initRequires[querki.time.Time]
  val Types = initRequires[querki.types.Types]
  
  lazy val SystemOnly = Basic.SystemOnlyProp(true)
  
  /**
   * The one true handle to the Photo Upload Manager for this node.
   */
  var _ref:Option[ActorRef] = None
  lazy val photoUploadManager = _ref.get
  
  override def createActors(createActorCb:CreateActorFunc):Unit = {
    // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
    // with "Props(classOf(Space), ...)". See:
    //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
    _ref = createActorCb(Props(new PhotoUploadManager(ecology)), "PhotoUploadManager")
  }
  
  def createWorker():Future[ActorRef] = {
    val fut = photoUploadManager.ask(BeginProcessing())(3 seconds)
    fut.mapTo[ActorRef]
  }
  
  lazy val bucketUrl = Config.getString("querki.aws.bucketUrl")
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val ImageHeightProp = new SystemProperty(ImageHeightOID, IntType, Optional,
    toProps(
      setName("Image Height"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property)))
  
  lazy val ImageWidthProp = new SystemProperty(ImageWidthOID, IntType, Optional,
    toProps(
      setName("Image Width"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property)))
  
  lazy val ImageMIMETypeProp = new SystemProperty(ImageMIMETypeOID, Basic.PlainTextType, Optional,
    toProps(
      setName("Image MIME Type"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property)))
  
  lazy val ImageFilenameProp = new SystemProperty(ImageFilenameOID, Basic.PlainTextType, Optional,
    toProps(
      setName("Image Filename"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property))) 
  
  lazy val ImageTimestampProp = new SystemProperty(ImageTimestampOID, Time.QDateTime, Optional,
    toProps(
      setName("Image Timestamp"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property)))
  
  lazy val ImageSizeProp = new SystemProperty(ImageSizeOID, IntType, Optional,
    toProps(
      setName("Image Size"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property)))
  
  lazy val ImageThumbnailFilenameProp = new SystemProperty(ImageThumbnailFilenameOID, Basic.PlainTextType, Optional,
    toProps(
      setName("Image Thumbnail Filename"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property)))
  
  lazy val ImageThumbnailHeightProp = new SystemProperty(ImageThumbnailHeightOID, IntType, Optional,
    toProps(
      setName("Image Thumbnail Height"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property)))
  
  lazy val ImageThumbnailWidthProp = new SystemProperty(ImageThumbnailWidthOID, IntType, Optional,
    toProps(
      setName("Image Thumbnail Width"),
      SystemOnly,
      Core.AppliesToKindProp(Kind.Property)))
  
  lazy val PreferredImageSizeProp = new SystemProperty(PreferredImageSizeOID, IntType, Optional,
    toProps(
      setName("Preferred Image Size"),
      Types.MaxIntValueProp(1000),
      Core.AppliesToKindProp(Kind.Property),
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
    
  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  lazy val ThumbnailFunction = new InternalMethod(ThumbnailFuncOID,
	toProps(
	  setName("_thumbnail"),
	  Summary("Show the thumbnail version of the received photos"),
	  Details("""    PHOTOS -> _thumbnail -> SMALL PHOTOS
	          |
	          |Each photo in Querki automatically has a "thumbnail" version, which is much smaller and quicker
	          |to load. It is usually appropriate to show the thumbnail when you are listing a bunch of photos,
	          |instead of each full-sized one.""".stripMargin)))
  {
	override def qlApply(inv:Invocation):QValue = {
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
      Summary("The target location to show one of a List of Photos"),
      Details("""    LIST OF PHOTOS PROPERTY -> _photoTarget
          |
          |When you have a List of Photos, you usually don't want to show all of them full-sized on the page. Instead,
          |you usually want to show one full-sized Photo somewhere and some thumbnails; when you click on a thumbnail, it
          |should change the full-sized Photo. That is what _photoTarget is for.
          |
          |When you pass a List of Photos into _photoTarget, it displays only the *first* Photo in that list. But if you
          |display the thumbnails somewhere else on the page like this --
          |
          |    LIST OF PHOTOS PROPERTY -> _thumbnail
          |
          |Then whenever a user clicks on a thumbnail, it will replace the full-sized picture. This way, you can have a
          |lot of Photos, but only take up a bit of display space on the page.
          |
          |It is legal, but usually pointless, to use this function with Exactly One or Optional Photo.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
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
    
    ThumbnailFunction,
    PhotoTargetFunction
  )

  /******************************************
   * TYPES
   ******************************************/
  
  lazy val PhotoModel = ThingState(PhotoModelOID, systemOID, RootOID,
    toProps(
      setName("Photo Model")))
      
  override lazy val things = Seq(
    PhotoModel
  )

  lazy val PhotoType = new ModelType(PhotoTypeOID, PhotoModelOID,
    toProps(
      setName("Photo Type"),
      Basic.ExplicitProp(true),
      Summary("A single Photograph in Querki"))) with FullInputRendering
  {
    override def doWikify(context:QLContext)(v:ModeledPropertyBundle, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Wikitext = {
      implicit val s = context.state
      val result = for {
        filename <- v.getFirstOpt(ImageFilenameProp)
        width <- v.getFirstOpt(ImageWidthProp)
        height <- v.getFirstOpt(ImageHeightProp)
      }
        yield HtmlWikitext(s"""<img src="$bucketUrl/${filename.raw}" width="$width" height="$height" alt="${filename.raw}" />""")
        
      result.getOrElse(Wikitext(""))
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
     * 
     * TODO: we take ownership of the full rendering here, at least for now, instead of using the default
     * mechanisms. In the long run, we should consider that a hack, separating the server-generated logic of
     * "a list of Foos" from the client-side rendering of the actual edit control. 
     */
    def renderInputFull(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal):NodeSeq = {
      implicit val s = context.state
      val cType = prop.cType 
      val verb =
        if (cType == ExactlyOne || cType == Optional) {
          val result = for {
            v <- currentValue.effectiveV
            photoBundle <- v.firstAs(this)
            filename <- photoBundle.getFirstOpt(ImageFilenameProp)
          }
            yield "Replace"
          
          result.getOrElse("Add")
        } else if (cType == QList || cType == QSet) {
          "Add"
        } else {
          // Curious -- an unknown cType of photos!
          "Add"
        }
      // TODO: render the thumbnails of the existing photos, so that they can be rearranged or deleted
      <input class="_photoEdit btn" value={verb + " photo"}></input>
    }
  }
  
  def fromPropStr(context:QLContext) = context.fromPropertyOfType(PhotoType).map(prop => s""" data-fromprop="${prop.id.toString}" """).getOrElse("")
  
  /**
   * A variant of PhotoType, which differs only in that it renders as the thumbnail instead of the actual
   * image. This is what comes out of the _thumbnail function.
   */
  lazy val ThumbnailType = new DelegatingType(PhotoType) {
    override def doWikify(context:QLContext)(v:ModeledPropertyBundle, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Wikitext = {
      implicit val s = context.state
      
      val fromProp = fromPropStr(context)
      
      val result = for {
        filename <- v.getFirstOpt(ImageThumbnailFilenameProp)
        width <- v.getFirstOpt(ImageThumbnailWidthProp)
        height <- v.getFirstOpt(ImageThumbnailHeightProp)
        fullFilename <- v.getFirstOpt(ImageFilenameProp)
        fullWidth <- v.getFirstOpt(ImageWidthProp)
        fullHeight <- v.getFirstOpt(ImageHeightProp)
        fromPropOpt = context.fromPropertyOfType(PhotoType)
      }
        yield HtmlWikitext(s"""<img src="$bucketUrl/${filename.raw}" class="img-polaroid _photoThumbnail" width="$width" height="$height"""" +
            s""" data-fullsrc="$bucketUrl/${fullFilename.raw}" data-fullwidth="$fullWidth" data-fullheight="$fullHeight" alt="${filename.raw}" $fromProp/>""")
        
      result.getOrElse(Wikitext(""))
    }
  }
  
  /**
   * A variant of PhotoType, which comes from the _photoTarget function. Basically the same as PhotoType, but marks itself as
   * the target for thumbnails.
   */
  lazy val TargetType = new DelegatingType(PhotoType) {
    override def doWikify(context:QLContext)(v:ModeledPropertyBundle, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Wikitext = {
      implicit val s = context.state
      val fromProp = fromPropStr(context)
      val result = for {
        filename <- v.getFirstOpt(ImageFilenameProp)
        width <- v.getFirstOpt(ImageWidthProp)
        height <- v.getFirstOpt(ImageHeightProp)
      }
        yield HtmlWikitext(s"""<img class="_photoTarget" src="$bucketUrl/${filename.raw}" width="$width" height="$height" alt="${filename.raw}" $fromProp/>""")
        
      result.getOrElse(Wikitext(""))
    }    
  }
  
  override lazy val types = Seq(
    PhotoType
  )
}