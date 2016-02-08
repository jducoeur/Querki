package querki.search

import scala.concurrent.Future

import scalatags.Text.all._

import models._

import querki.ecology._

import querki.identity.User

import querki.values.{RequestContext, SpaceState}

object MOIDs extends EcotIds(19) {
  val SearchInputOID = moid(1)
}

class SearchEcot(e:Ecology) extends QuerkiEcot(e) with Search with querki.core.MethodDefs {
  
  import MOIDs._
  
  val HtmlUI = initRequires[querki.html.HtmlUI]
  val QL = initRequires[querki.ql.QL]
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val Tags = interface[querki.tags.Tags]
  
  lazy val ParsedTextType = QL.ParsedTextType
  
  override def postInit() = {
    // Search does not require login:
    ApiRegistry.registerApiImplFor[SearchFunctions, SearchFunctionsImpl](SpaceOps.spaceRegion, false)
  }
  
  lazy val SearchInput = new InternalMethod(SearchInputOID,
    toProps(
      setName("_searchInput"),
      Summary("Displays a Search input box here"),
      Signature(
        expected = None,
        reqs = Seq.empty,
        opts = Seq(
          ("placeholder", ParsedTextType, QL.WikitextValue(Wikitext("Search")), "The prompt to show lightly inside the box")
        ),
        returns = (HtmlUI.RawHtmlType, "A Search input box")
      ),
      Details("""Most of the time, you can simply use the Search input in Querki's menu bar.
        |But when you are on a small screen (such as a smartphone), that can be hidden behind a
        |menu, making it less accessible. Spaces that are strongly search-centric may find that
        |inconvenient, so they can make sure that a Search box exists where they need it by
        |using this function.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        place <- inv.processAs("placeholder", ParsedTextType)
      }
        yield HtmlUI.HtmlValue(
          // Hmm. This is *totally* redundant with the declaration in SearchResultsPage on the Client.
          // How can we de-duplicate these?
          input(
            // TODO: in principle, this should be a form-control to make it look right. In practice,
            // making that work inline is a pain in the tuchus. Not sure what the right answer here is.
            cls:="_searchInput search-query _userSearchInput", 
            tpe:="text",
            placeholder:=place.strip.toString))
    }
  }
  
  override lazy val props = Seq(
    SearchInput
  )
  
  lazy val DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp
  
  def search(searchStr:String)(implicit state:SpaceState):Option[SearchResultsInternal] = {
    if (searchStr.length() < 3)
      None
    else {
      val searchComp = searchStr.toLowerCase()
      
      def matchLocs(s:String):List[Int] = {
        def matchLocsRec(start:Int):List[Int] = {
          val i = s.indexOf(searchComp, start)
          if (i == -1)
            Nil
          else
            i :: matchLocsRec(i + 1)
        }
        
        matchLocsRec(0)
      }
      
      def checkOne(t:Thing):Seq[SearchResultInternal] = {
        if (t.unsafeDisplayName.toLowerCase().contains(searchComp)) {
          Seq(SearchResultInternal(t, DisplayNameProp, 1.0, t.unsafeDisplayName, List(t.unsafeDisplayName.toLowerCase().indexOf(searchComp))))
        } else {
          // For now, we're only going to deal with Text types.
          // TODO: cope with Links, Tags, and things like that!
          // TODO: this currently only takes the first elem from the QValue; it should work on
          // all of them!
          val tResults:Iterable[Option[SearchResultInternal]] = t.props.map { pair =>
            val (propId, propValue) = pair
            propValue.pType match {
              case textType:querki.core.TextTypeBasis#TextTypeBase => {
                val firstVal = propValue.firstAs(textType)
                firstVal.flatMap { qtext => 
                  val propComp = qtext.text.toLowerCase()
                  if (propComp.contains(searchComp))
                    Some(SearchResultInternal(t, state.prop(propId).get, 0.5, qtext.text, matchLocs(propComp)))
                  else
                    None
                }
              }
              case _ => None
            }
          }
          tResults.flatten.toSeq
        }
      }
      
      val thingResults = state.everythingLocal.map(checkOne(_)).flatten.toSeq
      val tagResults:Seq[SearchResultInternal] =
        Tags
          .fetchAllTags(state)
          .filter(_.toLowerCase.contains(searchComp))
          .map(Tags.getTag(_, state))
          .map(tag => SearchResultInternal(tag, DisplayNameProp, 0.7, tag.displayName, matchLocs(tag.displayName.toLowerCase)))
          .toSeq
    
      Some(SearchResultsInternal(searchStr, thingResults ++ tagResults))      
    }
  }
}
