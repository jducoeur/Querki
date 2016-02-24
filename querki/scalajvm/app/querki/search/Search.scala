package querki.search

import scala.concurrent.Future

import scalatags.Text.all._

import models._

import querki.ecology._
import querki.globals._
import querki.identity.User
import querki.types.{ModeledPropertyBundle, ModelTypeDefiner, SimplePropertyBundle}
import querki.values.{RequestContext, SpaceState}

object MOIDs extends EcotIds(19) {
  val SearchInputOID = moid(1)
  val SearchInlineOID = moid(2)
  
  val SearchResultThingOID = moid(3)
  val SearchResultPropOID = moid(4)
  val SearchResultPositionOID = moid(5)
  val SearchResultScoreOID = moid(6)
  val SearchResultModelOID = moid(7)
  val SearchResultTypeOID = moid(8)
}

class SearchEcot(e:Ecology) extends QuerkiEcot(e) with Search with querki.core.MethodDefs with ModelTypeDefiner {
  
  import MOIDs._
  
  val HtmlUI = initRequires[querki.html.HtmlUI]
  val QL = initRequires[querki.ql.QL]
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val Tags = interface[querki.tags.Tags]
  
  lazy val ParsedTextType = QL.ParsedTextType
  lazy val SystemOnly = Basic.SystemOnlyProp(true)
  
  override def postInit() = {
    // Search does not require login:
    ApiRegistry.registerApiImplFor[SearchFunctions, SearchFunctionsImpl](SpaceOps.spaceRegion, false)
  }
  
  /*******************************************
   * The Search Result Type
   */
  
  lazy val SearchResultThing = new SystemProperty(SearchResultThingOID, Basic.PropertyBundleType, ExactlyOne,
    toProps(
      setName("_searchResultThing"),
      SystemOnly,
      Summary("The Thing or Tag that this Result points to")))
  
  lazy val SearchResultProp = new SystemProperty(SearchResultPropOID, LinkType, Optional,
    toProps(
      setName("_searchResultProperty"),
      SystemOnly,
      Summary("The Property that this Result was found in")))
  
  lazy val SearchResultPositions = new SystemProperty(SearchResultPositionOID, IntType, QList,
    toProps(
      setName("_searchResultPositions"),
      SystemOnly,
      Summary("Where in the _searchResultProperty the search query was found")))
  
  lazy val SearchResultScore = new SystemProperty(SearchResultScoreOID, Core.FloatType, ExactlyOne,
    toProps(
      setName("_searchResultScore"),
      SystemOnly,
      Summary("How good a match this Thing was for the search query")))
  
  lazy val SearchResultModel = ThingState(SearchResultModelOID, systemOID, RootOID,
    toProps(
      setName("_searchResultModel"),
      SystemOnly,
      Summary("This is the Model that gets produced when you call `_search()`"),
      SearchResultThing(),
      SearchResultProp(),
      SearchResultPositions(),
      SearchResultScore()))
      
  lazy val SearchResultType = new ModelType(SearchResultTypeOID, SearchResultModelOID,
    toProps(
      setName("_searchResultType"),
      SystemOnly,
      Summary("This is the Type that gets produced when you call `_search()`")))
      
  override lazy val things = Seq(
    SearchResultModel
  )
  
  override lazy val types = Seq(
    SearchResultType
  )
  
  /*******************************************
   * Functions
   */
  
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
  
  lazy val SearchInline = new InternalMethod(SearchInlineOID,
      toProps(
        setName("_search"),
        Summary("Searches for the specified text, and produces the matches"),
        Signature(
          expected = None,
          reqs = Seq(
            ("query", ParsedTextType, "The text to search for, minimum 3 characters")
          ),
          opts = Seq(
            ("models", LinkType, Core.QNone, "If provided, the search will be restricted to Instances and Tags of these Models"),
            ("properties", LinkType, Core.QNone, "If provided, only these Properties will be searched"),
            ("searchTags", YesNoType, ExactlyOne(YesNoType(true)), "(default true) Tag names will be searched only if this is true"),
            ("searchThings", YesNoType, ExactlyOne(YesNoType(true)), "(default true) Things (non-Tags) will be searched only if this is true")
          ),
          returns = (SearchResultType, "A List of search results.")
        ),
        Details("""Querki has built-in Search capabilities -- users can Search from the box in the menu bar, and the
          |results are shown on the standard Search Results page. But sometimes you want more control over your searches:
          |you want to show the results directly on a page, formatted the way you like. Or you want to control precisely what
          |gets searched. The `_search()` function gives you that power.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        query <- inv.processAs("query", ParsedTextType)
        searchTags <- inv.processAs("searchTags", YesNoType)
        searchThings <- inv.processAs("searchThings", YesNoType)
        modelId <- inv.processAsOpt("models", LinkType)
        propertyId <- inv.processAsOpt("properties", LinkType)
        fullResult <- inv.opt(search(query.plaintext, searchTags, searchThings, modelId, propertyId)(inv.state))
        result <- inv.iter(fullResult.results)
        // IMPORTANT: we mustn't return the Thing we're fetching this from, or the world will go recursive
        // when we try to render!!!
        if (inv.lexicalThing.isEmpty || inv.lexicalThing.get != result.thing)
      }
        yield 
          ExactlyOne(SearchResultType(SimplePropertyBundle(
            SearchResultThing(result.thing),
            SearchResultProp(result.prop.id),
            SearchResultPositions(result.positions:_*),
            SearchResultScore(result.score)
          )))
    }
  }
  
  override lazy val props = Seq(
    SearchInput,
    SearchInline,
    
    SearchResultThing,
    SearchResultProp,
    SearchResultPositions,
    SearchResultScore
  )
  
  /*************************************************
   * Search Internals
   */
  
  lazy val DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp
  
  def search(
    searchStr:String, 
    searchTags:Boolean = true, 
    searchThings:Boolean = true,
    modelIdOpt:Option[OID] = None,
    propertyIdOpt:Option[OID] = None)(implicit state:SpaceState):Option[SearchResultsInternal] = 
  {
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
          // If the request specified a particular Property, only use that one; otherwise, check all
          // Properties:
          val props = propertyIdOpt match {
            case Some(propertyId) => t.props.get(propertyId).map((propertyId, _)).toSeq
            case None => t.props.toSeq
          }
          val tResults:Iterable[Option[SearchResultInternal]] = props.map { pair =>
            val (propId, propValue) = pair
            propValue.pType match {
              // For now, we're only going to deal with Text types.
              // TODO: cope with Links, Tags, and things like that!
              case textType:querki.core.TextTypeBasis#TextTypeBase => {
                // TODO: this currently only takes the first elem from the QValue; it should work on
                // all of them!
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
      
      val thingResults =
        if (searchThings) {
          val things = modelIdOpt match {
            case Some(modelId) => state.descendants(modelId, false, true, false)
            case None => state.everythingLocal
          }
          things.map(checkOne(_)).flatten.toSeq
        } else
          Seq.empty
          
      val tagResults:Seq[SearchResultInternal] =
        if (searchTags)
          Tags
            .fetchAllTags(state)
            .filter(_.toLowerCase.contains(searchComp))
            .filter { tag =>
              modelIdOpt match {
                // If a particular model has been specified, then only return Tags for that Model:
                case Some(modelId) => Tags.preferredModelForTag(state, tag).id == modelId
                case None => true
              }
            }
            .map(Tags.getTag(_, state))
            .map(tag => SearchResultInternal(tag, DisplayNameProp, 0.7, tag.displayName, matchLocs(tag.displayName.toLowerCase)))
            .toSeq
        else
          Seq.empty
    
      Some(SearchResultsInternal(searchStr, thingResults ++ tagResults))      
    }
  }
}
