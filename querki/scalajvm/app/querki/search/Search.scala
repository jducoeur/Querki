package querki.search

import scala.collection.immutable.SortedSet
import scala.concurrent.Future

import scalatags.Text.all.{min => attrMin, _}

import models._

import querki.ecology._
import querki.globals._
import querki.identity.User
import querki.tags.IsTag
import querki.types.{ModeledPropertyBundle, ModelTypeDefiner, SimplePropertyBundle}
import querki.values.{ElemValue, RequestContext, SpaceState}

object MOIDs extends EcotIds(19) {
  val SearchInputOID = moid(1)
  val SearchInlineOID = moid(2)
  
  val SearchResultThingOID = moid(3)
  val SearchResultPropOID = moid(4)
  val SearchResultPositionOID = moid(5)
  val SearchResultScoreOID = moid(6)
  val SearchResultModelOID = moid(7)
  val SearchResultTypeOID = moid(8)
  val SearchResultTagOID = moid(9)
  val SearchResultIsTagOID = moid(10)
  val SearchResultElementModelOID = moid(11)
  val SearchResultElementsOID = moid(12)
  val SearchResultElementTypeOID = moid(13)
}

class SearchEcot(e:Ecology) extends QuerkiEcot(e) with Search with querki.core.MethodDefs with ModelTypeDefiner {
  
  import MOIDs._
  
  val HtmlUI = initRequires[querki.html.HtmlUI]
  val QL = initRequires[querki.ql.QL]
  val Tags = initRequires[querki.tags.Tags]
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  lazy val ParsedTextType = QL.ParsedTextType
  lazy val SystemOnly = Basic.SystemOnlyProp(true)
  
  override def postInit() = {
    // Search does not require login:
    ApiRegistry.registerApiImplFor[SearchFunctions, SearchFunctionsImpl](SpaceOps.spaceRegion, false)
  }
  
  /*******************************************
   * The Search Result Type
   */
  
  lazy val SearchResultThing = new SystemProperty(SearchResultThingOID, LinkType, Optional,
    toProps(
      setName("_searchResultThing"),
      SystemOnly,
      Summary("The Thing that this Result points to, if any")))
  
  lazy val SearchResultTag = new SystemProperty(SearchResultTagOID, Tags.NewTagSetType, Optional,
    toProps(
      setName("_searchResultTag"),
      SystemOnly,
      Summary("The Tag that this Result points to, if any")))
  
  lazy val SearchResultIsTag = new SystemProperty(SearchResultIsTagOID, YesNoType, ExactlyOne,
    toProps(
      setName("_searchResultIsTag"),
      SystemOnly,
      Summary("True if this result is a Tag; False if it is a Thing")))
  
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
  
  lazy val SearchResultElements = new SystemProperty(SearchResultElementsOID, SearchResultElementType, QList,
    toProps(
      setName("_searchResultElements"),
      SystemOnly,
      Summary("The actual Properties that matched the search query, the quality of each match, and the positions of the matches")))
  
  lazy val SearchResultModel = ThingState(SearchResultModelOID, systemOID, RootOID,
    toProps(
      setName("_searchResultModel"),
      SystemOnly,
      Summary("This is the Model that gets produced when you call `_search()`"),
      SearchResultThing(),
      SearchResultTag(),
      SearchResultIsTag(),
      SearchResultScore(),
      SearchResultElements()))
      
  lazy val SearchResultElementModel = ThingState(SearchResultElementModelOID, systemOID, RootOID,
    toProps(
      setName("_searchResultElementModel"),
      SystemOnly,
      Summary("One Element of a search result"),
      SearchResultProp(),
      SearchResultScore(),
      SearchResultPositions()))
      
  lazy val SearchResultType = new ModelType(SearchResultTypeOID, SearchResultModelOID,
    toProps(
      setName("_searchResultType"),
      SystemOnly,
      Summary("This is the Type that gets produced when you call `_search()`")))
  
  lazy val SearchResultElementType = new ModelType(SearchResultElementTypeOID, SearchResultElementModelOID,
    toProps(
      setName("_searchResultElementType"),
      SystemOnly,
      Summary("This is the Type of a single Element within a search result.")))
      
  override lazy val things = Seq(
    SearchResultModel,
    SearchResultElementModel
  )
  
  override lazy val types = Seq(
    SearchResultType,
    SearchResultElementType
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
            ("models", LinkType, Core.emptyListOf(LinkType), "If provided, the search will be restricted to Instances and Tags of these Models"),
            ("properties", LinkType, Core.emptyListOf(LinkType), "If provided, only these Properties will be searched. Remember to use _self on each of these!"),
            ("searchTags", YesNoType, ExactlyOne(YesNoType(true)), "(default true) Tag names will be searched only if this is true"),
            ("searchThings", YesNoType, ExactlyOne(YesNoType(true)), "(default true) Things (non-Tags) will be searched only if this is true")
          ),
          returns = (SearchResultType, "A List of search results.")
        ),
        Details("""Querki has built-in Search capabilities -- users can Search from the box in the menu bar, and the
          |results are shown on the standard Search Results page. But sometimes you want more control over your searches:
          |you want to show the results directly on a page, formatted the way you like. Or you want to control precisely what
          |gets searched. The `_search()` function gives you that power.
          |
          |*Note:* _search will never return the Thing that it is defined on. That's necessary in order to prevent
          |infinitely-recursive results.""".stripMargin)))
  {
    def createElements(result:SearchResultInternal):QValue = {
      val bundles = result.elements.map { elem =>
        SearchResultElementType(SimplePropertyBundle(
          SearchResultProp(elem.prop.id),
          SearchResultScore(elem.score),
          SearchResultPositions(elem.positions:_*)
        ))
      }
      QList.makePropValue(bundles, SearchResultElementType)
    }
    
    override def qlApply(inv:Invocation):QFut = {
      for {
        query <- inv.processAs("query", ParsedTextType)
        searchTags <- inv.processAs("searchTags", YesNoType)
        searchThings <- inv.processAs("searchThings", YesNoType)
        modelIds <- inv.processAs("models", LinkType).all
        propertyIds <- inv.processAs("properties", LinkType).all
        fullResult <- inv.opt(search(query.plaintext, searchTags, searchThings, modelIds.toSeq, propertyIds.toSeq)(inv.state))
        result <- inv.iter(fullResult.results)
        // IMPORTANT: we mustn't return the Thing we're fetching this from, or the world will go recursive
        // when we try to render!!!
        if (inv.lexicalThing.isEmpty || inv.lexicalThing.get != result.thing)
      }
        yield {
          result.thing match {
            case tag:IsTag => 
              ExactlyOne(SearchResultType(SimplePropertyBundle(
                SearchResultTag(result.thing.displayName),
                SearchResultIsTag(true),
                SearchResultScore(result.score),
                SearchResultElements(createElements(result))
              )))              
            case _ => 
              ExactlyOne(SearchResultType(SimplePropertyBundle(
                SearchResultThing(result.thing),
                SearchResultIsTag(false),
                SearchResultScore(result.score),
                SearchResultElements(createElements(result))
              )))
          }
        }
    }
  }
  
  override lazy val props = Seq(
    SearchInput,
    SearchInline,
    
    SearchResultThing,
    SearchResultTag,
    SearchResultIsTag,
    SearchResultProp,
    SearchResultPositions,
    SearchResultScore,
    SearchResultElements
  )
  
  /*************************************************
   * Search Internals
   */
  
  lazy val DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp
  
  def search(
    searchStr:String, 
    searchTags:Boolean = true, 
    searchThings:Boolean = true,
    modelIds:Seq[OID] = Seq.empty,
    propertyIds:Seq[OID] = Seq.empty)(implicit state:SpaceState):Option[SearchResultsInternal] = 
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
      
      def checkOne(t:Thing):Option[SearchResultInternal] = {
        
        def collectElements:List[SearchResultElementInternal] = {
          val nameElement:List[SearchResultElementInternal] = if ((propertyIds.isEmpty || propertyIds.contains(querki.basic.MOIDs.DisplayNameOID)) && t.unsafeDisplayName.toLowerCase().contains(searchComp)) {
            List(SearchResultElementInternal(DisplayNameProp, t.unsafeDisplayName, 0.5, List(t.unsafeDisplayName.toLowerCase().indexOf(searchComp))))
          } else {
            List.empty
          } 
          
          val restElements:List[SearchResultElementInternal] = {
            // If the request specified some particular Properties, only use those; otherwise, check all
            // Properties:
            val props =
              if (propertyIds.isEmpty)
                t.props.toSeq
              else {
                val propOpts = for {
                  propertyId <- propertyIds
                }
                  yield t.props.get(propertyId).map((propertyId, _))
                  
                propOpts.flatten
              }
                
            val tResults:List[Option[SearchResultElementInternal]] = props.toList.map { pair =>
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
                      Some(SearchResultElementInternal(state.prop(propId).get, qtext.text, 0.25, matchLocs(propComp)))
                    else
                      None
                  }
                }
                case _ => None
              }
            }
            tResults.flatten.toList
          }
          
          nameElement ++ restElements
        }
        
        val elements = collectElements
        if (elements.isEmpty)
          None
        else {
          Some(SearchResultInternal(t, Math.min(elements.map(_.score).reduce(_ + _), 1.0), elements))
        }
      }
      
      val thingResults =
        if (searchThings) {
          val things =
            if (modelIds.isEmpty)
              // Search in the entire Space:
              state.everythingLocal
            else {
              // Some Models have been specified, so gather the descendants of those Models:
              (Set.empty[Thing] /: modelIds) { (set, modelId) =>
                set ++ state.descendants(modelId, false, true, false)
              }
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
              if (modelIds.isEmpty)
                true
              else
                // If a particular model has been specified, then only return Tags for that Model:
                modelIds.contains(Tags.preferredModelForTag(state, tag).id)
            }
            .map(Tags.getTag(_, state))
            .map(tag => SearchResultInternal(tag, 0.7, List(SearchResultElementInternal(DisplayNameProp, tag.displayName, 0.7, matchLocs(tag.displayName.toLowerCase)))))
            .toSeq
        else
          Seq.empty
    
      Some(SearchResultsInternal(searchStr, thingResults ++ tagResults))      
    }
  }
}
