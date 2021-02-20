package querki.basic

import scala.xml.NodeSeq

import models._

import querki.api.commonName
import querki.conventions._
import querki.core._
import querki.ecology._
import querki.globals._
import querki.ql.QLPhrase
import querki.types._
import querki.values.{SpaceState, ElemValue, QLContext, RequestContext, QFut, StateCacheKey, PropAndVal}

class BasicModule(e:Ecology) extends QuerkiEcot(e) with Basic with WithQL with TextTypeBasis with PlainTextBaseType {
  import MOIDs._
  
  val DeriveName = initRequires[querki.types.DeriveName]
  lazy val QL = interface[querki.ql.QL]
  val Types = initRequires[querki.types.Types]
  
  lazy val IsModelProp = Core.IsModelProp
  def NameProp = Core.NameProp

  val BasicTag = querki.core.CoreTag

  /***********************************************
   * API
   ***********************************************/
  
  lazy val nameCacheKey = StateCacheKey(ecotId, "displayNameCache")
  type NameCacheT = scala.collection.concurrent.TrieMap[OID, Future[Wikitext]]
  def nameCache(implicit state:SpaceState) =
    state.fetchOrCreateCache(nameCacheKey, { scala.collection.concurrent.TrieMap.empty[OID, Future[Wikitext]] })
  
  /**
   * The increasingly-complex but terribly-important function that figures out the proper name for a Thing.
   * 
   * This has been pulled out to Basic so that we can use the State's dynamic cache; that belongs in an Ecot.
   * 
   * As always, we don't use the dynamic cache lightly. We use it here because few operations get hit as
   * often as nameOrComputed, and it can be *very* expensive if Computed Name is being used.
   *
   * The function is ghastly, and could be made a bit cleaner with, eg, some OptionT. But this is the hottest of
   * hot paths, so we're going to inline it all.
   *
   * TODO: when we can, rewrite this in terms of IO, which won't be nearly as horrible.
   */
  def nameOrComputedCore(tops:ThingOps)(implicit request:RequestContext, state:SpaceState): Future[Wikitext] = {
    nameCache.getOrElseUpdate(tops.id, {
      def strip(w:Wikitext):Wikitext = {
        Wikitext(w.strip)
      }

      def fromOID: Wikitext = Wikitext(tops.id.toThingId.toString)

      // Step three, invoked below:
      // If the name is "empty", for various definitions of empty, use the Link Name, or failing that the TID:
      def fromLinkNameOrOID(): Future[Wikitext] = {
        val localOpt = tops.localProp(NameProp)
        if (localOpt.isEmpty || localOpt.get.isEmpty) {
          Future.successful(fromOID)
        } else {
          localOpt.get.renderPlain.map { w =>
            if (w.plaintext.length > 0)
              w
            else
              fromOID
          }
        }
      }

      // Step one: try the Display Name
      val dispOpt = tops.localProp(DisplayNameProp)
      val dispPropVal: Option[PropAndVal[PlainText]] = if (dispOpt.isEmpty || dispOpt.get.isEmpty)
        None
      else
        dispOpt

      val dispFutOpt: Future[Option[Wikitext]] = dispPropVal match {
        case Some(pv) => {
          pv.renderPlain.map { w =>
            if (w.plaintext.length > 0)
              Some(w)
            else
              None
          }
        }
        case None => Future.successful(None)
      }

      dispFutOpt.flatMap {
        _.map(Future.successful(_)).getOrElse {
          // Step two: no Display Name, so try Computed Name
          val computed: Option[Future[Wikitext]] = for {
            pv <- tops.getPropOpt(ComputedNameProp)
            v <- pv.firstOpt
          }
            yield QL.process(v, tops.thisAsContext)

          computed.map { fut =>
            // We need to strip any Wikitext out of the computed name, or we wind up with broken links-in-links:
            fut.flatMap { w =>
              val stripped = strip(w)
              if (stripped.plaintext.length > 0) {
                Future.successful(stripped)
              } else {
                // The Computed Name turns out to be empty, so on to Step Three:
                fromLinkNameOrOID()
              }
            }
          }.getOrElse(
            // No Computed name, so fall back to Link Name or OID:
            fromLinkNameOrOID()
          )
        }
      }
    })
  }
  
  /***********************************************
   * TYPES
   ***********************************************/
  
  lazy val PlainTextType = new PlainTextType(PlainTextOID, 
    toProps(
      setName("Plain Text Type"),
      setInternal,
      SkillLevel(SkillLevelAdvanced),
      Categories(BasicTag),
      Summary("A short text that does not contain any QL"),
      Details("""Plain Text is a special, restricted sort of Text. It can contains all of the
          |formatting described in the [QText Reference](http://www.querki.net/u/systemUser/documentation/QText-Reference),
          |but it can *not* contain any QL expressions. This means that it can not have simply Links to other Things, and can
          |not use Properties at all.
          |
          |Therefore, in general this Type is *not* recommended for ordinary use, and is marked as internal. We recommend
          |using Text or Large Text instead.""".stripMargin))) 
  {
    override def editorSpan(prop:Property[_,_]):Int = 6
  }

  /**
   * A QL field is sort of like inside-out QLText. It is processed very similarly,
   * but whereas the "outer" layer of QLText is expected to be QText, with QL in
   * subclauses, the outer layer of a QL field is QL, with wikitext in subclauses.
   * 
   * In other words, it is like QLText, but just the stuff inside the square brackets.
   * 
   * QL fields are also processed a bit differently. QLText is fully processed and
   * rendered, producing QText. QL fields are essentially methods, which get *called*
   * from other methods and from QLText. So the results are not turned directly into
   * QText; instead, the resulting Context is fed back out to the caller.
   * 
   * The public Name for this is now Function, because really, that's what it is. It
   * now is getting powerful enough to be worth the name.
   */
  lazy val QLType = new TextTypeBase(QLTypeOID,
    toProps(
      setName("Function"),
      SkillLevel(SkillLevelAdvanced),
      Categories(BasicTag),
      Summary("A QL Expression, that you can use from other expressions"),
      Details("""Functions are basically how you do serious programming in Querki. They are, therefore,
          |very advanced, and are not recommended for anyone aside from programmers.
          |
          |Technically, a Function is simply an "inside-out" Large Text Property, containing the stuff that
          |would normally go inside square brackets -- a single QL expression of arbitrary complexity. It is
          |not often strictly necessary to pull code out into a Function, but is occasionally the only way to
          |deal with a complex problem such as recursion, and is often helpful for factoring a complex expression.
          |
          |Note that QL and QText can contain each other, to a nearly arbitrary level of recursion. Just as a Text
          |Property can contain QL expressions in double-square-brackets, a Function can contain QText in
          |double-double-quotes.
          |
          |When a Function is invoked from a QL expression, it receives the passed-in context (the value on the
          |left-hand side of the "->"), and that will be received by the first stage of this Function. The received
          |context can also be accessed from anywhere in the Function, as $\_context. If the Function is called with
          |parameters in parentheses, those can be used as $\_1, $\_2, etc.
          |
          |Functions are still pretty new, and it is not clear that all of the kinks have been ironed out yet.
          |If you encounter behaviour that seems to be wrong, please ask about it, and feel free to log an Issue
          |if you find a bug.
          |
          |In the future, Functions will be **greatly** enhanced. Among other things, we plan to add:
          |* Named parameters
          |* The ability to declare a Function Signature, specifying the legal Types for the context and params
          |* The ability to specify the return Type of the Function
          |* Name bindings for values (basically, the immutable version of variables)
          |* Mechanisms for changing the Space programmatically
          |* Type inference, which will attempt to automatically detect the signature and return Type
          |
          |That is all down the road, however -- for now, a Function is simply a raw block of code, with no
          |type checking, so use them with care. If you need specific language features, please ask.""".stripMargin)
    )) with PTypeBuilder[QLText,String] 
  {
    override def editorSpan(prop:Property[_,_]):Int = 12   
  
    override def renderInputXml(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):Future[NodeSeq] =
      fut(renderLargeText(prop, context, currentValue, v, this))

    // TBD: in principle, we really want this to return a *context*, not a *value*. This is a special
    // case of a growing concern: that we could be losing information by returning QValue from
    // qlApply, and should actually be returning a full successor Context.
    override def qlApplyFromProp(inv:Invocation, prop:Property[QLText,_]):Option[QFut] = {
      implicit val s = inv.state
      implicit val rc = inv.context.request
      // We have two very different code paths here. That's odd, but seems correct. If we're getting the function
      // *lexically*, then we want to feed *the entire value* into that function, so that it can handle it as a List.
      // But if we're getting the function from the values themselves, then we need to apply it to them individually.
      val useLexical = inv.lexicalThing.map(_.hasProp(prop)).getOrElse(false)
      
      val qv:QFut = if (useLexical) {
        val lt = inv.lexicalThing.get
        for {
          textPV <- inv.iter(lt.getPropOpt(prop))
          text <- inv.iter(textPV.v.rawList(this))
          result <- inv.fut(QL.processMethod(text, inv.context.forProperty(prop), Some(inv), Some(lt), Some(prop)))          
        }
          yield result
      } else {
        for {
          (bundle, elemContext) <- inv.bundlesAndContextsForProp(prop)
          textPV <- inv.iter(bundle.getPropOpt(prop))
          text <- inv.iter(textPV.v.rawList(this))
          result <- inv.fut(QL.processMethod(text, elemContext.forProperty(prop), Some(inv), Some(bundle), Some(prop)))
        }
          yield result    
      }
      
      Some(qv)
    }
  }
  
  override lazy val types = Seq(
    PlainTextType,
    QLType
  )
  
  def TextValue(msg:String):QValue = ExactlyOne(PlainTextType(msg))
  
  /***********************************************
   * PROPERTIES
   ***********************************************/

  // TODO: is there any reason this needs to go in Core? I think both this and QLType itself can go into Basic:
  lazy val ApplyMethod = new SystemProperty(ApplyMethodOID, QLType, Optional,
    toProps(
      setName("_apply"),
      SkillLevel(SkillLevelAdvanced),
      Categories(BasicTag),
      Summary("A QL Expression that will be run when you name this Thing."),
      Details("""_apply is an advanced function, and most users will not use it directly. But it is probably
          |the most important Property in Querki, and advanced users may want to play with it.
          |
          |One of Querki's design goals was that it should Just Work. This is reflected, more than anywhere else,
          |in the fact that you can just say:
          |[[_code(""[[My Thing]]"")]]
          |and it shows up as a pointer to *My Thing*.
          |
          |That seems obvious, but consider -- you can also say:
          |[[_code(""[[My Property]]"")]]
          |and what you get isn't a pointer to *My Property* -- instead, you get the *value* of My Property on the
          |Thing you're looking at.
          |
          |Moreover, you can say:
          |[[_code(""[[All Things]]"")]]
          |on a page, and what you get is a listing of all of the Things in this Space! So what the heck is going on
          |here?
          |
          |The secret behind the magic is the _apply method. _apply is a Property that is defined on *every* Thing.
          |(More or less -- system-defined Things use a closely-related built-in mechanism.) It defines exactly
          |"What should happen when I name this Thing?" So _apply on Properties displays the value of the Property
          |on the received Thing; _apply on All Things is this QL Expression:
          |[[_code(All Things._apply)]]
          |And _apply for Thing (the Model that everything is based on) simply produces a pointer to this thing.
          |
          |You can define _apply for your own Things as well -- indeed, the way you usually write your own serious
          |Methods is to define a Thing that just has an _apply Property, and then you can use the Method just like
          |the system-defined ones, by name.
          |
          |The QL Expression in the _apply Property will receive whatever is passed in, and should produce whatever
          |you want to pass out. It is currently completely unstructured and untyped. However, note that we will
          |probably be moving towards more structure in the future, and you should always try to be consistent:
          |as with any QL Expression, you should expect to receive a specific Type, and always produce a specific Type.""".stripMargin)))

  /**
   * If set, this is the display name of the specified object. Whereas the primary NameProp
   * has a number of restrictions, the DisplayNameProp does not. It is used to list a Thing
   * by preference when it is set.
   */
  lazy val DisplayNameProp = new SystemProperty(DisplayNameOID, PlainTextType, Optional,
    toProps(
      setName(commonName(_.basic.displayNameProp)),
      NotInherited,
      Types.MinTextLengthProp(1),
      Categories(BasicTag),
      Summary("This Thing's usual Name"),
      Details("""When you create a Thing in Querki, you will usually be asked to provide a Name.
        |This is *not* generally required -- you can generally leave it off if there isn't a sensible
        |name -- but it is usually recommended to give one.
        |
        |The Name can contain pretty much anything, including special characters like quotes, commas and
        |so on.
        |
        |Note that there is also a "Link Name" Property on most Things, which is usually derived
        |automatically from the Name. The Link Name is used in URLs, QL expressions, and places like
        |that, where special characters are not legal. You can usually ignore this distinction, but
        |keep in mind that the Link Name is what will show up in those places, rather than the usual Name.""".stripMargin)
      ))
  
  lazy val ComputedNameProp = new SystemProperty(ComputedNameOID, TextType, ExactlyOne,
    toProps(
      setName("Computed Name"),
      SkillLevel(SkillLevelAdvanced),
      Categories(BasicTag),
      Summary("How to compute a name, for Things that don't have their own names"),
      Details("""Most Things in Querki use the Name Property to set a name for this specific Thing.
          |But that sometimes isn't appropriate, especially for "child" Things that are providing more detail
          |to a "parent". In those cases, you often would like to still be able to display links to the child
          |in a way that is clearer than an Object ID like ".3y286ks".
          |
          |Computed Name exists for this sort of situation. This is a Text Property, which you usually define
          |as a Model Property for the Child Model, to give this Child a pseudo-name based on its properties.
          |For example, to show a name based on the Parent and the Child's "My Date" Property, you would set
          |Computed Name to `\[[Parent -> Name\]]-\[[My Date\]]`.
          |
          |The resulting displayed name is only as unique as you make it. Try to use a combination of Properties
          |that will consistently result in a unique name, to make it clear which Thing is which.
          |
          |This is a pretty advanced Property, and only recommended for relatively complex Spaces.""".stripMargin)))
  
  // TODO: the name DisplayTextProp still need to be renamed to DefaultViewProp:
  lazy val DisplayTextProp = new SystemProperty(DisplayTextOID, LargeTextType, Optional,
    toProps(
      setName(commonName(_.basic.defaultView)),
      Categories(BasicTag),
      Summary("How this Thing will be displayed"),
      Details("""Default View is one of the most important Properties in Querki,
      		|and nearly every Thing has one. The Default View describes how this Thing will usually show up when you
      		|look at it as a web page. It can say almost anything you like, but usually consists of a mix of
      		|text and QL expressions. (Where a "QL Expression" is anything inside double-square-brackets.)
          |
          |If you define the Default View on a Model, and then use that Model for creating a Property, the
          |Default View says how to display values of that Property.""".stripMargin)
      ))
  
  lazy val ModelViewProp = new SystemProperty(ModelViewOID, LargeTextType, Optional,
    toProps(
      setName("Model View"),
      SkillLevel(SkillLevelAdvanced),
      Core.ModelOnlyProp(true),
      Categories(BasicTag),
      Summary("How this Model will be displayed"),
      Details("""One of the most important Properties in Querki is [[Default View._self]], which says how Instances should be
          |displayed. However, you usually do not want to look at a Model the same way you do its Instances: the fields
          |are usually empty, and it simply isn't very useful.
          |
          |So Models use the Model View Property instead. If you set Model View, that says how to display this specific
          |Model. It is not inherited to the Instances.
          |
          |You can usually ignore Model View -- the default serves reasonably well most of the time. But it is available
          |if you would like to do something different.""".stripMargin)))

  lazy val DeprecatedProp = new SystemProperty(DeprecatedOID, YesNoType, ExactlyOne,
    toProps(
      setName("Deprecated"),
      NotInherited,
      SkillLevel(SkillLevelAdvanced),
      Categories(BasicTag),
      Summary("True iff this Thing is Deprecated."),
      Details("""This is a marker flag that you can put on a Thing to say that it is on its way out, and shouldn't
          |be used any more.
          |
          |The exact meaning of Deprecated depends on the situation, but Querki will tend to hide Things marked as
          |Deprecated. If you see somewhere that a Deprecated Thing is visible and shouldn't be, please log a bug
          |report about it.""".stripMargin)))
  
  lazy val KilledThing = new SystemProperty(KilledThingOID, YesNoType, ExactlyOne,
    toProps(
      setName("_Killed Thing"),
      setInternal,
      Summary("The next step after Deprecated -- marks something that is considered formally dead.")))
  
  lazy val ExplicitProp = new SystemProperty(ExplicitPropOID, YesNoType, ExactlyOne,
    toProps(
      setName("_explicitlyShown"),
      setInternal,
      Summary("The inverse of InternalProp -- says that this *is* specifically to be shown to users, even though it otherwise wouldn't be.")))
  
  lazy val SystemOnlyProp = new SystemProperty(SystemOnlyPropOID, YesNoType, ExactlyOne,
    toProps(
      setName("System Only Property"),
      (SystemOnlyPropOID -> ExactlyOne(YesNoType(true))),
      AppliesToKindProp(Kind.Property),
      Summary("A sort of weak version of InternalProp -- this is a Property that users can not add to Things, but you can read and use it.")))
  
  lazy val SystemHiddenProp = new SystemProperty(SystemHiddenPropOID, YesNoType, ExactlyOne,
    toProps(
      setName("System Hidden Property"),
      (SystemOnlyPropOID -> ExactlyOne(YesNoType(true))),
      AppliesToKindProp(Kind.Property),
      Summary("An extreme version of InternalProp -- this is a Property that is not even visible in user space.")))
  
  lazy val PrintViewProp = new SystemProperty(PrintViewOID, LargeTextType, Optional,
    toProps(
      setName(commonName(_.basic.printView)),
      Categories(BasicTag),
      Summary("How this Thing will be printed"),
      Details("""Most of the time, you can just print Querki pages, and they will work as you want. But in some
          |cases, you may want to print a Thing differently from how you look at it on the page -- you may want
          |to show different fields, summarize differently, and so on. When that is the case, add the Print View
          |Property. This is another Large Text, and works very much like Default View, but will only be used for
          |printing.
          |
          |**Important:** the Print View will only be used when you select Print... from the Querki Actions menu;
          |it will not be used if you say Print Page or something like that from the browser itself. This is a
          |technical limitation of browsers that is difficult to work around. So if you want to use Print View,
          |print from the Actions menu.
          |
          |**Advanced:** To make printing look *exactly* like you want, you may need to fiddle with CSS. The Print
          |View will be wrapped in the class "\_printView", so you can use .\_printView in CSS to define styles that
          |only happen in the Print View.
          |
          |To hide the page headers and footers, you can do something like this:
          |```
          |@media print {
          |  @page {
          |    margin-top: 0mm;
          |    margin-bottom: 0mm;
          |  }
          |
          |  body {
          |    padding-top: 0.25in;
          |    padding-bottom: 0.25in;
          |  }
          |}
          |```
          |Unfortunately, there is currently no consistent way to control the headers and footers the way you
          |would like. This is a browser limitation, which hopefully will one day get fixed.""".stripMargin)
      ))

  override lazy val props = Seq(
    ApplyMethod,
    DisplayNameProp,
    ComputedNameProp,
    DisplayTextProp,
    DeprecatedProp,
    ExplicitProp,
    SystemOnlyProp,
    ModelViewProp,
    SystemHiddenProp,
    PrintViewProp,
    KilledThing
  )
  
  /***********************************************
   * THINGS
   ***********************************************/

  lazy val SimpleThing = ThingState(SimpleThingOID, systemOID, RootOID,
    toProps(
      setName(commonName(_.basic.simpleThing)),
      IsModelProp(true),
      DisplayTextProp(Core.QNone),
      (querki.basic.MOIDs.DisplayNameOID -> Core.QNone),
      DeriveName.DeriveNameProp(DeriveName.DeriveAlways)))

  lazy val Page = ThingState(PageOID, systemOID, SimpleThingOID,
    toProps(
      setName("Simple-Page"),
      IsModelProp(true),
      DeprecatedProp(true),
      KilledThing(true)))
  
  def UITag = querki.html.UITag

  object Bulleted extends ThingState(BulletedOID, systemOID, RootOID,
    toProps(
      setName("_bulleted"),
      Categories(UITag),
      Summary("Displays the received values as a bullet list"),
      ApplyMethod("""* ""<ul>[[""<li class="_bullet">
          |____
          |</li>""]]
          |</ul>""""".stripMargin),
      DisplayTextProp("""
          |```
          |LIST -> _bulleted
          |```
          |This method takes a LIST, and render its elements as a bullet list, one per line. It is roughly the same as
          |```
          |LIST -> \""* \____\""
          |```
          |You can nest _bulleted lists within each other, and they will display properly nested.""".stripMargin)))

  object Commas extends ThingState(CommasMethodOID, systemOID, RootOID,
    toProps(
      setName("_commas"),
      Categories(UITag),
      Summary("Displays the received values as a comma-separated list"),
      ApplyMethod("""_join("", "")"""),
      DisplayTextProp("""
          |```
          |LIST -> _commas
          |```
          |This method takes a LIST, and render its elements comma-separated. It is simply syntactic sugar for
          |```
          |LIST -> _join(\"", \"")
          |```
          |""".stripMargin)))

object DisplayThingTree extends ThingState(DisplayThingTreeOID, systemOID, RootOID,
    toProps(
      setName("_displayThingTree"),
      Categories(UITag),
      setInternal,
      Summary("This receives a Thing, displays it, and if it is a Model displays a subtree for its children."),
      ApplyMethod("""""[[_if(_isModel, ""{{_modelInTree:"")]]____[[_if(_isModel, "" }}"")]]""" +
          """[[_if(_and(_isModel, _hasPermission(Who Can Create._self)), _createInstanceLink -> _iconButton(""plus"", ""Create an Instance""))]]
{{indent:[[_children -> 
  _filter(_isModel) ->
  _sort -> 
  _displayThingTree]]
}}
{{indent:[[_showSome(0, 50, ""Show More"", _children -> _filter(_not(_isModel)) -> _sort, _displayThingTree)]]
}}
""""")))

object AllThings extends ThingState(AllThingsOID, systemOID, RootOID,
    toProps(
      setName("Old All Things"),
      DeprecatedProp(true),
      DisplayTextProp("[[Old All Things]]"),
      ApplyMethod("""""{{_thingTree:
[[_currentSpace ->
  _externalRoots ->
  _sort ->
  _displayThingTree]]
}}""""")))
  
object NewAllThings extends ThingState(NewAllThingsOID, systemOID, RootOID,
  toProps(
    setName("All Things"),
    DisplayTextProp("[[All Things]]"),
    ApplyMethod("""_currentSpace -> _externalRoots -> _sort -> _showModelTree""".stripMargin)))
  
object ShowModelTree extends ThingState(ShowModelTreeOID, systemOID, RootOID,
  toProps(
    setName("_showModelTree"),
    ApplyMethod("""_thingTree(""{{_modelInTree:____ [[_if(_and(_isModel, _hasPermission(Who Can Create._self)), _createInstanceLink -> _iconButton(""plus"", ""Create an Instance""))]]}}"", 
                  |opened=_is(Simple Thing), 
                  |id=""node-[[_oid]]"", 
                  |children=""[[
                  |_children -> _filter(_isModel) -> _filter(_not(_Killed Thing)) -> _sort -> _showModelTree]][[
                  |_children -> _filter(_not(_isModel)) -> _sort -> _thingTree]]"")""".stripMargin)))

object AllProps extends ThingState(AllPropsThingOID, systemOID, RootOID,
    toProps(
      setName("All Properties"),
      DisplayTextProp("[[All Properties]]"),
      ApplyMethod("""""{{_thingTree:
[[_currentSpace ->
  _allProps ->
  _bulleted]]
}}
""""")))

  override lazy val things = Seq(
    SimpleThing,
    Page,
    Bulleted,
    Commas,
    DisplayThingTree,
    AllThings,
    AllProps,
    NewAllThings,
    ShowModelTree
  )
}