package models

import querki.globals._
import querki.ql.{Invocation, QLFunction}
import querki.values.{PropAndVal, QLContext, QValue, RequestContext}

class ThingOps(thing: Thing)(implicit e: Ecology) extends PropertyBundleOps(thing) with QLFunction {

  def Core = interface[querki.core.Core]
  def Basic = interface[querki.basic.Basic]
  def QL = interface[querki.ql.QL]
  def Renderer = interface[querki.html.HtmlRenderer]
  def Tags = interface[querki.tags.Tags]

  def ApplyMethod = Basic.ApplyMethod
  def DisplayNameProp = Basic.DisplayNameProp
  def NameProp = Core.NameProp

  def id = thing.id
  def model = thing.model
  def props = thing.props

  def thisAsQValue: QValue = Core.ExactlyOne(Core.LinkType(thing.id))

  def localProp[VT, CT](prop: Property[VT, _]): Option[PropAndVal[VT]] = thing.localProp(prop)
  def getPropOpt[VT](prop: Property[VT, _])(implicit state: SpaceState): Option[PropAndVal[VT]] = thing.getPropOpt(prop)

  def nameOrComputedWiki(
    implicit
    request: RequestContext,
    state: SpaceState
  ): Future[Wikitext] = {
    Basic.nameOrComputedCore(this)
  }

  def nameOrComputed(
    implicit
    request: RequestContext,
    state: SpaceState
  ): Future[DisplayText] = {
    nameOrComputedWiki.map(_.strip)
  }

  def unsafeNameOrComputed(
    implicit
    rc: RequestContext,
    state: SpaceState
  ): Future[String] = {
    nameOrComputedWiki.map(_.plaintext)
  }

  /**
   * True iff the other is an ancestor of this Thing via the Model chain.
   */
  def isAncestor(other: OID)(implicit state: SpaceState): Boolean = {
    (other == model) || thing.getModelOpt.map(_.isAncestor(other)).getOrElse(false)
  }

  /**
   * Convenience method, to check whether a YesNo Property is non-empty, and is true.
   */
  def ifSet(prop: Property[Boolean, _])(implicit state: SpaceState): Boolean = {
    thing.firstOr(prop, false)
  }

  /**
   * Returns true iff this Thing has the IsModel flag set to true on it.
   */
  def isModel(implicit state: SpaceState): Boolean = {
    ifSet(Core.IsModelProp)
  }

  def renderProps(
    implicit
    request: RequestContext,
    state: SpaceState
  ): Future[Wikitext] = {
    Renderer.renderThingDefault(thing)
  }

  /**
   * Show the default rendering for this Thing, if it has no DisplayTextProp defined.
   *
   * This mainly exists so that the different Kinds can override it and do their own thing.
   */
  def renderDefault(
    implicit
    request: RequestContext,
    state: SpaceState
  ): Future[Wikitext] = {
    renderProps
  }

  /**
   * Every Thing can be rendered -- this returns a Wikitext string that will then be
   * displayed in-page.
   *
   * If you specify a property, that property will be rendered with this Thing as a context;
   * otherwise, DisplayText will be rendered.
   */
  def render(
    implicit
    request: RequestContext,
    state: SpaceState,
    prop: Option[Property[_, _]] = None
  ): Future[Wikitext] = {
    val actualProp =
      if (ifSet(Core.IsModelProp))
        prop.getOrElse(Basic.ModelViewProp)
      else
        prop.getOrElse(Basic.DisplayTextProp)

    getPropOpt(actualProp) match {
      // Usual case: there is a Model/Default View to use:
      case Some(pv) if (!pv.isEmpty) => pv.render(thing.thisAsContext.forProperty(pv.prop), Some(thing))
      // Otherwise, if this *was* a Tag, treat it as one:
      case _ if (ifSet(Tags.IsReifiedTagProp)) => {
        val tagView = Tags.getUndefinedTagView(model)
        QL.process(tagView, thing.thisAsContext)
      }
      // Otherwise, fall back to the default:
      case _ => renderDefault
    }
  }

  /**
   * Called when this Thing is encountered with no method invocation in a QL expression.
   * Subclasses are allowed to override it as make sense.
   *
   * This basic version returns a Link to this thing.
   *
   * Callers should generally prefer qlApplyTop instead, since that allows somewhat more
   * powerful operations.
   *
   * Note that this returns a Future of a value, since that is the general case. InternalMethod
   * allows you to provide either a raw value or a Future, depending on which overload you use.
   */
  def qlApply(inv: Invocation): Future[QValue] = {
    val context = inv.context
    val paramsOpt = inv.paramsOpt

    val applyOpt = getPropOpt(ApplyMethod)(context.state)
    applyOpt match {
      case Some(apply) => {
        val qlText = apply.first
        QL.processMethod(qlText, context.forProperty(apply.prop), Some(inv), Some(thing))
      }
      case None => Future.successful(Core.ExactlyOne(Core.LinkType(thing.id)))
    }
  }

  /**
   * The wrapper around qlApply(), which is actually called from the outside. Specific Things may
   * override this if they need to return a QLContext instead of simply a QValue, or want to do
   * something async. Basically, this is the full-control version.
   */
  def qlApplyTop(
    inv: Invocation,
    transformThing: Thing
  ): Future[QLContext] = {
    qlApply(inv).map(inv.context.nextFrom(_, transformThing))
  }
}
