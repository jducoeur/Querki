package querki.ql

import models._

import querki.ecology._
import querki.globals._
import querki.types.{ModeledPropertyBundle, ModelTypeDefiner, SimplePropertyBundle}
import querki.values.{QLContext, QValue}

object SignatureMOIDs extends EcotIds(61) {
  val SignatureModelOID = moid(1)
  val SignatureTypeOID = moid(2)
  val ReqParamsOID = moid(3)
  val OptParamsOID = moid(4)
  val ParamModelOID = moid(5)
  val ParamTypeOID = moid(6)
  val SignaturePropOID = moid(7)
  val SignatureDisplayOID = moid(8)
  val ParamsDisplayOID = moid(9)
}

private [ql] trait ParamResult {
  def process(parser:QLParser, context:QLContext):Future[QLContext]
}

/**
 * What a signature looks like to InvocationImpl. Hides the grungy details.
 */
private [ql] trait SignatureMap {
  /**
   * Used by InvocationImpl to fetch the actual value for the named formal parameter.
   */
  def getParam(nameIn:String):ParamResult
}

private [ql] trait SignatureInternal extends EcologyInterface {
  def getSignature(t:Thing, state:SpaceState, paramsOpt:Option[Seq[QLParam]]):SignatureMap
}

/**
 * Ecot to hold all the properties and types for Signature definitions.
 * 
 * This is split out into its own Ecot simply because there's a bunch of inter-related stuff
 * here, and no good reason to clog up the general QLEcot with it.
 * 
 * @author jducoeur
 */
class SignatureEcot(e:Ecology) extends QuerkiEcot(e) with Signature with SignatureInternal with ModelTypeDefiner {
  import SignatureMOIDs._
  
  val Conventions = initRequires[querki.conventions.Conventions]
  val Basic = initRequires[querki.basic.Basic]
  val Types = initRequires[querki.types.Types]
  
  def apply(reqs:Seq[(String, PType[_], String)], opts:Seq[(String, PType[_], QValue, String)]):(OID, QValue) = {
    SignatureProp(
      SimplePropertyBundle(
        ReqParams(
          reqs.map { param =>
            val (name, tpe, summary) = param
            SimplePropertyBundle(
              Core.NameProp(name),
              Core.TypeProp(tpe),
              Conventions.PropSummary(summary)
            )
          }:_*
        ),
        OptParams(
          opts.map { param =>
            val (name, tpe, default, summary) = param
            SimplePropertyBundle(
              Core.NameProp(name),
              Core.TypeProp(tpe),
              Types.DefaultValueProp(default),
              Conventions.PropSummary(summary)
            )
          }:_*
        )
      )
    )
  }
  
  def getSignature(t:Thing, state:SpaceState, paramsOpt:Option[Seq[QLParam]]):SignatureMap = SignatureMapImpl(t, state, paramsOpt)
  
  case class SignatureMapImpl(t:Thing, state:SpaceState, paramsOpt:Option[Seq[QLParam]]) extends SignatureMap {
    implicit val s = state
    
    case class ParamResultImpl(phraseOpt:Option[QLPhrase], formal:ModeledPropertyBundle) extends ParamResult {
      def name = formal.getProp(Core.NameProp).first
      
      def default = {
        formal.getPropOpt(Types.DefaultValueProp).map(_.v) match {
          case Some(defa) => defa
          case None => throw new PublicException("Func.missingNamedParam", t.displayName, name)
        }
      }
      
      def process(parser:QLParser, context:QLContext):Future[QLContext] = {
        phraseOpt match {
          case Some(phrase) => parser.processPhrase(phrase.ops, context)
          case _ => Future.successful(context.next(default))
        }
      }
    }
  
    lazy val sigOpt = t.getPropOpt(SignatureProp)
    // The formal parameters of this function, required then optional
    // TODO: investigate pre-building this, at least for InternalMethods, and storing it as a separate
    // parameter, as an optimization.
    lazy val formalsOpt =
      for {
        sigPV <- sigOpt
        sigBundle <- sigPV.firstOpt
        req <- sigBundle.getPropOpt(ReqParams)
        opt <- sigBundle.getPropOpt(OptParams)
        allParams = req.rawList ++ opt.rawList
      }
        yield allParams
    lazy val indexedOpt = formalsOpt map (_.zipWithIndex)

    def getParam(nameIn:String):ParamResult = {
      val name = nameIn.toLowerCase
      def notFound = throw new PublicException("Func.unknownParamName", name, t.displayName)
      indexedOpt match {
        case Some(indexed) => {
          val formalOpt = indexed.find { pair =>
            val (bundle, i) = pair
            bundle.getPropOpt(Core.NameProp).flatMap(_.firstOpt).map(_ == name).getOrElse(false)
          }
          val (formal, index) = formalOpt.getOrElse(notFound)
          lazy val returnDefault = ParamResultImpl(None, formal)
          
          paramsOpt match {
            case Some(params) => {
              // First see if there is a named actual parameter...
              params.find(_.name.map(_.toLowerCase == name).getOrElse(false)) match {
                case Some(named) => ParamResultImpl(Some(named.phrase), formal)
                case _ => {
                  // ... otherwise, find it positionally...
                  if (params.length >= (index + 1))
                    ParamResultImpl(Some(params(index).phrase), formal)
                  // ... and if that fails, use the default, assuming there is one:
                  else
                    returnDefault
                }
              }
            }
            // There are no actual parameters, so let's see if we can at least find a default:
            case _ => returnDefault
          }
        }
        // We're looking for a named Parameter, but we don't have any formal params!
        case None => notFound
      }
    }
  }
  
  /***********************************************
   * MODELS and THINGS
   ***********************************************/
  
  /**
   * A Function Parameter consists of:
   * -- Name
   * -- PType
   * -- DefaultValue (if it's optional)
   * -- Summary
   * We might add Collection later, but I'm actually not sure that's even a good idea.
   */
  lazy val ParameterModel = ThingState(ParamModelOID, systemOID, RootOID, 
    toProps(
      setName("_Function Parameter Model"),
      setInternal,
      Core.TypeProp(Core.UrType),
      Types.DefaultValueProp(),
      Conventions.PropSummary()))
      
  lazy val SignatureModel = ThingState(SignatureModelOID, systemOID, RootOID,
    toProps(
      setName("_Function Signature Model"),
      setInternal,
      ReqParams(),
      OptParams()))
      
  lazy val SignatureDisplay = ThingState(SignatureDisplayOID, systemOID, RootOID,
    toProps(
      setName("_Display Function Signature"),
      Basic.ApplyMethod(
          """""[[Name]][[_Function Signature -> ""([[_concat(_Required Parameters, _Optional Parameters) -> Name -> _join("", "")]])""]]""""")))
          
  lazy val ParamsDisplay = ThingState(ParamsDisplayOID, systemOID, RootOID,
    toProps(
      setName("_Display Function Parameters"),
      Basic.ApplyMethod(
          """""[[_Function Signature -> _concat(_Required Parameters, _Optional Parameters) ->
            |    "": [[Name]][[_if(_not(_isEmpty(Default Value)), "" (optional)"")]] : [[Summary]]""]]""""".stripMargin)))
      
  override lazy val things = Seq(
    ParameterModel,
    SignatureModel,
    SignatureDisplay,
    ParamsDisplay
  )
  
  /***********************************************
   * TYPES
   ***********************************************/

  lazy val ParameterType = new ModelType(ParamTypeOID, ParamModelOID, 
    toProps(
      setName("_Function Parameter Type"),
      setInternal))
  
  lazy val SignatureType = new ModelType(SignatureTypeOID, SignatureModelOID,
    toProps(
      setName("_Function Signature Type"),
      setInternal))
  
  override lazy val types = Seq(
    ParameterType,
    SignatureType
  )
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val OptParams = new SystemProperty(OptParamsOID, ParameterType, QList,
    toProps(
      setName("_Optional Parameters"),
      setInternal))
  
  lazy val ReqParams = new SystemProperty(ReqParamsOID, ParameterType, QList,
    toProps(
      setName("_Required Parameters"),
      setInternal))
  
  lazy val SignatureProp = new SystemProperty(SignaturePropOID, SignatureType, ExactlyOne,
    toProps(
      setName("_Function Signature"),
      // Currently internal, but this Property is likely to eventually be visible in the UI,
      // although maybe only through a custom Gadget.
      setInternal))
  
  override lazy val props = Seq(
    OptParams,
    ReqParams,
    SignatureProp
  )
}
