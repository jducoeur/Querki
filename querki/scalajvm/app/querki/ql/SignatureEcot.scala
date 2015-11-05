package querki.ql

import models._

import querki.ecology._
import querki.globals._
import querki.types.{ModelTypeDefiner, SimplePropertyBundle}
import querki.values.QValue

object SignatureMOIDs extends EcotIds(61) {
  val SignatureModelOID = moid(1)
  val SignatureTypeOID = moid(2)
  val ReqParamsOID = moid(3)
  val OptParamsOID = moid(4)
  val ParamModelOID = moid(5)
  val ParamTypeOID = moid(6)
  val SignaturePropOID = moid(7)
}

private [ql] case class ParamResult(param:Option[QLPhrase], default:Option[QValue])

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
          def returnDefault = {
            formal.getPropOpt(Types.DefaultValueProp).flatMap(_.firstOpt) match {
              case Some(default) => ParamResult(None, Some(default))
              case None => throw new PublicException("Func.missingNamedParam", t.displayName, name)
            }
          }
          
          paramsOpt match {
            case Some(params) => {
              // TODO: first see if there is a named actual parameter
              if (params.length >= (index + 1))
                ParamResult(Some(params(index).phrase), None)
              else
                returnDefault
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
   * MODELS
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
      
  override lazy val things = Seq(
    ParameterModel,
    SignatureModel
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
