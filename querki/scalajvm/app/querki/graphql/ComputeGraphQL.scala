package querki.graphql

import models.{Thing, OID}
import play.api.libs.json.{JsObject, JsValue, JsArray}
import querki.globals.{SpaceState, Ecology}
import querki.util.PublicException
import sangria.ast._
import sangria.parser.QueryParser

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Success, Failure}

/**
  * This object is basically a wrapper around the processQuery() function.
  *
  * TODO: rewrite this stack in IO terms -- it would be much cleaner and more efficient than raw Futures. And we
  * should probably use Cats Validated: trying to do this with pre-2.12 Either is just an exercise in annoyance.
  */
class ComputeGraphQL(implicit state: SpaceState, ec: ExecutionContext, e: Ecology) {

  final val thingQueryName = "_thing"

  type Res = Future[Either[PublicException, JsObject]]

  /**
    * Main entry point -- takes a stringified GraphQL query and returns a response.
    *
    * This returns a Future so that we can potentially do more serious processing in here, when we get
    * more sophisticated.
    */
  def processQuery(query: String): Res = {
    QueryParser.parse(query) match {
      case Success(document) => processQuery(document)
      case Failure(ex) => ???  // TODO: transform to a Left[PublicException]
    }
  }

  /**
    * We've parsed the query, now actually process it.
    */
  def processQuery(document: Document): Res = {
    // TODO: deal with an empty definitions list
    // TODO: deal with multiple definitions
    processDefinition(document.definitions.head)
  }

  def processDefinition(definition: Definition): Res = {
    definition match {
      case od: OperationDefinition => processOperation(od)
      case _ => ???  // TODO: handle others, and error on the ones we don't want
    }
  }

  def coalesceResults(futs: Vector[Res]): Res = {
    Future.sequence(futs).map { eithers =>
      eithers.find(_.isLeft).getOrElse {
        // Okay, they all succeeded
        val objects: Vector[JsObject] = eithers.map(_.right.get)
        val oneObject: JsObject = objects.reduce(_ ++ _)
        Right(oneObject)
      }
    }
  }

  /**
    * A single Query or Mutation operation.
    *
    * This is the real guts of the functionality.
    */
  def processOperation(operation: OperationDefinition): Res = {
    if (operation.operationType != OperationType.Query) {
      // TODO: error for now
      // TODO: deal with mutations
      ???
    } else {
      val result = coalesceResults(operation.selections.map(processTopSelection))
      // Wrap the results in a top-level "data" object:
      result.map(_.right.map(jsObject => JsObject(Map("data" -> jsObject))))
    }
  }

  /**
    * A single "selection" -- more or less a distinct lookup.
    */
  def processTopSelection(selection: Selection): Res = {
    selection match {
      case field: Field => processTopFieldSelection(field)
      case _ => ??? // TODO: handle the other possibilities, or error
    }
  }

  def processTopFieldSelection(field: Field): Res = {
    val name = field.name
    if (name == thingQueryName) {
      // We're querying a *specific* thing, which should be specified as an argument:
      field.arguments.find(_.name == "id") match {
        case Some(thingIdArg) => {
          thingIdArg.value match {
            case strValue: StringValue => {
              val oid = OID(strValue.value)
              state.anything(oid) match {
                case Some(thing) => {
                  processThingSelections(thing, field)
                }
                case None => ??? // TODO: unknown thing ID
              }
            }
            case _ => ??? // TODO: deal with other possible argument types
          }
        }
        case None => {
          field.arguments.find(_.name == "name") match {
            case Some(thingNameArg) => {
              thingNameArg.value match {
                case strValue: StringValue => {
                  state.anythingByName(strValue.value) match {
                    case Some(thing) => {
                      processThingSelections(thing, field)
                    }
                    case None => ??? // TODO: unknown Thing Name
                  }
                }
                case _ => ??? // TODO: deal with other possible argument types
              }
            }
            case None => ??? // TODO: missing required id or name argument
          }
        }
      }
    }
    state.anythingByName(name) match {
      case Some(model) => {
        if (model.isModel) {
          val resultFuts = state.descendants(model, includeModels = false, includeInstances = true).map { thing =>
            processThingSelections(thing, field)
          }.toList
          Future.sequence(resultFuts).map { eithers: List[Either[PublicException, JsObject]] =>
            eithers.find(_.isLeft).getOrElse {
              val jsObjs = eithers.map(_.right.get)
              Right(JsObject(Map(name -> JsArray(jsObjs))))
            }
          }
        } else {
          // TODO: only Models are allowed as top-level names
          ???
        }
      }
      case None => ??? // TODO: error
    }
  }

  def processThingSelections(thing: Thing, field: Field): Res = {
    if (field.selections.isEmpty) {
      ??? // Error -- we require Things to have fields specified
    } else {
      coalesceResults(field.selections.map(processThingSelection(thing, _)))
    }
  }

  def processThingSelection(thing: Thing, selection: Selection): Res = {
    selection match {
      case field: Field => ???
      case _ => ??? // TODO: error, or deal with it
    }
  }
}
