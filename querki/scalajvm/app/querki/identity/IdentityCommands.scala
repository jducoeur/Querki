package querki.identity

import akka.pattern._

import querki.console.ConsoleFunctions._
import querki.globals._
import querki.spaces.messages._
import querki.util.ActorHelpers

class IdentityCommands(e:Ecology) extends QuerkiEcot(e) {
  import MOIDs._
  
  val Console = initRequires[querki.console.Console]

  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val DataModel = interface[querki.datamodel.DataModelAccess]
  lazy val Person = interface[Person]
  
  lazy val CanReadProp = AccessControl.CanReadProp
  
  lazy val MakeAllPersonsPublicCommand = Console.defineSpaceCommand(
    MakeAllPersonsPublicCommandOID, 
    "Make All Persons Public", 
    "This is a one-off command to fix [this bug](https://www.querki.net/u/jducoeur/alpha-issue-tracking/#!.7w4g99b) in your Space", 
    Seq(AccessControl.CanEditProp))
  { args =>
    
    val inv = args.inv 
    implicit val state = inv.state
    // TODO: this is icky, even though I believe it's always true. Really, all of this is proving
    // that the whole bloody ApiImpl thing should have been done in terms of typeclasses instead
    // of subclasses. *Sigh*.
    val spaceApi = args.api.asInstanceOf[querki.api.SpaceApiImpl]
    implicit val timeout = ActorHelpers.timeout
    
    Person.withCache { cache =>
      val responseFuts = for {
        person <- cache.allPeopleIncludingInvitees
        msg = ChangeProps(
            inv.context.request.requesterOrAnon, 
            state.id, 
            person.id, 
            toProps(CanReadProp(DataModel.getDeletedValue(CanReadProp))))
      }
        yield spaceApi.spaceRouter ? msg
      val responseFut = Future.sequence(responseFuts)
      
      responseFut.map { responses =>
        val errors = responses.collect {
          case ThingError(ex, _) => ex
        }
        errors
          .headOption
          .map(ex => ErrorResult(ex.display(Some(inv.context.request))))
          .getOrElse(DisplayTextResult("All Persons Updated"))
      }
    }
  }

  override lazy val props = Seq(
    MakeAllPersonsPublicCommand
  )
}
