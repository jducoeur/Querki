package modules.person

import models._
import models.Space.oidMap
import models.Thing._
import models.system._
import models.system.OIDs._

// TODO: this Module should formally depend on the Email Module. Probably --
// it isn't entirely clear whether statically described Properties really
// require initialization-order dependencies. But I believe that the Person
// object shouldn't be constructed until after the Email Module has been.
class PersonModule(val moduleId:Short) extends modules.Module {

  object MOIDs {
    val PersonOID = moid(1)
  }
  import MOIDs._
  
  override val things = Seq(Person)

  object Person extends ThingState(PersonOID, systemOID, RootOID,
    toProps(
      setName("Person"),
      IsModelProp(true),
      // TODO: this is a fugly declaration, and possibly unsafe -- do we have any
      // assurance that modules.Modules.Email has been constructed before this?
      (modules.Modules.Email.MOIDs.EmailPropOID -> Optional.None),
      DisplayTextProp("""
This represents a Person who is using Querki or can be invited to it. You can create a Person in
your Space, and compose an email to invite them to use the Space; you can also create a new Model
to add new Properties for any Person in your Space.
""")
    ))
}