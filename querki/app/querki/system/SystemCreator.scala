package querki.system

import querki.ecology._

/**
 * The master owner of the Official Ecology.
 * 
 * This knows how to construct all the main pieces of the system. Test code *may* use it,
 * but is not required to do so -- it is entirely normal to build a test harness that is
 * composed mainly of stubs.
 */
object SystemCreator {
  def createAllEcots(ecology:Ecology):Ecology = {
    // IMPORTANT: The numbers attached to these Ecots must NEVER BE CHANGED!!!!! They
    // get built into the moid's, and thence into the database! If an Ecot is removed,
    // comment it out, but leave its number and all others alone.
    new modules.stylesheet.StylesheetModule(ecology)               // 1
    new querki.email.impl.EmailModule(ecology)                     // 2
    new querki.identity.PersonModule(ecology)                      // 3
    new querki.security.AccessControlModule(ecology)               // 4
    new querki.time.TimeModule(ecology)                            // 5
    new querki.collections.CollectionsModule(ecology)              // 6
//  new render.RenderingModule(ecology)                            // 7
    new querki.system.TOSModule(ecology)                           // 8
    new querki.logic.LogicModule(ecology)                          // 9
    new querki.types.impl.TypesModule(ecology)                     // 10
    new querki.html.UIModule(ecology)                              // 11
    new querki.types.DeriveNameModule(ecology)                     // 12
    new querki.editing.EditorModule(ecology)                       // 13
    new querki.identity.skilllevel.impl.SkillLevelModule(ecology)  // 14
    new querki.conventions.ConventionsModule(ecology)              // 15
    new querki.core.CoreModule(ecology)                            // 16
    new querki.basic.BasicModule(ecology)                          // 17
    new querki.system.SystemEcot(ecology)                          // 18
    new querki.search.SearchEcot(ecology)                          // 19
    new querki.core.PropListManagerEcot(ecology)                   // 20
    new querki.datamodel.DataModelAccessEcot(ecology)              // 21
    new querki.tags.TagsEcot(ecology)                              // 22
    
    ecology
  }
}