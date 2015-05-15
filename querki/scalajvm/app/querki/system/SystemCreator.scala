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
  /**
   * This creates the Ecots that really should not be used in unit testing, because they work
   * with the database.
   */
  def createDBEcots(ecology:Ecology) = {
    new querki.identity.UserPersistence(ecology)                   // 30    
    new querki.spaces.DBSpacePersistenceFactory(ecology)           // 38
  }
  
  /**
   * This creates the other Ecots that need to be stubbed in unit testing.
   */
  def createStubbableEcots(ecology:Ecology) = {
    new controllers.PublicUrlDefinitions(ecology)                  // 53    
  }
  
  /**
   * This creates the Ecots that can potentially be used in testing.
   * 
   * As of this writing, I haven't gone through this list carefully. Some of these
   * will need to be moved to createDBEcots eventually.
   */
  def createTestableEcots(ecology:Ecology) = {
    // IMPORTANT: The numbers attached to these Ecots must NEVER BE CHANGED!!!!! They
    // get built into the moid's, and thence into the database! If an Ecot is removed,
    // comment it out, but leave its number and all others alone.
    new querki.css.StylesheetModule(ecology)                       // 1
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
    new querki.text.TextEcot(ecology)                              // 23
    new querki.ql.QLEcot(ecology)                                  // 24
    new querki.spaces.PropTypeMigratorEcot(ecology)                // 25
    new querki.html.HtmlRendererEcot(ecology)                      // 26
    new querki.links.LinksEcot(ecology)                            // 27
    new querki.spaces.SpacePersistenceEcot(ecology)                // 28
    new querki.evolutions.EvolutionsEcot(ecology)                  // 29
                                                                   // 30
    new controllers.PageEventManagerEcot(ecology)                  // 31
    new querki.spaces.SpaceChangeManagerEcot(ecology)              // 32    
    new querki.imexport.ImexportEcot(ecology)                      // 33
    new querki.datamodel.introspection.IntrospectionEcot(ecology)  // 34
    new querki.conversations.ConversationEcot(ecology)             // 35
    new querki.collections.GroupingEcot(ecology)                   // 36
    new querki.spaces.SpaceEcot(ecology)                           // 37
                                                                   // 38
    new querki.identity.IdentityEcot(ecology)                      // 39
    new controllers.NavSectionEcot(ecology)                        // 40
    new querki.types.PropPathEcot(ecology)                         // 41
    new querki.security.EncryptionEcot(ecology)                    // 42
    new querki.admin.AdminEcot(ecology)                            // 43
    new querki.uservalues.UserValueEcot(ecology)                   // 44
    new querki.uservalues.RatingEcot(ecology)                      // 45
    new querki.links.ExternalLinkEcot(ecology)                     // 46
    new querki.session.SessionEcot(ecology)                        // 47
    new querki.notifications.NotificationEcot(ecology)             // 48
    new querki.notifications.NotificationPersistenceEcot(ecology)  // 49
    new querki.conversations.CommentNotifierEcot(ecology)          // 50
    new querki.security.RolesEcot(ecology)                         // 51
    new querki.photos.PhotoEcot(ecology)                           // 52
    															   // 53
    new querki.api.ClientApiEcot(ecology)                          // 54
    new querki.tools.ProfilerEcot(ecology)                         // 55
  }
  
  def createAllEcots(ecology:Ecology):Ecology = {
    createTestableEcots(ecology)
    createDBEcots(ecology)
    createStubbableEcots(ecology)
    
    ecology
  }
}