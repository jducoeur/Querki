package querki.test.functional

/**
 * Security-centric tests
 * 
 * @author jducoeur
 */
trait Security { this:FuncMixin =>
  object ExploreRestrictedSpace extends TSpace(
    "Explore Restricted Space"
  )
  
  object runSecurityTests extends TestDef(Some(Admin1), IndexPage, "Run Security Tests")({ state => state }) {
    override def subTests = Seq(
        
      // The test of Who Can Explore comes in two parts. First, we have to build the Space:
      TestDef(Some(Admin1), IndexPage, "Setup Explore Test") { state =>
        run(state,
          createSpace(ExploreRestrictedSpace),
          advancedEditThing(
            addExistingProperty(WhoCanExplorePerm),
            WhoCanExplorePerm.setValue(MembersRole)
          )
        )
      },
      
      // Then we log out, come back as Anonymous, and try to look at the now-hidden stuff:
      TestDef(None, LoginPage, "Check hidden items with Explore turned off") { state =>
        
        def checkMissing(item:MenuItem) = {
          find(id(item.id)) should equal (None)
        }
        
        run(state,
          goTo(ExploreRestrictedSpace),
          { state =>
            
            openMenuFor(RefreshItem)
            checkMissing(DesignModelItem)
            checkMissing(CreateThingItem)
            checkMissing(AdvancedEditItem)
          
            state
          }
        )
      }
    )
  }
}
