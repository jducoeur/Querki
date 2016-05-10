package querki.test.functional

/**
 * Security-centric tests
 * 
 * Among other things, this test issues an invitation to TestUser1, and has them join the new ExploreRestrictedSpace.
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
          { state =>
            
            // TODO: this all needs to be refactored, to become a richer harness for doing Security
            // experiments:
            val space = state.getSpace(ExploreRestrictedSpace)
            clickMenuItem(SecurityItem)
            val page = SecurityPage(space)
            waitFor(page)
            val whoCanExplorePath = editorId(space, WhoCanExplorePerm)
            val membersId = MembersRole.tid
            radioButtonGroup(whoCanExplorePath).value = membersId
            eventually { find(id("statusLine")).get.text should be ("Saved") }
            click on "_doneButton"
            // TODO: the state should reflect the changed permissions!
          
            state
          },
          
          shareByEmail(TestUser1)
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
      },
      
      // Now, accept the invitation, and set up Test User 1:
      TestDef(None, RootPage(ExploreRestrictedSpace), "Accept the invitation for Test User 1") { state =>
        run(state,
          { state =>
            
            val inviteLink = extractInviteLink()
            println(s"----> The invitation link is $inviteLink")
          
            state
          }
        )
      }
    )
  }
}
