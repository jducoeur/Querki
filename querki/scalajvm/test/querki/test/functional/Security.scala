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
  
  object TestUser1Space extends TSpace("Test User 1 Personal Space")
  
  object runSecurityTests extends TestDef(Some(Admin1), IndexPage, "Run Security Tests")({ state => state }) {
    override def subTests = Seq(
        
      // The test of Who Can Explore comes in two parts. First, we have to build the Space:
      TestDef(Some(Admin1), IndexPage, "Setup Explore Test") { state =>
        run(state,
          createSpace(ExploreRestrictedSpace),
          { state =>
            // Bump Admin1 into Programmer Mode:
            find(SecurityItem.id) should be (empty)
            clickMenuItem(SkillLevelItem)
            val advancedButtonId = "_advancedComplexity" 
            waitFor(advancedButtonId)
            click on advancedButtonId
            // This will do a full reload, and then we should see the Security menu item:
            eventually { 
              openMenuFor(SecurityItem)
              find(SecurityItem.id) should not be (empty)
              // Close the menu again, for the code below
              click on SecurityItem.menu.id
            }
            
            // TODO: this all needs to be refactored, to become a richer harness for doing Security
            // experiments:
            val space = state.getSpace(ExploreRestrictedSpace)
            clickMenuItem(SecurityItem)
            val page = SecurityPage(space)
            waitFor(page)
            val whoCanExplorePath = editorId(space, WhoCanExplorePerm)
            val membersId = MembersRole.tid
            radioButtonGroup(whoCanExplorePath).value = membersId
            // While the permission change is saving, the radio buttons are disabled. We do this
            // specifically because the "Saved" display in the status line is too transient.
            eventually { find(whoCanExplorePath).get.attribute("disabled") should be (None) }
            click on "_doneButton"
            // TODO: the state should reflect the changed permissions!
            state
          },
          
          shareByEmail(TestUser1),
          shareByEmail(TestUser3)
        )
      },
      
      // Then we log out, come back as Anonymous, and try to look at the now-hidden stuff:
      TestDef(None, LoginPage, "Check hidden items with Explore turned off") { state =>
        
        def checkMissing(item:MenuItem) = {
          find(id(item.id)) should equal (None)
        }
        
        run(state,
          goTo(ExploreRestrictedSpace),
          s {
            openMenuFor(RefreshItem)
            checkMissing(DesignModelItem)
            checkMissing(CreateThingItem)
            checkMissing(AdvancedEditItem)
          }
        )
      },
      
      // Now, accept the invitation, and set up Test User 1:
      TestDef(None, RootPage(ExploreRestrictedSpace), "Accept the invitation for Test User 1") { state =>
        // Make sure that TestUser3 also got invited:
        extractInviteLink(TestUser3.email)
       
        run(state,
           acceptInvitationToJoinQuerki(TestUser1, ExploreRestrictedSpace)
        )
      },
      
      // And check that Test User 1 is a full user -- they can create a Space:
      TestDef(Some(TestUser1), IndexPage, "Check that Test User 1 can create a Space") { state =>
        run(state,
          createSpace(TestUser1Space))
      },
      
      // Now, try signing up Test User 2 from the home screen:
      TestDef(None, LoginPage, "Test User 2 joins Querki") { state =>
        run(state,
          { state =>
            
            // Sign up from the Login Page:
            click on "signup_button"
            waitFor(SignupPage)
            find("signupButton").get.attribute("disabled") should be (Some("true"))
            textField("emailInput").value = TestUser2.email
            pwdField("passwordInput").value = TestUser2.password
            textField("handleInput").value = TestUser2.handle
            textField("displayInput").value = TestUser2.display
            // It appears that in practice this goes to None instead Some("false")?
            eventually { find("signupButton").get.attribute("disabled") should be (None) }
            click on "signupButton"
            acceptTermsOfService()(state)
            waitFor(IndexPage)
            
            // Okay, now let's go to the email we received, and "click" on its link:
            val validateLink = extractValidateLink()
            go to validateLink
            // This now goes through the Validation page, but that takes an unpredictable amount of time,
            // so we're just going to assertion that we wind up back at the Index page, now with a Create
            // button that we can use, indicating that we're ready to roll:
            eventually { find("_createSpaceButton").get.isEnabled should be (true) }
          
            state.copy(currentUserOpt = Some(TestUser2)) -> IndexPage
          }
        )
      }
    )
  }
}
