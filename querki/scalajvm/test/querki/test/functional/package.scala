package querki.test

import models.OID
import querki.data.TID

/**
 * Querki's Functional Tests.
 * 
 * To run these tests, say `ftst` from sbt / Activator.
 * 
 * 
 * ==Structure==
 * 
 * This is currently defined as a gigantic cake. That's because (for reasons discussed below)
 * we're currently running as one huge, long, functional test. That's not ideal -- it's
 * not parallelizable, in particular -- but it's easier in many ways. So we're going to
 * work this way for the time being, with an expectation of refactoring later when we have
 * the resources. (Note, though, that we first need to figure out how to completely exclude
 * the functional tests -- not even opening a browser window -- when running unit tests. So
 * far, I don't have a solution.) 
 * 
 * 
 * ==Prerequisites for running these tests==
 * 
 * You must first download the native ChromeDriver from
 * 
 *   [[https://sites.google.com/a/chromium.org/chromedriver/downloads]]
 *   
 * Install that on your path, such as /usr/bin/chromedriver, and make sure it has
 * permissions 755. ("which chromedriver" should find it.) Or point the system
 * property "webdriver.chrome.driver" to wherever that is installed.
 * 
 * Also, make sure you have a reasonably *current* version of Chrome installed.
 * 
 * If ScalaTest cancels these tests, saying it "was unable to create a Selenium
 * ChromeDriver on this platform", it means the underlying ChromeDriver threw an
 * exception. Uncomment the DriverTests below and use those to find out what that
 * exception was in order to debug it. The source code for ChromeDriver can be found at:
 * 
 *   [[https://github.com/SeleniumHQ/selenium/tree/master/java/client/src/org/openqa/selenium/chrome]]
 * 
 *   
 * In order to run these tests headless, make sure that you have xvfb installed, with:
 * {{{
 *   sudo apt-get install xvfb
 * }}}
 * In your shell, say:
 * {{{
 *   Xvfb :1 -screen 5 1280x1024x8 &
 * }}}
 * (Note the capital "X" there.) That creates display 1, screen 5 as a virtual framebuffer.
 * 
 * In the shell say:
 * {{{
 *   export DISPLAY=:1.5
 * }}}
 * That tells the system to use this virtual screen for the output. Then start up
 * sbt and run your test.
 * 
 * 
 * These tests assume that you have empty databases named "test_user" and "test_system", that can be
 * accessed by the same user credentials as the usual system and user DBs.
 * 
 * They also assume that you have a local DB named "test_system_template", which is the empty template
 * version of the system DB. You can load this from test_system_template.sql in Querki's git root.
 * 
 * 
 * ==Writing Tests==
 * 
 * As a rule of thumb, individual functions in the functional test world shouldn't usually be more than
 * 20 lines long. While exceptions are *possible*, they should be looked at with an acid eye.
 * 
 * In particular, always be on the lookout for duplication. Automated tests are still code, and Rule #1
 * is still "Duplicate Data is the Root of all Evil". If you're duplicating more than a line or two
 * across multiple places, that suggests refactoring. The actual code is trying to be very agile; to
 * accomplish that, the tests need to be equally agile, and duplication hinders that.
 * 
 * Note that all top-level tests are run in QuerkiFuncTests, under runTests(). Feel free to add more
 * modules to that. Keep those modules relatively decoupled from each other. You are allowed to add
 * additional tests that depend on CommonSpace, but only if they don't do anything dramatic to it. If
 * you need to do stranger stuff, add another test-specific Space.
 * 
 * `State` is the most important data structure in the harness. More or less every function should be
 * taking the current State, and returning a new State that reflects the new reality. The State is
 * basically the "virtual DOM" for the test harness, keeping track of our understanding of the browser
 * and server. Feel free to enhance it, but be careful to be consistent.
 * 
 * Use the `run` function liberally, to encapsulate the pattern "run these functions, each of which
 * takes a State and returns a State". If functions need additional parameters, they should usually
 * be curried.
 * 
 * 
 * The tests are EcologyMembers, and you *can* access the real Ecology. Please keep in mind that the
 * tests are running in a separate thread from the real Actors, and watch for anything that might
 * cause contention. (Since the Ecots are supposed to be stateless, they *should* be safe to use from
 * the tests. But keep an eye open.)
 * 
 * 
 * ==Notes and Future Plans==
 * 
 * In a perfect world, we should be using One[Server|Browser]PerSuite. Problem is, ScalaTest's exclusion
 * mechanism works on *tests*, not *suites*. So even if we are excluding this suite using its tags, it
 * still starts up both the server and the browser, though we don't want them. So instead, we're structuring
 * this as one huge test. It's suboptimal, but adequate for now.
 * 
 * 
 * @author jducoeur
 */
package object functional {
  implicit def oid2tid(oid:OID):TID = TID(oid.toThingId)
}
