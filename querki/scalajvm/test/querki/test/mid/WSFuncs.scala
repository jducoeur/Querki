package querki.test.mid

import play.api.libs.ws.{WSClient, WSRequest}

class WSFuncs {
  def wsUrl(url: String): TestOp[WSRequest] = TestOp.withState { testState =>
    testState.harness.controller[WSClient].url(url)
  }
}

object WSFuncs extends WSFuncs
