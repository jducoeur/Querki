package querki.api

import querki.globals._

import querki.data.ThingInfo

class PassthroughHandler(contents: Map[String, ThingInfo]) extends PassthroughHandlerBase {

  def pass(name: String): ThingInfo = contents.get(name) match {
    case Some(info) => info
    case None       => throw new Exception(s"Didn't find Standard Thing named $name!")
  }
}
