package querki.test

import scala.scalajs.js

import querki.globals._

import querki.comm.ApiComm

class ApiCommStub(e:Ecology) extends ClientEcot(e) with ApiComm {
  def implements = Set(classOf[ApiComm])
  
  /**
   * Tests should plug their own entry points into the fields of controllers.
   * 
   * IMPORTANT: all "controllers" used in the tests must be listed here! By and large,
   * all the controllers listed in client.scala.html should be also listed here.
   */
  lazy val controllers = lit(
    Application = lit(),
    ExploreController = lit(),
    AdminController = lit(),
    ClientController = lit(),
    TOSController = lit()
  )
  
//  case class StubTree(v:Option[js.Dynamic], children:Map[String, StubTree]) {
//    def addNode(path:Seq[String], vAdd:js.Dynamic):StubTree = {
//      path match {
//        case Seq(name) => {
//          if (children.contains(name))
//            throw new Exception(s"Trying to add duplicate node $name to the StubTree!")
//          copy(children = children + (name -> StubTree(Some(vAdd), Map.empty)))
//        }
//        case Seq(name, names@_*) => {
//          children.get(name) match {
//            case Some(child) => copy(children = children + (name -> child.addNode(path.drop(1), vAdd)))
//            case None => copy(children = children + (name -> StubTree(None, Map.empty).addNode(path.drop(1), vAdd)))
//          }
//        }
//      }
//    }
//    
//    def litTree:js.Dynamic = v match {
//      case Some(dyn) => dyn
//      case None => lit() //lit(children.toSeq:_*)
//    }
//  }
//  
//  var _stubTree = StubTree(None, Map.empty)
//  
//  def addNode(path:Seq[String], v:js.Dynamic) = { _stubTree = _stubTree.addNode(path, v) }
//  
//  lazy val controllers = _stubTree.litTree
}
