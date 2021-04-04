package querki.ql

import querki.values.QValue

case class QLScope(bindings: Map[String, QValue] = Map.empty) {
  def bind(pair: (String, QValue)) = copy(bindings = bindings + pair)
  def lookup(name: String) = bindings.get(name)
}

case class QLScopes(scopes: List[QLScope] = List.empty) {

  // Add the specified binding to the current scope:
  def bind(pair: (String, QValue)) = {
    val newScopes = (scopes.head.bind(pair)) :: scopes.tail
    copy(scopes = newScopes)
  }

  def lookup(name: String): Option[QValue] = {
    (Option.empty[QValue] /: scopes) { (result, scope) =>
      result match {
        case Some(r) => result
        case None    => scope.lookup(name)
      }
    }
  }

  def push = copy(scopes = QLScope() :: scopes)
  def pop = copy(scopes = scopes.tail)
}
