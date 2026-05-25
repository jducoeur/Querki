package funcakka

object Implicits {
  implicit def ActorRefToOps[T : ActorRefLike](t: T): ActorRefLike.Ops[T] = new ActorRefLike.Ops(t)
}
