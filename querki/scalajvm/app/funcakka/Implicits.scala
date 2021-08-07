package funcakka

object Implicits {
  implicit def ActorRefToOps[T : ActorRefLike](t: T) = new ActorRefLike.Ops(t)
}
