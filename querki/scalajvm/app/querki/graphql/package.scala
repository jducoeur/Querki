package querki

import cats.effect.IO
import cats.data.{EitherT, NonEmptyChain, ValidatedNec}

package object graphql {
  type SyncRes[T] = ValidatedNec[GraphQLError, T]
  type Res[T] = EitherT[IO, NonEmptyChain[GraphQLError], T]
}
