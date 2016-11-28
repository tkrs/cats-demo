package demo.trans

import cats.Monad
import cats.data.Kleisli
import cats.free.{Free, Inject}

object tag {

  sealed trait TagOp[A] {
    def operate[M[_]: Monad]: Kleisli[M, Env, A]
  }

  case class Reverse(data: Option[String]) extends TagOp[Option[String]] {
    override def operate[M[_]: Monad]: Kleisli[M, Env, Option[String]] =
      Kleisli[M, Env, Option[String]] { _ => Monad[M].pure(data.map(_.reverse)) }
  }

  class TagOps[F[_]](implicit I: Inject[TagOp, F]) {
    def reverse(data: Option[String]): Free[F, Option[String]] = Free.inject[TagOp, F](Reverse(data))
  }

  object TagOps {
    implicit def tagOp[F[_]](implicit I: Inject[TagOp, F]): TagOps[F] = new TagOps[F]
  }
}
