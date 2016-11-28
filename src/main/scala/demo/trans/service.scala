package demo.trans

import cats.Monad
import cats.data.Kleisli
import cats.free.{Free, Inject}

object service {

  sealed trait ServiceOp[A] {
    def operate[M[_]: Monad]: Kleisli[M, Env, A]
  }

  case class Put(id: Int, data: String) extends ServiceOp[Unit] {
    override def operate[M[_]: Monad]: Kleisli[M, Env, Unit] =
      Kleisli[M, Env, Unit] { env => env.db += id -> data; Monad[M].pure(()) }
  }

  case class Get(id: Int) extends ServiceOp[Option[String]] {
    override def operate[M[_]: Monad]: Kleisli[M, Env, Option[String]] =
      Kleisli[M, Env, Option[String]] { env => Monad[M].pure(env.db.get(id)) }
  }

  class ServiceOps[F[_]](implicit inject: Inject[ServiceOp, F]) {
    def put(id: Int, data: String): Free[F, Unit] = Free.inject[ServiceOp, F](Put(id, data))
    def get(id: Int): Free[F, Option[String]] = Free.inject[ServiceOp, F](Get(id))
  }

  object ServiceOps {
    implicit def serviceOp[F[_]](implicit I: Inject[ServiceOp, F]): ServiceOps[F] = new ServiceOps[F]
  }
}
