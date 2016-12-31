package demo.trans

import cats.free.Free
import cats.{Monad, ~>}

object request {

  def interpreterK[F[_] : Monad]: RequestOp ~> F =
    new (RequestOp ~> F) {
      override def apply[A](fa: RequestOp[A]): F[A] = fa match {
        case Lift(j, a, m) ⇒ m.transK[F].apply(a).run(j)
        case Reply(s) ⇒ Monad[F].pure(Response(s))
      }
    }

  sealed trait RequestOp[A]

  case class Lift[Op[_], A, J](j: J, action: Free[Op, A], mod: KleisliTrans.Aux[Op, J]) extends RequestOp[A]

  case class Reply[A](s: A) extends RequestOp[Response[A]]

  object RequestOp {
    def pure[A](a: A): Free[RequestOp, A] = Free.pure(a)

    def lift[Op[_], A, J](j: J, action: Free[Op, A])(implicit mod: KleisliTrans.Aux[Op, J]): Free[RequestOp, A] =
      Free.liftF(Lift(j, action, mod))

    def reply[A](s: A): Free[RequestOp, Response[A]] =
      Free.liftF[RequestOp, Response[A]](Reply[A](s))
  }

}
