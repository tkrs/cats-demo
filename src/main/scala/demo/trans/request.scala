package demo.trans

import cats.data.Kleisli
import cats.free.Free
import cats.{Monad, ~>}

object request {

  def interpreterK[F[_] : Monad]: RequestOp ~> Kleisli[F, Unit, ?] =
    new (RequestOp ~> Kleisli[F, Unit, ?]) {
      override def apply[A](fa: RequestOp[A]): Kleisli[F, Unit, A] = fa match {
        case Lift(j, a, m) ⇒ Kleisli(_ ⇒ m.transK[F].apply(a).run(j))
        case Reply(s) ⇒ Kleisli(_ ⇒ Monad[F].pure(Response(s)))
      }
    }

  sealed trait RequestOp[A]

  case class Lift[Op[_], A, J](j: J, action: Free[Op, A], mod: Transformer.Aux[Op, J]) extends RequestOp[A]

  case class Reply[A](s: A) extends RequestOp[Response[A]]

  object RequestOp {
    def pure[A](a: A): Free[RequestOp, A] = Free.pure(a)

    def lift[Op[_], A, J](j: J, action: Free[Op, A])(implicit mod: Transformer.Aux[Op, J]): Free[RequestOp, A] =
      Free.liftF(Lift(j, action, mod))

    def reply[A](s: A): Free[RequestOp, Response[A]] =
      Free.liftF[RequestOp, Response[A]](Reply[A](s))
  }

}
