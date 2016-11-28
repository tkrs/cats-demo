package demo.trans

import cats.{Monad, ~>}
import cats.data.Kleisli
import cats.free.Free
import demo.trans.service.ServiceOp
import demo.trans.tag.TagOp

trait Transformer[Op[_]] { mod =>
  type J
  type OpF[A] = Free[Op, A]

  def interpreterK[M[_]: Monad]: Op ~> Kleisli[M, J, ?]

  def transK[M[_]: Monad]: OpF ~> Kleisli[M, J, ?] =
    new (OpF ~> Kleisli[M, J, ?]) {
      override def apply[A](fa: OpF[A]): Kleisli[M, J, A] =
        fa.foldMap[Kleisli[M, J, ?]](interpreterK[M])
    }
}

object Transformer {
  type Aux[Op[_], J0] = Transformer[Op] { type J = J0 }

  implicit val serviceOpTrans: Transformer.Aux[ServiceOp, Env] = new Transformer[ServiceOp] {
    type J = Env
    override def interpreterK[M[_] : Monad]: ServiceOp ~> Kleisli[M, J, ?] = new (ServiceOp ~> Kleisli[M, J, ?]) {
      override def apply[A](fa: ServiceOp[A]): Kleisli[M, J, A] = fa.operate[M]
    }
  }

  implicit val tagOpTrans: Transformer.Aux[TagOp, Env] = new Transformer[TagOp] {
    type J = Env
    override def interpreterK[M[_] : Monad]: TagOp ~> Kleisli[M, J, ?] = new (TagOp ~> Kleisli[M, J, ?]) {
      override def apply[A](fa: TagOp[A]): Kleisli[M, J, A] = fa.operate[M]
    }
  }

}

