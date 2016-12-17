package demo.trans

import cats.{Monad, ~>}
import cats.data.Kleisli
import cats.free.Free
import demo.trans.service.ServiceOp
import demo.trans.tag.TagOp

trait Transformation[Op[_]] { mod ⇒
  type J
  type OpF[A] = Free[Op, A]

  def interpreterK[M[_] : Monad]: Op ~> Kleisli[M, J, ?]

  def transK[M[_] : Monad]: OpF ~> Kleisli[M, J, ?] =
    new (OpF ~> Kleisli[M, J, ?]) {
      override def apply[A](fa: OpF[A]): Kleisli[M, J, A] =
        fa.foldMap[Kleisli[M, J, ?]](interpreterK[M])
    }
}

object Transformation {
  type Aux[Op[_], J0] = Transformation[Op] {type J = J0}

  implicit val serviceOpTrans: Transformation.Aux[ServiceOp, Env] = new Transformation[ServiceOp] {
    type J = Env

    override def interpreterK[M[_] : Monad]: ServiceOp ~> Kleisli[M, J, ?] = new (ServiceOp ~> Kleisli[M, J, ?]) {
      override def apply[A](fa: ServiceOp[A]): Kleisli[M, J, A] = fa.operate[M]
    }
  }

  implicit val tagOpTrans: Transformation.Aux[TagOp, Env] = new Transformation[TagOp] {
    type J = Env

    override def interpreterK[M[_] : Monad]: TagOp ~> Kleisli[M, J, ?] = new (TagOp ~> Kleisli[M, J, ?]) {
      override def apply[A](fa: TagOp[A]): Kleisli[M, J, A] = fa.operate[M]
    }
  }

  implicit def appTransformation(implicit
    T: Transformation.Aux[TagOp, Env],
    S: Transformation.Aux[ServiceOp, Env]
  ): Transformation.Aux[Action, Env] =
    new Transformation[Action] {
      type J = Env

      override def interpreterK[M[_] : Monad]: Action ~> Kleisli[M, J, ?] = new (Action ~> Kleisli[M, J, ?]) {
        type H[A] = Kleisli[M, J, A]

        override def apply[A](fa: Action[A]): Kleisli[M, J, A] = {
          val f: TagOp ~> H = T.interpreterK[M]
          val g: ServiceOp ~> H = S.interpreterK[M]
          fa.fold(f, g)
        }
      }
    }
}

