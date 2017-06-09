package tut.trans

import cats.{Monad, ~>}
import cats.data.Kleisli
import cats.free.Free
import tut.trans.service.ServiceOp
import tut.trans.tag.TagOp

// See [[https://github.com/tpolecat/doobie/blob/series/0.3.x/yax/core/src/main/scala/doobie/free/kleislitrans.scala]]
trait KleisliTrans[Op[_]] { mod ⇒
  type J
  type OpF[A] = Free[Op, A]

  def interpreterK[M[_] : Monad]: Op ~> Kleisli[M, J, ?]

  def transK[M[_] : Monad]: OpF ~> Kleisli[M, J, ?] =
    new (OpF ~> Kleisli[M, J, ?]) {
      override def apply[A](fa: OpF[A]): Kleisli[M, J, A] =
        fa.foldMap[Kleisli[M, J, ?]](interpreterK[M])
    }
}

object KleisliTrans {
  type Aux[Op[_], J0] = KleisliTrans[Op] {type J = J0}

  implicit val serviceOpTrans: KleisliTrans.Aux[ServiceOp, Env] = new KleisliTrans[ServiceOp] {
    type J = Env

    override def interpreterK[M[_] : Monad]: ServiceOp ~> Kleisli[M, J, ?] = new (ServiceOp ~> Kleisli[M, J, ?]) {
      override def apply[A](fa: ServiceOp[A]): Kleisli[M, J, A] = fa.operate[M]
    }
  }

  implicit val tagOpTrans: KleisliTrans.Aux[TagOp, Env] = new KleisliTrans[TagOp] {
    type J = Env

    override def interpreterK[M[_] : Monad]: TagOp ~> Kleisli[M, J, ?] = new (TagOp ~> Kleisli[M, J, ?]) {
      override def apply[A](fa: TagOp[A]): Kleisli[M, J, A] = fa.operate[M]
    }
  }

  implicit def appTransformation(implicit
                                 T: KleisliTrans.Aux[TagOp, Env],
                                 S: KleisliTrans.Aux[ServiceOp, Env]
  ): KleisliTrans.Aux[Action, Env] =
    new KleisliTrans[Action] {
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

