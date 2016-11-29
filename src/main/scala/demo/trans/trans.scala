package demo

import cats.data.{Coproduct, Kleisli}
import cats.free.Free
import cats.syntax.flatMap._
import demo.trans.service.{ServiceOp, ServiceOps}
import demo.trans.tag.{TagOp, TagOps}

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

package object trans {

  type FutureK[A] = Kleisli[Future, Unit, A]
  type DB = TrieMap[Int, String]
  type Action[A] = Coproduct[TagOp, ServiceOp, A]

  object Action {
    def action0(id: Int, s: String)(implicit T: TagOps[Action], S: ServiceOps[Action]): Free[Action, String] =
      S.put(id, s) >> pure("😉")

    def pure[A](a: A): Free[Action, A] = Free.pure(a)

    def action1(id: Int)(implicit T: TagOps[Action], S: ServiceOps[Action]): Free[Action, String] =
      (S.get(id) >>= T.reverse).map(_.getOrElse("😵"))

  }

}

package trans {

  final case class Env(db: DB)

  case class Response[A](content: A)

}
