package tut

import cats.data.Coproduct
import cats.free.Free
import cats.syntax.flatMap._
import tut.trans.service.{ServiceOp, ServiceOps}
import tut.trans.tag.{TagOp, TagOps}

import scala.collection.concurrent.TrieMap

package object trans {

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
