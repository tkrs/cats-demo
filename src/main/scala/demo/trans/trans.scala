package demo

import cats.data.{Coproduct, Kleisli}
import cats.free.Free
import demo.trans.service.ServiceOp
import demo.trans.tag.TagOp

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

package object trans {

  type FutureK[A] = Kleisli[Future, Unit, A]
  type DB = TrieMap[Int, String]
  type Action[A] = Coproduct[TagOp, ServiceOp, A]
  def pure[A](a: A): Free[Action, A] = Free.pure(a)

}

package trans {

  final case class Env(db: DB)
  case class Response[A](content: A)
}
