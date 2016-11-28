package demo

import cats.data.Kleisli

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

package object trans {

  type FutureK[A] = Kleisli[Future, Unit, A]
  type DB = TrieMap[Int, String]
}

package trans {

  final case class Env(db: DB)
  case class Response(content: String)
}
