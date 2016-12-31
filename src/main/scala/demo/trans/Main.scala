package demo.trans

import cats.~>
import cats.free.Free
import cats.instances.future._
import demo.trans.request.{RequestOp, interpreterK}

import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object F {

  import Action._

  def runDaDoRunRun(implicit env: Env): Free[RequestOp, Response[String]] = for {
    _ ← request(action0(10, "Hello"))
    _ ← request(action1(10))
    r ← request(action1(11))
  } yield r

  def request[A](action: Free[Action, A])(implicit env: Env): Free[RequestOp, Response[A]] = for {
    s ← RequestOp.lift(env, action)
    r ← RequestOp.reply(s)
  } yield r
}

object Main extends App {

  import F._

  implicit val env: Env = Env(db = TrieMap.empty[Int, String])

  val fut: Future[Response[String]] =
    runDaDoRunRun.foldMap(interpreterK[Future]: RequestOp ~> Future)
  val res: Response[String] =
    Await.result(fut, Duration.Inf)

  println(res.content)
}
