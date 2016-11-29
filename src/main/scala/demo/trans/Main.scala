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

  def runDaDoRunRun(implicit env: Env): Free[RequestOp, Unit] = for {
    r0 ← request(action0(10, "Hello"))
    _ ← RequestOp.pure(println(r0.content))
    r1 ← request(action1(10))
    _ ← RequestOp.pure(println(r1.content))
    r2 ← request(action1(11))
    _ ← RequestOp.pure(println(r2.content))
  } yield ()

  def request[A](action: Free[Action, A])(implicit env: Env): Free[RequestOp, Response[A]] = for {
    s ← RequestOp.lift(env, action)
    r ← RequestOp.reply(s)
  } yield r
}

object Main extends App {

  import F._

  implicit val env: Env = Env(db = TrieMap.empty[Int, String])
  val fut: Future[Unit] = runDaDoRunRun.foldMap(interpreterK[Future]: RequestOp ~> FutureK).run(())
  Await.result(fut, Duration.Inf)
}
