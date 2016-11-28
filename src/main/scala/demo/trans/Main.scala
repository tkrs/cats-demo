package demo.trans

import cats.~>
import cats.free.Free
import cats.instances.future._
import cats.syntax.flatMap._
import demo.trans.request.{RequestOp,interpreterK}
import demo.trans.service.ServiceOps
import demo.trans.tag.TagOps

import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object F {
  def request[A](action: Free[Action, A])(implicit env: Env): Free[RequestOp, Response[A]] = for {
    s ← RequestOp.lift(env, action)
    r ← RequestOp.reply(s)
  } yield r

  def action0(id: Int, s: String)(implicit T: TagOps[Action], S: ServiceOps[Action]): Free[Action, String] =
    S.put(id, s) >> pure("＼(^o^)／")

  def action1(id: Int)(implicit T: TagOps[Action], S: ServiceOps[Action]): Free[Action, String] =
    (S.get(id) >>= T.reverse).map(_.getOrElse("((((；ﾟДﾟ))))ｶﾞｸｶﾞｸﾌﾞﾙﾌﾞﾙ"))

  def runDaDoRunRun(implicit env: Env): Free[RequestOp, Unit] = for {
    r0 ← request(action0(10, "Hello"))
    _  ← RequestOp.pure(println(r0))
    r1 ← request(action1(10))
    _  ← RequestOp.pure(println(r1))
    r2 ← request(action1(11))
    _  ← RequestOp.pure(println(r2))
  } yield ()
}

object Main extends App {
  import F._

  implicit val env: Env = Env(db = TrieMap.empty[Int, String])
  val fut = runDaDoRunRun.foldMap(interpreterK[Future]: RequestOp ~> FutureK).run(())
  Await.result(fut, Duration.Inf)
}
