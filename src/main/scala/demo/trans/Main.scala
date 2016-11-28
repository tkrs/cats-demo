package demo.trans

import java.time.{Instant, Duration => JD}

import cats.data.{Coproduct, Kleisli}
import cats.{Monad, ~>}
import cats.free.Free
import cats.instances.future._
import demo.trans.request.{RequestOp,interpreterK}
import demo.trans.service.{ServiceOp, ServiceOps}
import demo.trans.tag.{TagOp, TagOps}

import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Main extends App {

  def request(s: String)(implicit env: Env): Free[RequestOp, Response] = for {
    s <- RequestOp.lift(env, app(s))
    r <- RequestOp.reply(s.getOrElse("((((；ﾟДﾟ))))ｶﾞｸｶﾞｸﾌﾞﾙﾌﾞﾙ"))
  } yield r

  type DemoApp[A] = Coproduct[TagOp, ServiceOp, A]

  implicit def appTransformer(
    implicit
    T: Transformer.Aux[TagOp, Env],
    S: Transformer.Aux[ServiceOp, Env]
  ): Transformer.Aux[DemoApp, Env] =
    new Transformer[DemoApp] {
      type J = Env
      override def interpreterK[M[_] : Monad]: DemoApp ~> Kleisli[M, J, ?] = new (DemoApp ~> Kleisli[M, J, ?]) {
        type H[A] = Kleisli[M, J, A]
        override def apply[A](fa: DemoApp[A]): Kleisli[M, J, A] = {
          val f: TagOp ~> H = T.interpreterK[M]
          val g: ServiceOp ~> H = S.interpreterK[M]
          fa.fold(f, g)
        }
      }
    }

  def app(s: String)(implicit T: TagOps[DemoApp], S: ServiceOps[DemoApp]): Free[DemoApp, Option[String]] = for {
    _ <- S.put(10, s)
    x <- S.get(10)
    r <- T.reverse(x)
  } yield r

  implicit val env: Env = Env(db = TrieMap.empty[Int, String])

  {
    val start = Instant.now

    val r = Await.result(
      request("Hello").foldMap(interpreterK[Future]: RequestOp ~> FutureK).run(()),
      Duration.Inf
    )
    println(s"result: $r, elapsed: ${JD.between(start, Instant.now).toMillis}ms")
  }

  {
    val start = Instant.now

    val r = Await.result(
      request("Bonjour").foldMap(interpreterK[Future]: RequestOp ~> FutureK).run(()),
      Duration.Inf
    )
    println(s"result: $r, elapsed: ${JD.between(start, Instant.now).toMillis}ms")
  }

  {
    val start = Instant.now

    val r = Await.result(
      request("Ora").foldMap(interpreterK[Future]: RequestOp ~> FutureK).run(()),
      Duration.Inf
    )
    println(s"result: $r, elapsed: ${JD.between(start, Instant.now).toMillis}ms")
  }

}
