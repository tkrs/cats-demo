package tut.fap

import java.time.Instant
import java.time.format.DateTimeFormatterBuilder

import cats.data.EitherK
import cats.syntax.apply._
import cats.{~>, Monad, MonadError}
import cats.free.{Free, FreeApplicative => FreeAp}

import scala.concurrent.{Await, Future}
import scala.concurrent.blocking
import scala.concurrent.duration._

// Imitate to [[http://frees.io/]] (´・ω・`)

object fs {

  type SeqF[F[_], A] = Free[FreeAp[F, ?], A]

  object SeqF {

    type Par[F[_], A] = FreeAp[F, A]

    def liftPar[F[_], A](fa: Par[F, A]): SeqF[F, A] =
      Free.liftF[Par[F, ?], A](fa)

    def pure[F[_], A](a: A): SeqF[F, A] =
      Free.pure[Par[F, ?], A](a)

    def inject[F[_], G[_]](implicit interpret: F ~> G): F ~> Par[G, ?] =
      λ[F ~> Par[G, ?]](fa => FreeAp.lift(interpret(fa)))

    def interpret[F[_], G[_]: Monad](implicit interpret: F ~> G): Par[F, ?] ~> G =
      λ[Par[F, ?] ~> G](_.foldMap(interpret))
  }

  implicit def handleInjection[F[_], G[_], H[_]](
      implicit
      f: F ~> H,
      g: G ~> H
  ): EitherK[F, G, ?] ~> H =
    λ[EitherK[F, G, ?] ~> H](_.fold(f, g))

  implicit def parToSeq[F[_], A](fa: SeqF.Par[F, A]): SeqF[F, A] =
    SeqF.liftPar(fa)
}

object models {
  final case class CA(s: String)
  final case class CB(l: Long)
}

object mod {
  import models._

  trait EffectLike[F[_]] {
    type FS[A] = FreeAp[F, A]
  }

  trait RepositoryA[F[_]] extends EffectLike[F] {
    def get(id: Long): FS[CA]
    def put(a: CA): FS[Unit]
  }

  object RepositoryA {
    sealed trait Op[A]              extends Product with Serializable
    final case class Get(id: Long)  extends Op[CA]
    final case class Put(value: CA) extends Op[Unit]

    implicit def repositoryA[F[_]](implicit I: Op ~> F): RepositoryA[F] = new RepositoryA[F] {
      override def get(id: Long): FS[CA] = fs.SeqF.inject[Op, F].apply(Get(id))
      override def put(a: CA): FS[Unit]  = fs.SeqF.inject[Op, F].apply(Put(a))
    }

    def apply[F[_]](implicit c: RepositoryA[F]): RepositoryA[F] = c

    trait Handler[M[_]] extends (Op ~> M) {

      protected[this] def get(id: Long): M[CA]
      protected[this] def put(value: CA): M[Unit]

      override def apply[A](fa: Op[A]): M[A] = fa match {
        case l @ Get(_) => get(l.id)
        case l @ Put(_) => put(l.value)
      }
    }
  }

  trait RepositoryB[F[_]] extends EffectLike[F] {
    def get(id: Long): FS[CB]
    def put(a: CB): FS[Unit]
  }

  object RepositoryB {
    sealed trait Op[A]              extends Product with Serializable
    final case class Get(id: Long)  extends Op[CB]
    final case class Put(value: CB) extends Op[Unit]

    implicit def repositoryB[F[_]](implicit I: Op ~> F): RepositoryB[F] = new RepositoryB[F] {
      override def get(id: Long): FS[CB] = fs.SeqF.inject[Op, F].apply(Get(id))
      override def put(a: CB): FS[Unit]  = fs.SeqF.inject[Op, F].apply(Put(a))
    }

    def apply[F[_]](implicit c: RepositoryB[F]): RepositoryB[F] = c

    trait Handler[M[_]] extends (Op ~> M) {

      protected[this] def get(id: Long): M[CB]
      protected[this] def put(value: CB): M[Unit]

      override def apply[A](fa: Op[A]): M[A] = fa match {
        case l @ Get(_) => get(l.id)
        case l @ Put(_) => put(l.value)
      }
    }
  }
}

object Repo {
  import fs._
  import mod._

  type Op[A] = EitherK[RepositoryA.Op, RepositoryB.Op, A]

  def interpret[F[_]: Monad](implicit i: Op ~> F): SeqF.Par[Op, ?] ~> F =
    SeqF.interpret[Op, F]

  object implicits {
    implicit val repoAtoRepo: RepositoryA.Op ~> Op = λ[RepositoryA.Op ~> Op](EitherK.leftc(_))
    implicit val repoBtoRepo: RepositoryB.Op ~> Op = λ[RepositoryB.Op ~> Op](EitherK.rightc(_))
  }
}

object Main {
  import fs._
  import mod._
  import models._
  import Repo.implicits._

  implicit def repoA[F[_]](
      implicit
      ME: MonadError[F, Throwable]
  ): RepositoryA.Handler[F] = new RepositoryA.Handler[F] {
    override protected[this] def get(id: Long): F[CA] =
      ME.catchNonFatal(blocking {
        MILLISECONDS.sleep(1000)
        Log.println(s"A.get($id)")
        CA("hola")
      })
    override protected[this] def put(a: CA): F[Unit] =
      ME.catchNonFatal(blocking {
        MILLISECONDS.sleep(500)
        Log.println(s"A.put($a)")
      })
  }

  implicit def repoB[F[_]](
      implicit
      ME: MonadError[F, Throwable]
  ): RepositoryB.Handler[F] = new RepositoryB.Handler[F] {
    override protected[this] def get(id: Long): F[CB] =
      ME.catchNonFatal(blocking {
        MILLISECONDS.sleep(800)
        Log.println(s"B.get($id)")
        CB(Long.MaxValue)
      })
    override protected[this] def put(b: CB): F[Unit] =
      ME.catchNonFatal(blocking {
        MILLISECONDS.sleep(200)
        Log.println(s"B.put($b)")
      })
  }

  def main(args: Array[String]): Unit = {
    import cats.instances.future._

    import scala.concurrent.ExecutionContext.Implicits.global

    def getA[F[_]](implicit A: RepositoryA[F]): SeqF[F, CA] =
      for {
        _ <- (A.put(CA("a")),
              A.put(CA("b")),
              A.put(CA("c")),
              A.put(CA("d")),
              A.put(CA("e")),
              A.put(CA("f")),
              A.put(CA("g"))).tupled
        a <- A.get(30)
      } yield a

    def getB[F[_]](implicit B: RepositoryB[F]): SeqF[F, (CB, CB)] =
      for {
        _ <- (B.put(CB(100L)), B.put(CB(200L)), B.put(CB(300L))).tupled
        b <- (B.get(10L), B.get(10L)).tupled
      } yield b

    def program[F[_]: RepositoryB: RepositoryA]: SeqF[F, (CA, (CB, CB))] =
      (getA[F], getB[F]).tupled

    Log.println(s"START")
    val f      = program[Repo.Op].foldMap(Repo.interpret[Future])
    val result = Await.result(f, 10.seconds)
    Log.println(result)
  }
}

object Log {
  private[this] val fmt = new DateTimeFormatterBuilder().appendInstant(3).toFormatter()
  def println(s: Any): Unit =
    scala.Predef.println(fmt.format(Instant.now) + " [" + Thread.currentThread().getName + "] " + s)
}
