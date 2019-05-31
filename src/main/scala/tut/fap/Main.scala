package tut.fap

import java.time.Instant
import java.time.format.DateTimeFormatterBuilder

import cats.data.EitherK
import cats.{~>, Eval, InjectK, Monad, MonadError}
import cats.free.{Free, FreeApplicative => FreeAp}

import scala.concurrent.{Await, Future}
import scala.concurrent.blocking
import scala.concurrent.duration._

object mod {
  type ParF[F[_], A] = FreeAp[F, A]
  type SeqF[F[_], A] = Free[ParF[F, ?], A]

  object ParF {
    def pure[F[_], A](a: A): ParF[F, A] = FreeAp.pure[F, A](a)
  }

  object SeqF {

    def liftPar[F[_], A](fa: ParF[F, A]): SeqF[F, A] =
      Free.liftF[ParF[F, ?], A](fa)

    def pure[F[_], A](a: A): SeqF[F, A] =
      Free.pure[ParF[F, ?], A](a)

    def inject[F[_], G[_]](implicit inj: InjectK[F, G]): F ~> ParF[G, ?] =
      λ[F ~> ParF[G, ?]](fa => FreeAp.lift(inj(fa)))

    def interpret[F[_], G[_]: Monad](interpret: F ~> G): ParF[F, ?] ~> G =
      λ[ParF[F, ?] ~> G](_.foldMap(interpret))
  }

  implicit def parToSeq[F[_], A](fa: ParF[F, A]): SeqF[F, A] =
    SeqF.liftPar(fa)

  implicit final class ParOp[F[_], A](val par: ParF[F, A]) extends AnyVal {
    def liftPar: SeqF[F, A] = SeqF.liftPar(par)
  }

  trait EffectLike[F[_]] {
    type FS[A] = FreeAp[F, A]
  }
}

import mod._

final case class CA(s: String)
final case class CB(l: Long)
final case class CC(l: Long)

trait RepoA[F[_]] extends EffectLike[F] {
  def get(id: Long): FS[CA]
  def put(a: CA): FS[Unit]
}

object RepoA {
  sealed trait Op[A]              extends Product with Serializable
  final case class Get(id: Long)  extends Op[CA]
  final case class Put(value: CA) extends Op[Unit]

  implicit def repositoryA[F[_]](implicit inj: InjectK[Op, F]): RepoA[F] = new RepoA[F] {
    def get(id: Long): FS[CA] = SeqF.inject[Op, F].apply(Get(id))
    def put(a: CA): FS[Unit]  = SeqF.inject[Op, F].apply(Put(a))
  }

  def apply[F[_]](implicit c: RepoA[F]): RepoA[F] = c

  trait Handler[M[_]] extends (Op ~> M) {

    protected def get(id: Long): M[CA]
    protected def put(value: CA): M[Unit]

    def apply[A](fa: Op[A]): M[A] = fa match {
      case l: Get => get(l.id)
      case l: Put => put(l.value)
    }
  }
}

trait RepoB[F[_]] extends EffectLike[F] {
  def get(id: Long): FS[CB]
  def put(a: CB): FS[Unit]
}

object RepoB {
  sealed trait Op[A]              extends Product with Serializable
  final case class Get(id: Long)  extends Op[CB]
  final case class Put(value: CB) extends Op[Unit]

  implicit def repositoryB[F[_]](implicit inj: InjectK[Op, F]): RepoB[F] = new RepoB[F] {
    def get(id: Long): FS[CB] = SeqF.inject[Op, F].apply(Get(id))
    def put(a: CB): FS[Unit]  = SeqF.inject[Op, F].apply(Put(a))
  }

  def apply[F[_]](implicit c: RepoB[F]): RepoB[F] = c

  trait Handler[M[_]] extends (Op ~> M) {

    protected def get(id: Long): M[CB]
    protected def put(value: CB): M[Unit]

    def apply[A](fa: Op[A]): M[A] = fa match {
      case l: Get => get(l.id)
      case l: Put => put(l.value)
    }
  }
}

trait RepoC[F[_]] extends EffectLike[F] {
  def get(id: Long): FS[CC]
  def put(a: CC): FS[Unit]
}

object RepoC {
  sealed trait Op[A]              extends Product with Serializable
  final case class Get(id: Long)  extends Op[CC]
  final case class Put(value: CC) extends Op[Unit]

  implicit def repositoryC[F[_]](implicit inj: InjectK[Op, F]): RepoC[F] = new RepoC[F] {
    def get(id: Long): FS[CC] = SeqF.inject[Op, F].apply(Get(id))
    def put(a: CC): FS[Unit]  = SeqF.inject[Op, F].apply(Put(a))
  }

  def apply[F[_]](implicit c: RepoC[F]): RepoC[F] = c

  trait Handler[M[_]] extends (Op ~> M) {

    protected def get(id: Long): M[CC]
    protected def put(value: CC): M[Unit]

    def apply[A](fa: Op[A]): M[A] = fa match {
      case l: Get => get(l.id)
      case l: Put => put(l.value)
    }
  }
}

object Repo {

  type Op0[A] = EitherK[RepoB.Op, RepoC.Op, A]
  type Op[A]  = EitherK[RepoA.Op, Op0, A]

  def interpret[F[_]: Monad](i: Op ~> F): ParF[Op, ?] ~> F =
    SeqF.interpret[Op, F](i)
}

object Main {

  def repoA[F[_]](implicit ME: MonadError[F, Throwable]): RepoA.Handler[F] = new RepoA.Handler[F] {
    def get(id: Long): F[CA] =
      ME.catchNonFatal(blocking {
        MILLISECONDS.sleep(100)
        Log.println(s"A.get($id)")
        CA("hola")
      })
    def put(a: CA): F[Unit] =
      ME.catchNonFatal(blocking {
        MILLISECONDS.sleep(500)
        Log.println(s"A.put($a)")
      })
  }

  def repoB[F[_]](implicit ME: MonadError[F, Throwable]): RepoB.Handler[F] = new RepoB.Handler[F] {
    def get(id: Long): F[CB] =
      ME.catchNonFatal(blocking {
        MILLISECONDS.sleep(300)
        Log.println(s"B.get($id)")
        CB(Long.MaxValue)
      })
    def put(b: CB): F[Unit] =
      ME.catchNonFatal(blocking {
        MILLISECONDS.sleep(500)
        Log.println(s"B.put($b)")
      })
  }

  def repoC[F[_]](implicit ME: MonadError[F, Throwable]): RepoC.Handler[F] = new RepoC.Handler[F] {
    def get(id: Long): F[CC]    = ME.catchNonFatalEval(Eval.later(CC(id)))
    def put(value: CC): F[Unit] = ME.catchNonFatal(Log.println(value))
  }

  def main(args: Array[String]): Unit = {
    import cats.instances.future._
    import cats.syntax.apply._

    import scala.concurrent.ExecutionContext.Implicits.global

    def putA[F[_]](implicit A: RepoA[F]): ParF[F, Unit] =
      (A.put(CA("a")), A.put(CA("b")), A.put(CA("c")), A.put(CA("d")), A.put(CA("e")), A.put(CA("f")), A.put(CA("g"))).tupled *> ParF
        .pure(())

    def putB[F[_]](implicit B: RepoB[F]): ParF[F, Unit] =
      (B.put(CB(100L)), B.put(CB(200L)), B.put(CB(300L))).tupled *> ParF.pure(())

    def program[F[_]](implicit A: RepoA[F], B: RepoB[F]): SeqF[F, (CA, CB, CB)] =
      for {
        _ <- (putA[F], putB[F]).tupled
        v <- (A.get(10L), B.get(20L), B.get(30L)).tupled
      } yield v

    def repo[F[_]](implicit F: MonadError[F, Throwable]): Repo.Op ~> F =
      repoA[F].or(repoB[F].or(repoC[F]))

    Log.println(s"START")

    val f      = program[Repo.Op].foldMap(Repo.interpret[Future](repo[Future]))
    val result = Await.result(f, 10.seconds)

    Log.println(result)
  }
}

object Log {

  private[this] val fmt = new DateTimeFormatterBuilder().appendInstant(3).toFormatter()

  def println(s: Any): Unit =
    scala.Predef.println(fmt.format(Instant.now) + " [" + Thread.currentThread().getName + "] " + s)
}
