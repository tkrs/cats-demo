package tut.fap

import cats.data.Coproduct
import cats.{Monad, MonadError, ~>}
import cats.free.{Free, FreeApplicative => FreeAp}

import scala.concurrent.{Await, Future}
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
      λ[F ~> Par[G, ?]] { fa => FreeAp.lift(interpret(fa)) }

    def interpret[F[_], G[_]: Monad](implicit interpret: F ~> G): Par[F, ?] ~> G =
      λ[Par[F, ?] ~> G] { _.foldMap(interpret) }
  }

  implicit def handleInjection[F[_], G[_], H[_]](
    implicit
    f: F ~> H, g: G ~> H
  ): Coproduct[F, G, ?] ~> H =
    λ[Coproduct[F, G, ?] ~> H] { _.fold(f, g) }

  implicit def parToSeq[F[_], A](fa: SeqF.Par[F, A]): SeqF[F, A] = SeqF.liftPar(fa)
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
    sealed trait Op[A] extends Product with Serializable
    final case class Get(id: Long) extends Op[CA]
    final case class Put(value: CA) extends Op[Unit]

    class To[F[_]](implicit I: Op ~> F) extends RepositoryA[F] {
      override def get(id: Long): FS[CA] = fs.SeqF.inject[Op, F].apply(Get(id))
      override def put(a: CA): FS[Unit] =  fs.SeqF.inject[Op, F].apply(Put(a))
    }

    implicit def to[F[_]](implicit I: Op ~> F): To[F] = new To

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
    sealed trait Op[A] extends Product with Serializable
    final case class Get(id: Long) extends Op[CB]
    final case class Put(value: CB) extends Op[Unit]

    class To[F[_]](implicit I: Op ~> F) extends RepositoryB[F] {
      override def get(id: Long): FS[CB] = fs.SeqF.inject[Op, F].apply(Get(id))
      override def put(a: CB): FS[Unit] =  fs.SeqF.inject[Op, F].apply(Put(a))
    }

    implicit def to[F[_]](implicit I: Op ~> F): To[F] = new To

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

  type Op[A] = Coproduct[RepositoryA.Op, RepositoryB.Op, A]

  def interpret[F[_]: Monad](implicit i: Op ~> F): SeqF.Par[Op, ?] ~> F =
    SeqF.interpret[Op, F]

  object implicits {

    implicit val AtoRepo: RepositoryA.Op ~> Op = new (RepositoryA.Op ~> Op) {
      override def apply[A](fa: RepositoryA.Op[A]): Op[A] = fa match {
        case RepositoryA.Get(id) => Coproduct.leftc(RepositoryA.Get(id))
        case RepositoryA.Put(value) => Coproduct.leftc(RepositoryA.Put(value))
      }
    }

    implicit val BtoRepo: RepositoryB.Op ~> Op = new (RepositoryB.Op ~> Op) {
      override def apply[A](fa: RepositoryB.Op[A]): Op[A] = fa match {
        case RepositoryB.Get(id) => Coproduct.rightc(RepositoryB.Get(id))
        case RepositoryB.Put(value) => Coproduct.rightc(RepositoryB.Put(value))
      }
    }
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
    override protected[this] def get(id: Long): F[CA] = ME.catchNonFatal(CA("hola"))
    override protected[this] def put(a: CA): F[Unit] = ME.catchNonFatal(println(a))
  }

  implicit def repoB[F[_]](
    implicit
    ME: MonadError[F, Throwable]
  ): RepositoryB.Handler[F] = new RepositoryB.Handler[F] {
    override protected[this] def get(id: Long): F[CB] = ME.catchNonFatal(CB(Long.MaxValue))
    override protected[this] def put(b: CB): F[Unit] = ME.catchNonFatal(println(b))
  }

  def main(args: Array[String]): Unit = {
    import cats.instances.future._
    import cats.syntax.cartesian._

    import scala.concurrent.ExecutionContext.Implicits.global

    def getA[F[_]](implicit A: RepositoryA[F]): SeqF[F, CA] = for {
      _ <- (A.put(CA("a")) |@| A.put(CA("b"))).tupled
      a <- A.get(30)
    } yield a

    def getB[F[_]](implicit B: RepositoryB[F]): SeqF[F, CB] = B.get(10L)

    def prog[F[_]: RepositoryB: RepositoryA] = for {
      a <- getA[F]
      b <- getB[F]
    } yield (a, b)

    val f = prog[Repo.Op].foldMap(Repo.interpret[Future])
    val result = Await.result(f, 10.seconds)
    println(result)
  }
}
