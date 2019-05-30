package tut.fm

import cats.data.EitherK
import cats.free.Free
import cats.{Id, InjectK, ~>}

// See [[http://underscore.io/blog/posts/2017/03/29/free-inject.html]]

sealed trait FOp[A]
object FOp {
  case object One extends FOp[Int]

  def interpret: FOp ~> Id = new (FOp ~> Id) {
    override def apply[A](fa: FOp[A]): Id[A] = fa match {
      case One => 1
    }
  }
}

class FOps[F[_]](implicit F: InjectK[FOp, F]) {
  import FOp._
  def one: Free[F, Int] = Free.inject[FOp, F](One)
}

object FOps {
  implicit def fops[F[_]](implicit F: InjectK[FOp, F]): FOps[F] = new FOps
}

sealed trait GOp[A]
object GOp {
  case object Two extends GOp[Int]

  def interpret: GOp ~> Id = new (GOp ~> Id) {
    override def apply[A](fa: GOp[A]): Id[A] = fa match {
      case Two => 2
    }
  }
}

class GOps[F[_]](implicit F: InjectK[GOp, F]) {
  import GOp._
  def two: Free[F, Int] = Free.inject[GOp, F](Two)
}

object GOps {
  implicit def gops[F[_]](implicit F: InjectK[GOp, F]): GOps[F] = new GOps
}

sealed trait HOp[A]
object HOp {
  case object Three extends HOp[Int]

  def interpret: HOp ~> Id = new (HOp ~> Id) {
    override def apply[A](fa: HOp[A]): Id[A] = fa match {
      case Three => 3
    }
  }
}

class HOps[F[_]](implicit F: InjectK[HOp, F]) {
  import HOp._
  def three: Free[F, Int] = Free.inject[HOp, F](Three)
}

object HOps {
  implicit def hops[F[_]](implicit F: InjectK[HOp, F]): HOps[F] = new HOps
}

object Main {

  type GH[A] = EitherK[GOp, HOp, A]
  object GH {
    def interpret: GH ~> Id = GOp.interpret or HOp.interpret
  }

  type FGH[A] = EitherK[FOp, GH, A]
  object FGH {
    def interpret: FGH ~> Id = FOp.interpret or GH.interpret
  }

  def prog(implicit F: FOps[FGH], G: GOps[FGH], H: HOps[FGH]): Free[FGH, Int] =
    for {
      f <- F.one
      g <- G.two
      h <- H.three
    } yield f + g + h

  def main(args: Array[String]): Unit = {
    val ans: Int = prog.foldMap(FGH.interpret)
    println(ans)
  }
}
