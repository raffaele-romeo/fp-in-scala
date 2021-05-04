package iomonad

import cats.free.Free
import cats.free.Free._

sealed trait IO[A]{
  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap(this, f)
  def map[B](f: A => B): IO[B] =
    flatMap(f andThen (Return(_)))
}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO{
  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(resume) => resume()
    case FlatMap(sub, k)  => sub match {
      case Return(a) => run(k(a))
      case Suspend(resume) => run(k(resume()))
      case FlatMap(y: A, g: A) => run(y flatMap (a => g(a) flatMap k))
    }
  }
}

object TailRec {
  type TailRec[A] = Free[Function0, A]

  val f: Int => TailRec[Int] = (x: Int) => pure(x)
  val g: Int => TailRec[Int] = List.fill(10000)(f).foldLeft(f) {
    (a: (Int) => TailRec[Int],
     b: (Int) => TailRec[Int]) => {
      (x: Int) => defer(a(x).flatMap(b))
    }
  }

  def main(args: Array[String]): Unit = {
    val gFortyTwo = g(42)
    println("g(42) = " + gFortyTwo)
  }
}