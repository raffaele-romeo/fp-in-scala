package monoid

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

sealed trait WC

object WC {

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) {
      m.zero
    } else if (as.length == 1) {
      f(as.head)
    } else {
      val (as1, as2) = as.splitAt(as.length / 2)
      m.op(foldMapV(as1, m)(f), foldMapV(as2, m)(f))
    }

  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC =
      (a1, a2) match {
        case (Stub(a), Stub(b)) => Stub(a + b)
        case (Stub(a), Part(l, w, r)) => Part(a + l, w, r)
        case (Part(l, w, r), Stub(a)) => Part(l, w, r + a)
        case (Part(l, w, r), Part(l1, w1, r1)) => Part(l, w + (if ((r + l1).isEmpty) 0 else 1) + w1, r1)
      }

    override def zero: WC = Stub("")
  }

  def count(s: String): Int = {
    def wc(c: Char): WC = {
      if (c.isWhitespace)
        Part("", 0, "")
      else Stub(c.toString)
    }

    def unstub(s: String) = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s) min 1
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }

  }
}

object Test extends App {
  val str = "Domani vado al mare"

  println( WC.count(str) )
}
