package u03

import u03.Optionals.Optional
import u03.Optionals.Optional.*

import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(_, t) if n > 0 => skip(t)(n - 1)
      case _                   => s

    def skip2[A](s: Sequence[A])(n: Int): Sequence[A] =
      @annotation.tailrec
      def _skip(currentS: Sequence[A])(remaining: Int): Sequence[A] = currentS match
        case Cons(_, t) if remaining > 0 => _skip(t)(remaining - 1)
        case _ => currentS
      _skip(s)(n)


    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h, t), Cons(h2, t2)) => Cons((h, h2), zip(t, t2))
      case _ => Nil()

    def zip2[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
      @tailrec
      def _zip(f: Sequence[A], s: Sequence[B], acc: Sequence[(A, B)]): Sequence[(A, B)] = (f, s) match
        case (Cons(h1, t1), Cons(h2, t2)) => _zip(t1, t2, Cons((h1, h2), acc))
        case _ => acc
      _zip(first, second, Nil())

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
      case Cons(h, t) => Cons(h, concat(t, s2))
      case Nil() => s2

    def concat2[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] =
      @tailrec
      def _concat(s1: Sequence[A], s2: Sequence[A], acc: Sequence[A]): Sequence[A] = s1 match
        case Cons(h,t) => _concat(t, s2, Cons(h, acc))
        case _ => acc
      _concat(s1,s2,Nil())

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse2[A](s: Sequence[A]): Sequence[A] =
      @annotation.tailrec
      def _reverse(curr: Sequence[A], acc: Sequence[A]): Sequence[A] = curr match
        case Cons(h, t) => _reverse(t, Cons(h, acc))
        case Nil()      => acc
      _reverse(s, Nil())

    def reverse[A](s: Sequence[A]): Sequence[A] = s match
      case Nil() => Nil()
      case Cons(h, Nil()) => Cons(h, Nil())
      case Cons(h,t) => concat(reverse(t), Cons(h, Nil()))


    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Nil() => Nil()
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))

    def flatMap2[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
      @tailrec
      def _flatMap[A,B](s: Sequence[A])(mapper: A => Sequence[B])(acc: Sequence[B]): Sequence[B] = s match
        case Nil() => acc
        case Cons(h,t) => _flatMap(t)(mapper)(concat(acc, mapper(h)))
      _flatMap(s)(mapper)(Nil())


    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */

    def min (s: Sequence[Int]): Optional[Int] = s match
      case Nil() => Empty()
      case Cons(h,t) if orElse(min(t), h) >= h => Just(h)
      case Cons(h,t) => min(t)

    def min2(s: Sequence[Int]): Optional[Int] =
      @annotation.tailrec
      def _min(curr: Sequence[Int], acc: Optional[Int]): Optional[Int] = curr match
        case Nil() => acc
        case Cons(h, t) =>
          val nextMin = acc match
            case Just(v) => if (h < v) Just(h) else Just(v)
            case Empty() => Just(h)
          _min(t, nextMin)
      _min(s, Empty())

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
      case Nil() => Nil()
      case Cons(h,Cons(_, t)) =>  Cons(h, evenIndices(t))
      case Cons(h, Nil()) => Cons(h, Nil())

    def evenIndices2[A](s: Sequence[A]): Sequence[A] = s match
      case Nil() => Nil()
      case Cons(h,t) => Cons(h, evenIndices2(skip(t)(1)))

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    @tailrec
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Cons(h,_) if h == elem => true
      case Cons(_,t) => contains(t)(elem)
      case _ => false

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */

    def distinct[A](s: Sequence[A]): Sequence[A] = s match
      case Nil() => Nil()
      case Cons(h, t) => Cons(h, distinct(filter(t)(_ != h)))

    def distinct2[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def _distinct(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
        case Nil() => acc
        case Cons(h, t) => if contains(acc)(h) then _distinct(t, acc) else _distinct(t, concat(acc, Cons(h, Nil())))
      _distinct(s, Nil())

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = s match
      case Cons(h, Nil()) => Cons(Cons(h, Nil()), Nil())
      case Nil() => Nil()
      case Cons(h, Cons(h2, t)) if h == h2 => Cons(Cons(h, Cons(h2, Nil())), group(t))
      case Cons(h, Cons(h2, t)) if h != h2 => Cons(Cons(h, Nil()), group(Cons(h2, t)))

      def group2[A](s: Sequence[A]): Sequence[Sequence[A]] =
        @tailrec
        def _group(s: Sequence[A], currentS: Sequence[A], acc: Sequence[Sequence[A]]): Sequence[Sequence[A]] =
          (s, currentS) match
            case (Nil(), Nil()) => reverse(acc)
            case (Nil(), _) => reverse(Cons(currentS, acc))
            case (Cons(h, t), Nil()) => _group(t, Cons(h, Nil()), acc)
            case (Cons(h, t), Cons(hc, _)) if h == hc => _group(t, Cons(h, currentS), acc)
            case (Cons(h, t), _) => _group(t, Cons(h, Nil()), Cons(currentS, acc))
        _group(s,Nil(),Nil())

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = s match
      case Nil() => (Nil(), Nil())
      case Cons(h,t) =>
        val(x,y) = partition(t)(pred)
        if pred(h) then (Cons(h, x),y) else (x, Cons(h,y))

    def partition2[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      @tailrec
      def _partition(s: Sequence[A], x: Sequence[A], y: Sequence[A]): (Sequence[A], Sequence[A]) = s match
        case Nil() => (reverse(x), reverse(y))
        case Cons(h, t) if pred(h) => _partition(t, Cons(h, x), y)
        case Cons(h, t) => _partition(t, x, Cons(h, y))
      _partition(s, Nil(), Nil())

    def foldLeft[A,B](s: Sequence[A])(default: B)(op: (B,A) => B): B = s match
      case Cons(h,t) => foldLeft(t)(op(default, h))(op)
      case Nil() => default



@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
