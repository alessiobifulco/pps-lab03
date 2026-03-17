package it.unibo.pps.u03

import org.junit.Test
import org.junit.Assert.*
import u03.Streams.*
import u03.Sequences.*

class StreamsTest {

  @Test def testTakeWhile() =
    val s: Stream[Int] = Stream.cons(10, Stream.cons(20, Stream.cons(30, Stream.empty())))
    val streamResult = Stream.takeWhile(s)(_ < 25)
    val expected: Sequence[Int] = Sequence.Cons(10, Sequence.Cons(20, Sequence.Nil()))
    assertEquals(expected, Stream.toList(streamResult))

  @Test def testFill() =
    val i = 3
    val a = "a"
    val expected = Sequence.Cons("a", Sequence.Cons("a", Sequence.Cons("a", Sequence.Nil())))
    assertEquals(expected, Stream.toList(Stream.fill(i)(a)))
}