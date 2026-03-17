package it.unibo.pps.u03

import org.junit.Test
import org.junit.Assert.*

class PersonTest {

  import u03.Sequences.*
  import Sequence.*
  import Person.*

    val p1: Person = Person.Student(" mario ", 2015)
    val p2: Teacher = Person.Teacher(" mirko ", "PPS")
    val p3: Teacher = Person.Teacher(" mario ", "LCMC")
    val p4: Teacher = Person.Teacher(" alessandro ", "PCD")
    val p5: Teacher = Person.Teacher(" alessandro ", "PCD")
    val s: Sequence[Person] = Cons(p1, Cons(p2, Cons(p3, Cons(p4, Cons(p5, Nil())))))


  @Test def testCoursesOfTeacher() =
    val result: Sequence[String] = Cons("PPS", Cons("LCMC", Cons("PCD", Nil())))
    assertEquals(result, coursesOfTeachers(s))
    assertEquals(result, coursesOfTeachersFM(s))

  @Test def testTotalNumberOfDistinctCourses() =

    val result: Int = 3
    assertEquals(result, totalNumberOfDistinctCourse(s))
    assertEquals(result, totNumOfCourse(s))
}
