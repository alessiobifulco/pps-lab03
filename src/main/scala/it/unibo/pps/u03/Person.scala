package it.unibo.pps.u03

// An ADT: type + module
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

  import Person.*
  import u03.Sequences.*
  import u03.Sequences.Sequence.*

// a method outside the Person module
  def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _ => false

//  def coursesOfTeachers(persons: Sequence[Person]): Sequence[String] = persons match
//    case Cons(Teacher(n,c), t) => Cons(c, coursesOfTeachers(distinct(t)))
//    case Cons(Student(n,y), t) => coursesOfTeachers(distinct(t))
//    case Nil() => Nil()

  def coursesOfTeachers(persons: Sequence[Person]): Sequence[String] =
    distinct(map(filter(persons)(p => !isStudent(p)))(_ match
      case Teacher(n,c) => c
      case _ => ""
    ))

  def coursesOfTeachersFM(persons: Sequence[Person]): Sequence[String] =
    flatMap(distinct(persons))(_ match
      case Teacher(n,c) => Cons(c, Nil())
      case _ => Nil()
    )

  def coursesOfTeachers2(persons: Sequence[Person]): Sequence[String] = filter(persons)(p => !isStudent(p)) match
      case Cons(Teacher(_, c), t) => Cons(c, coursesOfTeachers2(distinct(t)))
      case Nil() => Nil()

  def totalNumberOfDistinctCourse(persons: Sequence[Person]): Int = persons match
    case Cons(Teacher(n,c), t) => totalNumberOfDistinctCourse(distinct(t)) + 1
    case Cons(Student(n, y), t) => totalNumberOfDistinctCourse(distinct(t))
    case Nil() => + 0









