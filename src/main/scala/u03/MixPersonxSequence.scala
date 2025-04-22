package u03

enum Person:
  case Student(name: String, year: Int)
  case Teacher(name: String, course: String)

enum Sequence[E]:
  case Cons(head: E, tail: Sequence[E])
  case Nil()

object Sequence:

  import Sequence.*

  def sum(l: Sequence[Int]): Int = l match
    case Cons(h, t) => h + sum(t)
    case _ => 0

  def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
    case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
    case Nil() => Nil()

  def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
    case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
    case Cons(_, t) => filter(t)(pred)
    case Nil() => Nil()

  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
    case Nil() => Nil()
    case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))

  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
    case Cons(h, t) => Cons(h, concat(t, s2))
    case Nil() => s2

  def getCoursesOfTeachers(sequence: Sequence[Person]): Sequence[String] =
    flatMap(sequence) {
      case Person.Teacher(_, course) => Cons(course, Nil())
      case _ => Nil()
    }

  def foldLeft[A, B](seq: Sequence[A])(initial: B)(op: (B, A) => B): B = seq match
    case Sequence.Nil() => initial
    case Sequence.Cons(h, t) => foldLeft(t)(op(initial, h))(op)

  def countCourses(seq: Sequence[Person]): Int =
    val onlyTeachers = filter(seq) {
      case Person.Teacher(_, _) => true
      case _ => false
    }
    val ones = map(onlyTeachers)(_ => 1)
    foldLeft(ones)(0)(_ + _)

@main def tryPeopleCourses =
  import Sequence.*
  import Person.*

  val people = Cons(
    Teacher("Alice", "Math"),
    Cons(Student("Bob", 2),
      Cons(Teacher("Carol", "Physics"),
        Cons(Teacher("David", "History"), Nil())))
  )

  println(countCourses(people))