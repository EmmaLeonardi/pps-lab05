package ex

import util.Optionals.Optional
import util.Sequences.Sequence
import util.Sequences.Sequence.*

object CourseFunctions:
  extension (sequence: Sequence[Course])
    /*
    * Returns an optional containing the shared category of all the courses.
    * If the optional is empty the sequence is Nil or the courses have different categories
    * */
    def sameCategory(): Optional[String] =
      val fullSize = sequence.lenght()
      sequence match
        case Nil() => Optional.Empty()
        case Cons(h, _) =>
          val categorySize = sequence.filter(c => c.category == h.category).lenght()
          if categorySize == fullSize
          then Optional.Just(h.category)
          else Optional.Empty()


@main def TestCourseFunctions(): Unit =
  import ex.CourseFunctions.sameCategory
  val scalaCourse = Course("SCALA01", "Functional Programming in Scala", "Prof. Odersky", "Programming")
  val pythonCourse = Course("PYTHON01", "Introduction to Python", "Prof. van Rossum", "Programming")
  val designCourse = Course("DESIGN01", "UI/UX Design Fundamentals", "Prof. Norman", "Design")

  val sameCat = Cons(scalaCourse, Cons(pythonCourse, Nil()))
  val diffCat = Cons(scalaCourse, Cons(designCourse, Cons(pythonCourse, Nil())))
  val empty: Sequence[Course] = Nil()

  assert(sameCat.sameCategory() != Optional.Empty())
  assert(diffCat.sameCategory() == Optional.Empty())
  assert(empty.sameCategory() == Optional.Empty())

