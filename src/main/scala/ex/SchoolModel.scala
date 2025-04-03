package ex

import util.Sequences.Sequence
import util.Sequences.Sequence.{Cons, Nil, empty}


/*  Exercise 2:
 *  Implement the below trait, and write a meaningful test.
 *  Suggestions:
 *  - reuse Sequences and Optionals as imported above
 *  - For other suggestions look directly to the methods and their description
 */
trait SchoolModule:
  type School
  type Teacher
  type Course

  /**
   * This a factory method for create a teacher from a name
   * e.g.,
   * teacher("John") // => Teacher("John")
   * Note!! The internal representation of a teacher may vary, decide what is the best for you
   *
   * @param name the name of the teacher
   * @return the teacher created
   */
  def teacher(name: String): Teacher

  /**
   * This a factory method for create a course from a name
   * e.g.,
   * course("Math") // => Course("Math")
   * Note!! The internal representation of a course may vary, decide what is the best for you
   *
   * @param name the name of the course
   * @return the course created
   * */
  def course(name: String): Course

  /**
   * This method should return an empty school, namely a school without any teacher and course
   * e.g.,
   * emptySchool // => School(courses = Nil(), teachers = Nil(), teacherToCourses = Nil())
   * NOTE!! The above is just an example, the internal representation may vary, decide what is the best for you
   * You can store just the teacherToCourses, or having a case class for the school, or whatever you think is the best
   *
   * @return the empty school
   */
  def emptySchool(): School

  /**
   * This method should return a new school with the teacher assigned to the course
   * e.g.,
   * emptySchool
   * .setTeacherToCourse(teacher("John"), course("Math")) // => School(courses = Cons("Math", Nil()), teachers = Cons("John", Nil()), teacherToCourses = Cons(("John", "Math"), Nil()))
   * */
  def setTeacherToCourse(teacher: Teacher, course: Course): Unit

  /**
   * This method should return the list of courses assigned to a teacher
   * e.g.,
   * emptySchool.coursesOfATeacher(teacher("John")) // => Nil()
   * emptySchool
   * .setTeacherToCourse(teacher("John"), course("Math"))
   * .coursesOfATeacher(teacher("John")) // => Cons("Math", Nil())
   * emptySchool
   * .setTeacherToCourse(teacher("John"), course("Math"))
   * .setTeacherToCourse(teacher("John"), course("Italian"))
   * .coursesOfATeacher(teacher("John")) // => Cons("Math", Cons("Italian", Nil()))
   *
   * @return the list of courses assigned to a teacher
   */
  def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  /**
   * This method should return true if the teacher is present in the school
   * e.g.,
   * emptySchool.hasTeacher("John") // => false
   * emptySchool
   * .setTeacherToCourse(teacher("John"), course("Math"))
   * .hasTeacher("John") // => true
   *
   */
  def hasTeacher(name: String): Boolean

  /**
   * This method should return true if the course is present in the school
   * e.g.,
   * emptySchool.hasCourse("Math") // => false
   * emptySchool
   * .setTeacherToCourse(teacher("John"), course("Math"))
   * .hasCourse("Math") // => true
   *
   */
  def hasCourse(name: String): Boolean


  /**
   * This method should return the list of courses
   * e.g.,
   * emptySchool.courses // => Nil()
   * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).courses // => Cons("Math", Nil())
   * emptySchool
   * .setTeacherToCourse(teacher("John"), course("Math"))
   * .setTeacherToCourse(teacher("John"), course("Italian")).courses // => Cons("Math", Cons("Italian", Nil()))
   * Note!! If there are duplicates, just return them once
   *
   * @return the list of courses
   */
  def courses: Sequence[String]

  /**
   * This method should return the list of teachers
   * e.g.,
   * emptySchool.teachers // => Nil()
   * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).teachers // => Cons("John", Nil())
   * val john = teacher("John")
   * emptySchool
   * .setTeacherToCourse(john, course("Math"))
   * .setTeacherToCourse(john, course("Italian")).teachers // => Cons("John", Nil())
   * Note!! If there are duplicates, just return them once
   *
   * @return the list of teachers
   */
  def teachers: Sequence[String]

object SchoolModule:
  def apply(): SchoolModule = BasicSchoolModule()

case class BasicSchoolModule() extends SchoolModule:
  override type School = Sequence[(Teacher, Course)]
  override type Teacher = String
  override type Course = String
  var school: School = Nil()

  def courses: Sequence[String] = school.map((t, c) => c).filter(c => c != "").distinct()

  def teachers: Sequence[String] = school.map((t, c) => t).filter(t => t != "").distinct()

  def setTeacherToCourse(teacher: Teacher, course: Course): Unit = school=school.concat(Cons((teacher, course), Nil()))

  def coursesOfATeacher(teacher: Teacher): Sequence[Course] = school.filter((t, c) => t == t).map((t, c) => c)

  def hasTeacher(name: String): Boolean = school.map((t, c) => t).contains(name)

  def hasCourse(name: String): Boolean = school.map((t, c) => c).contains(name)

  def teacher(name: String): Teacher = name

  def course(name: String): Course = name

  def emptySchool(): School = Nil()

@main def examples(): Unit =
  val basicSchool = SchoolModule()

  val school = basicSchool.emptySchool()
  println(basicSchool.teachers) // Nil()
  println(basicSchool.courses) // Nil()
  println(basicSchool.hasTeacher("John")) // false
  println(basicSchool.hasCourse("Math")) // false
  val john = basicSchool.teacher("John")
  val math = basicSchool.course("Math")
  val italian = basicSchool.course("Italian")
  basicSchool.setTeacherToCourse(john, math)
  println(basicSchool.teachers) // Cons("John", Nil())
  println(basicSchool.courses) // Cons("Math", Nil())
  println(basicSchool.hasTeacher("John")) // true
  println(basicSchool.hasCourse("Math")) // true
  println(basicSchool.hasCourse("Italian")) // false
  basicSchool.setTeacherToCourse(john, italian)
  println(basicSchool.teachers) // Cons("John", Nil())
  println(basicSchool.courses) // Cons("Math", Cons("Italian", Nil()))
  println(basicSchool.hasTeacher("John")) // true
  println(basicSchool.hasCourse("Math")) // true
  println(basicSchool.hasCourse("Italian")) // true
  println(basicSchool.coursesOfATeacher(john)) // Cons("Math", Cons("Italian", Nil()))
  basicSchool.setTeacherToCourse(john, math)
  val mario = basicSchool.teacher("Mario")
  val oop = basicSchool.course("OOP")
  basicSchool.setTeacherToCourse(mario, oop)
  basicSchool.setTeacherToCourse(john, italian)
  println(basicSchool.teachers) // Cons("John",Cons("Mario", Nil()))
  println(basicSchool.courses) // Cons("Math", Cons("OOP", Cons("Italian", Nil())))
  println(basicSchool.hasTeacher("Mario")) // true
  println(basicSchool.hasCourse("OOP")) // true

  //New tests
  val emptySchool=SchoolModule()
  println("New tests")
  println(basicSchool.equals(emptySchool)) //false
  println(emptySchool.teachers) // Nil()
  println(emptySchool.courses) // Nil()



