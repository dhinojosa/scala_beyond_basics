package com.ora.scalabeyondbasics

import org.scalatest.{FunSpec, Matchers}

import scala.collection.immutable

class AdvancedImplicitsSpec extends FunSpec with Matchers {
  describe(
    """Implicits is like a Map[Class[A], A] where A is any object
      |  and it is tied into the scope,
      |  and it is there when you need it, hence it is implicit. This provide
      |  a lot of great techniques that we
      |  can use in Scala.""".stripMargin) {

    implicit val defaultCountry:String = "US"

    it(
      """is done per scope so in the following example, we will
        |  begin with an implicit value
        |  and call it from inside a method which uses a
        |  multiple parameter list where one
        |  one group would """.stripMargin) {

      implicit val rate:Int = 20
      def calcPayment(hours:Int)(implicit currentRate:Int,
                                 defaultCountry:String) =
        (hours * currentRate) + defaultCountry


      calcPayment(40) should be ("800US")
    }

    it("""will allow you to place something manually, if you want to
        |  override the implicit value""".stripMargin) {
      implicit val rate:Int = 20
      def calcPayment(hours:Int)(implicit currentRate:Int,
                                 defaultCountry:String) =
        (hours * currentRate) + defaultCountry

      calcPayment(40)(50, "AUS") should be ("2000AUS")
    }

    it(
      """will gripe at compile time if there are two implicit bindings
        |  of the same type.  It's worth noting that what Scala doing
        |  are compile time tricks for implicit. One strategy is to
        |  wrap a value in a type to avoid conflict""".stripMargin) {

      case class Rate(value:Int)
      case class Age(value:Int)

      implicit val rate:Rate = Rate(20)
      implicit val age:Age = Age(50)
      def calcPayment(hours:Int)(implicit currentRate:Rate,
                                 defaultCountry:String) =
        (hours * currentRate.value) + defaultCountry

      calcPayment(10) should be ("200US")
    }


    it("""is really used to bind services that require something and
        |  you don't particularly need to inject everywhere explicitly, in this
        |  case let's discuss Future[+T]""".stripMargin) {
     import scala.concurrent._
     import java.util.concurrent.Executors

     val executorService = Executors.newFixedThreadPool(10) //Java
     implicit val executionContext = ExecutionContext.fromExecutorService(executorService)

     val future:Future[Int] = Future {
       println(s"Inside future: ${Thread.currentThread().getName}")
       Thread.sleep(2000)
       50 + 100
     }

      future.foreach(x => x should be (150))
    }


    it("""can bring up any implicit directly by merely
        |  calling up implicitly""".stripMargin) {

      case class Rate(value:Int)
      implicit val rate:Rate = Rate(30)

      def calcPayment(x:Int): String = {
        (x * implicitly[Rate].value) + implicitly[String]
      }

      calcPayment(20) should be ("600US")
    }

    it(
      """the implicit group parameter list, can contain
        |  more than one parameter, but needs to be in the same
        |  implicit parameter group""".stripMargin) {

       implicit val a: Int = 300

       def foo(x:Int)(implicit y:Int, s:String):String = (x * y) + s

       foo(2) should be ("600US")
    }

    it("""can also be replaced with default parameters,
        |  choose accordingly""".stripMargin) {
      def foo(x:Int, y:Int = 300, s:String = "US"):String = (x * y) + s

      foo(2) should be ("600US")
    }


    it("""can discriminate between List[String] implicitly will it try
        | to inject into a List[Double]?""".stripMargin) {

      implicit val listOfString: List[String] = List("Foo", "Bar", "Baz")
      implicit val listOfDouble: List[Double] = List(30.00, 12.00, 9.0)

      val result = implicitly[List[Double]]
      result should be (List(30.00, 12.00, 9.0))
    }

    it("""can be used for something like what Ruby has called
        |  monkey patching or Groovy calls mopping where we can add functionality to
        |  a class that we don't have access to, like isOdd/isEven
        |  in the Int class.  This is what we call implicit wrappers.
        |  First we will use a conversion method.""".stripMargin) {

      import scala.language.implicitConversions

      class IntWrapper(x:Int) {
        def isOdd:Boolean = x % 2 != 0
        def isEven:Boolean = !isOdd
      }

      implicit def int2IntWrapper(x:Int):IntWrapper = new IntWrapper(x)

      10.isOdd should be (false)
    }


    it("""can use implicit wrappers can be created using a function and is
        |  often easier to mental map.""".stripMargin) {
      import scala.language.implicitConversions

      class IntWrapper(x:Int) {
        def isOdd:Boolean = x % 2 != 0
        def isEven:Boolean = !isOdd
      }

      implicit val int2IntWrapper:Int => IntWrapper = (x:Int) => new IntWrapper(x)

      10.isOdd should be (false)
    }


    it("""Place a implicit inside of an object and use it""".stripMargin) {

      object MyLibrary {
        import scala.language.implicitConversions

        class IntWrapper(x: Int) {
          def isOdd: Boolean = x % 2 != 0
          def isEven: Boolean = !isOdd
        }

        implicit val int2IntWrapper: Int => IntWrapper =
          (x: Int) => new IntWrapper(x)
      }

      import MyLibrary._

      10.isOdd should be (false)
    }

    it("""can be use a short hand version of this called implicit classes,
        |  before using them there are some rules:
        |  1. They can only be used inside of an object/trait/class
        |  2. They can only take one parameter in the constructor
        |  3. There can not be any colliding method name as that
        |     with the implicit outer scope""".stripMargin) {

      import scala.language.implicitConversions

      implicit class IntWrapper(x: Int) {
        def isOdd: Boolean = x % 2 != 0
        def isEven: Boolean = !isOdd
      }

      3 -> "Three"
      25.isOdd should be (true)
    }

    it(
      """can also convert things to make it fit into a particular API,
        |  this is called implicit conversion,
        |  in this scenario we will use a method""".stripMargin) {

      import scala.language.implicitConversions

      sealed abstract class Currency
      case class Dollar(value:Int) extends Currency
      case class Yuan(value:Int) extends Currency
      case class Euro(value:Int) extends Currency

      def addDollars(x:Dollar, y:Dollar) = Dollar(x.value + y.value)

      addDollars(Dollar(10), Dollar(20)) should be (Dollar(30))

      //recipe for conversion
      implicit def int2Dollar(x:Int):Dollar = Dollar(x)

      addDollars(10, 20) should be (Dollar(30))
    }

    it(
      """can also convert things to make it fit into a particular API,
        |  this is called implicit conversion,
        |  in this scenario we will use a function""".stripMargin) {

      import scala.language.implicitConversions

      sealed abstract class Currency
      case class Dollar(value:Int) extends Currency
      case class Yuan(value:Int) extends Currency
      case class Euro(value:Int) extends Currency

      def addDollars(x:Dollar, y:Dollar) = Dollar(x.value + y.value)

      addDollars(Dollar(10), Dollar(20)) should be (Dollar(30))

      //recipe for conversion
      implicit val int2Dollar: Int => Dollar = (x:Int) => Dollar(x)

      addDollars(10, 20) should be (Dollar(30))

    }

    it("""is done automatically in Scala because what is inside of
        |  scala.Predef, for example, it explains how be can set a
        |  scala.Float , and there is java.lang.Float,
        |  java primitive float.
        |  We can investigate this by looking at
        |  the documentation.""".stripMargin) {

      val f1:scala.Float = 3000.0f
      val f2:scala.Float = new java.lang.Float(4000.0f)

      val result = java.lang.Math.min(f1, f2)
      result should be (3000.0f)
    }
  }

  describe("Locating implicits recipes") {
    it("""has a common way, to store that particular implicit
        |  recipe in an object that makes should make
        |  sense and then import that object""".stripMargin) {

      object MyPredef {
        import scala.language.implicitConversions

        class IntWrapper(x: Int) {
          def isOdd: Boolean = x % 2 != 0
          def isEven: Boolean = !isOdd
        }

        implicit val int2IntWrapper: Int => IntWrapper =
          (x: Int) => new IntWrapper(x)
      }

      import MyPredef._

      10.isOdd should be (false)
    }

    it("""can also use a companion object to store
        |  any implicit recipes""".stripMargin) {
      class Artist(val firstName:String, val lastName:String)
      object Artist {
        import scala.language.implicitConversions
        implicit def tupleToArtist(x:(String, String)): Artist
              = new Artist(x._1, x._2)
      }

      def playPerformer(a:Artist) = s"Now playing: ${a.firstName} ${a.lastName}"

      playPerformer(new Artist("Elvis", "Presley"))
      playPerformer("Suzanne" -> "Vega")
    }

    it( """can also use a package object to store some of these implicits""") {
      def numItems(list:List[String]) = list.mkString(",")

      numItems(3 -> "Wow") should be ("Wow,Wow,Wow")
    }

    it("""can use JavaConverters to convert a collection in
        |  Java to Scala and vice versa""".stripMargin) {

      import scala.collection.JavaConverters._
      import java.time._
      ZoneId.getAvailableZoneIds.asScala.toList
        .filter(s => s.startsWith("America"))
        .map(s => s.split("/").last)
        .sorted
        .groupBy(x => x.head)
    }
  }

  describe("""View Bounds are used to ensure that there is a particular " +
              |  recipe for a certain type".stripMargin""") {
    it("""Uses <% inside of a parameterized type declaration to determine
        |  if there is a conversion available then within you can treat
        |  an object as an object of that type. It is unorthodox,
        |  and has since been deprecated.""".stripMargin) {

      class Employee(val firstName:String, val lastName:String)

      import scala.language.implicitConversions

      implicit def str2Employee(s:String):Employee = {
        val tokens = s.split(" ")
        new Employee(tokens.head, tokens.last)
      }

      def hireEmployee[A <% Employee](a:A) = {
        s"Hired this employee named ${a.firstName} ${a.lastName}"
      }

      //Another take, but preferred
      def hireEmployee2[A](a:A)(implicit ev: A => Employee) = {
        val employee: Employee = ev(a)
        s"Hired this employee named ${employee.firstName} ${employee.lastName}"
      }

      hireEmployee("Joe Armstrong") should be ("Hired this employee named Joe Armstrong")
    }
  }

  describe(
    """Context Bounds works so that there is a type A, and it
      |  requires a B[A] somewhere within the the implicit scope,
      |  for example like Ordered[T], or TypeTag[T], or Numeric[T],
      |  this provides a way to check that something is something
      |  can be implicitly defined but gives the end user no opportunity
      |  to the ability to inject a different implementation""".stripMargin) {

    it("""uses the signature [T:WrappedType], which is
        |  equivalent to (t:T)(implicit w:WrappedType[T])
        |  let's try it with """.stripMargin) {

      class Employee(val firstName:String, val lastName:String)

      trait Loggable[T] {
        def log(t:T):String
      }

      object EmployeePredef {
        import scala.language.implicitConversions

        implicit val employeeToStringFull: Loggable[Employee] = new Loggable[Employee] {
          override def log(t: Employee): String =
            s"Employee(firstName:${t.firstName}, lastName:${t.lastName})"
        }

        implicit val employeeToStringShort: Loggable[Employee] = new Loggable[Employee] {
          override def log(t: Employee): String =
            s"Employee(${t.firstName}, ${t.lastName})"
        }
      }

      import EmployeePredef.employeeToStringFull

      def myToString[A : Loggable](a: A) = {
        implicitly[Loggable[A]].log(a)
      }

      //Allows the end user to plug their own
      def myToString2[A](a: A)(implicit conv:Loggable[A]) = {
         conv.log(a)
      }

      myToString(new Employee("Satya", "Nadella")) should be ("Employee(firstName:Satya, lastName:Nadella)")
    }
  }

  describe(
    """Type Constraints are used to ensure that a particular method can run
      | if a particular generic is of a certain type, this is typically
      | used for one method""".stripMargin) {

    it("""uses one operator, =:= which is actually the full type =:=[A,B] that
        |  will to see if something is of the same type""".stripMargin) {

      class MyPair[A,B](a:A, b:B) {
        def first:A = a
        def second:B = b
        def toList(implicit ev: A =:= B):List[A] = List(a,b).asInstanceOf[List[A]]
      }

      val myPair = new MyPair(3, 10)
      myPair.toList should be (List(3, 10))
    }

    it("""uses the operator, <:< which will test if A is a subtype of B""") {
      pending
    }
  }

  describe("Getting around Erasure Using TypeTags") {
    it("""used to use Manifest but now uses a type tag to
          |  retrieve what is erased""".stripMargin) {

      import scala.reflect.runtime.universe._

      def matchList[A](list:List[A])(implicit tt:TypeTag[A]):String = {
        tt.tpe match {
          case t if t =:= typeOf[String] => "List of String"
          case t if t =:= typeOf[Int] => "List of Int"
          case _ => "List of Something"
        }
      }

      matchList(List(1,2,3,4)) should be ("List of Int")
    }
  }

  describe("""Typeclasses are a way of generating or extending behavior
      |  using Java-like interfaces,
      |  but operate as outside.  There is another term for this,
      |  and it's called ad-hoc polymorphism""".stripMargin) {

    it("""can be used to determine equality, so whether than make equals inside of an class,
          |  it is now an outside concern""".stripMargin) {
      pending
    }

    it("can be used for ordering") {
      class Employee(val firstName:String, val lastName:String)
      object Employee {
        implicit val employeeOrderingByFirstName: Ordering[Employee] =
          new Ordering[Employee] {
            override def compare(x: Employee, y: Employee): Int =
              x.firstName.compareTo(y.firstName)
          }

        implicit val employeeOrderingByLastName: Ordering[Employee] =
          new Ordering[Employee] {
            override def compare(x: Employee, y: Employee): Int =
              x.lastName.compareTo(y.lastName)
        }
      }

      val employees = List(new Employee("Robert", "Dylan")
                           ,new Employee("Justin", "Bieber")
                           ,new Employee("Carly", "Simon")
                           ,new Employee("Frank", "Zappa")
                           ,new Employee("Stevie", "Wonder")
                           ,new Employee("Paul", "McCartney")
                           ,new Employee("Prince", "Rogers-Nelson"))


      import Employee.employeeOrderingByFirstName
      employees.sorted.head.firstName should be ("Carly")
    }
  }
}
