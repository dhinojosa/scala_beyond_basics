package com.ora.scalabeyondbasics

import java.util.concurrent.Executors

import org.scalatest.{FunSpec, Matchers}

class AdvancedImplicitsSpec extends FunSpec with Matchers {


  describe(
    """Implicits is like a Map[Class[A], A] where A is any object and it is tied into the scope,
      | and it is there when you need it, hence it is implicit. This provide a lot of great techniques that we
      | can use in Scala.""".stripMargin) {


    it(
      """is done per scope so in the following example, we will begin with an implicit value
        |  and call it from inside a method which uses a multiple parameter list where one
        |  one group would """.stripMargin) {

      implicit val hourlyRate = 100 //invisible map in our scope map.put(Int.class, 100)

      def calcPayment(hours: Int)(implicit rate: Int) = hours * rate

      calcPayment(50) should be(5000)
    }

    it("""will allow you to place something manually, if you want to override the implicit value""".stripMargin) {

      implicit val hourlyRate = 100 //invisible map in our scope map.put(Int.class, 100)

      def calcPayment(hours: Int)(implicit rate: Int) = hours * rate

      calcPayment(50)(200) should be(10000)
    }

    it(
      """will gripe at compile time if there are two implicit bindings of the same type.  It's
        |  worth noting that what Scala doing are compile time tricks for implicit. One strategy is to
        |  wrap a value in a type to avoid conflict""".stripMargin) {

      case class Rate(value: Int)
      case class Age(value: Int)

      implicit val rate = Rate(100)
      implicit val age = Age(40)

      def calcPayment(hours: Int)(implicit rate: Rate) = hours * rate.value

      calcPayment(50) should be(5000)
    }


    it(
      """is really used to bind services that require something and
        |  you don't particularly need to inject everywhere explicitly, in this
        |  case let's discuss Future[+T]""".stripMargin) {

      import scala.concurrent._

      implicit val executionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))

      val f = Future.apply {
        println(Thread.currentThread().getName)
        Thread sleep 4000
        100 + 40
      }

      f foreach println

      Thread sleep 5000
    }


    it( """can bring up any implicit directly by merely calling up implicitly""") {
      case class IceCream(name: String)
      case class Scoops(num: Int, flavor: IceCream)

      implicit val flavorOfTheDay = IceCream("Mayday Cherry Whip")

      def orderIceCream(num: Int) = Scoops(num, implicitly[IceCream])

      orderIceCream(2) should be(Scoops(2, IceCream("Mayday Cherry Whip")))
    }

    it(
      """the implicit group parameter list, can contain more than one parameter, but
        |  needs to be in the same implicit parameter group""".stripMargin) {

      implicit val bonus = 5000
      implicit val currency = "Euro"

      def calculateYearRate(amount: Int)(implicit bonus: Int, currency: String): String = {
        amount + bonus + " " + currency
      }

      calculateYearRate(60000) should be("65000 Euro")
    }

    it( """can also be replaced with default parameters, choose accordingly""") {
      def calculateYearRate(amount: Int, bonus: Int = 5000, currency: String = "Euro"): String = {
        amount + bonus + " " + currency
      }

      calculateYearRate(60000) should be("65000 Euro")
    }


    it("""Christopher A. Question: List[String] and List[Double]""") {
      implicit val listOfInt = List(1, 2, 3, 4, 5)
      implicit val listOfString = List("foo", "bar", "baz")

      def myAppend[A](x: A)(implicit list: List[A]): List[A] = {
        list :+ x
      }

      myAppend(6) should be(List(1, 2, 3, 4, 5, 6))
      myAppend("Zoom") should be(List("foo", "bar", "baz", "Zoom"))
    }


    it(
      """can be used for something like what Ruby has called
        |  monkey patching or Groovy calls mopping where we can add functionality to
        |  a class that we don't have access to, like isOdd/isEven
        |  in the Int class.  This is what we call implicit wrappers.
        |  First we will use a conversion method.""".stripMargin) {

      class IntWrapper(x: Int) {
        def isOdd: Boolean = x % 2 != 0

        def isEven: Boolean = !isOdd
      }

      import scala.language.implicitConversions

      implicit def int2IntWrapper(x: Int): IntWrapper = new IntWrapper(x)

      40.isOdd should be(false)
      33.isOdd should be(true)
      40.isEven should be(true)
    }


    it( """Implicit wrappers can be created using a function and is often easier to mental map.""".stripMargin) {
      class IntWrapper(x: Int) {
        def isOdd: Boolean = x % 2 != 0

        def isEven: Boolean = !isOdd
      }

      import scala.language.implicitConversions

      implicit def int2IntWrapper = (x: Int) => new IntWrapper(x)

      40.isOdd should be(false)
      33.isOdd should be(true)
      40.isEven should be(true)
    }

    it(
      """can be use a short hand version of this called implicit classes, before using them
        |  there are some rules:
        |  1. They can only be used inside of an object/trait/class
        |  2. They can only take one parameter in the constructor
        |  3. There can not be any colliding method name as that
        |     with the implicit outer scope""".stripMargin) {

      import scala.language.implicitConversions

      implicit class IntWrapper(x: Int) {
        def isOdd: Boolean = x % 2 != 0

        def isEven: Boolean = !isOdd
      }

      40.isOdd should be(false)
      33.isOdd should be(true)
      40.isEven should be(true)
    }

    it(
      """can also convert things to make it fit into a particular API,
        | this is called implicit conversion,
        | in this scenario we will use a method""".stripMargin) {

      import scala.language.implicitConversions

      sealed abstract class Currency
      case class Dollar(value: Int) extends Currency
      case class Yen(value: Int) extends Currency

      implicit def int2Dollar(x: Int): Dollar = Dollar(x)

      def addDollars(x: Dollar, y: Dollar) = Dollar(x.value + y.value)

      addDollars(40, 100)
    }

    it(
      """can also convert things to make it fit into a particular API,
        | this is called implicit conversion,
        | in this scenario we will use a function""".stripMargin) {
      import scala.language.implicitConversions

      sealed abstract class Currency
      case class Dollar(value: Int) extends Currency
      case class Yen(value: Int) extends Currency

      implicit val int2Dollar = (x: Int) => Dollar(x)

      def addDollars(x: Dollar, y: Dollar) = Dollar(x.value + y.value)

      addDollars(40, 100)
    }

    it(
      """is done automatically in Scala because what is inside of scala.Predef, for example,
        |  it explains how be can set a scala.Float , and there is java.lang.Float,
        |  java primitive float.
        |  We can investigate this by looking at
        |  the documentation.""".stripMargin) {

      val f: scala.Float = 4001.00f
      val f2: scala.Float = 3011.00f
      val result = java.lang.Math.min(f, f2)

      result should be(3011.00f)

    }
  }

  describe("Locating implicits recipes") {
    it(
      """has a common way, to store that particular implicit
        |  recipe in an object that makes should make
        |  sense and then import that object""".stripMargin) {

      object MyPredef {

        import scala.language.implicitConversions

        implicit class IntWrapper(x: Int) {
          def isOdd: Boolean = x % 2 != 0

          def isEven: Boolean = !isOdd
        }

      }

      import MyPredef._

      10.isEven should be(true)
      13.isEven should be(false)
    }

    it( """can also use a companion object to store any implicit recipes""".stripMargin) {
      class Artist(val firstName: String, val lastName: String)
      object Artist {

        import scala.language.implicitConversions

        implicit def tupleToArtist(t: Tuple2[String, String]): Artist = new Artist(t._1, t._2)
      }

      def playArtist(a: Artist) = s"The artist ${a.firstName} ${a.lastName} is performing"

      playArtist("Taylor" -> "Swift") should be("The artist Taylor Swift is performing")
    }

    it( """can also use a package object to store some of these implicits""") {
      def numItems(list: List[String]) = list.reduce((total: String, next: String) => total + next)

      numItems(3 -> "Whoa") should be("WhoaWhoaWhoa")
    }

    it("""can use JavaConverters to convert a collection in Java to Scala and vice versa""") {
      import java.time._
      import scala.collection.JavaConverters._
      val result = ZoneId.getAvailableZoneIds.asScala
        .toList.filter(_.startsWith("America"))
        .map(_.split("/")(1)).sorted.filter(_.startsWith("L"))
      result.size should be(5)
    }
  }

  describe("View Bounds are used to ensure that there is a particular recipe for a certain type") {
    it(
      """Uses <% inside of a parameterized type declaration to determine if there is a conversion available
        | then within you can treat an object as an object of that type. It is unorthodox, and has since been
        | deprecated.""".stripMargin) {

      class Employee(val firstName: String, val lastName: String)
      import scala.language.implicitConversions

      implicit def str2Employee(s: String): Employee = {
        s.split(" ").toList match {
          case Nil => new Employee("John", "Doe")
          case fn :: Nil => new Employee(fn, "Doe")
          case fn :: ln :: _ => new Employee(fn, ln)
        }
      }

      implicit def tuple22Employee(t: Tuple2[String, String]): Employee = {
        new Employee(t._1, t._2)
      }

      def hireEmployee[A <% Employee](a: A): String = {
        s"I have hired ${a.firstName} ${a.lastName}"
      }

      hireEmployee("George Takei") should be("I have hired George Takei")
      hireEmployee("William" -> "Shatner") should be("I have hired William Shatner")
    }
  }

  describe(
    """Context Bounds works so that there is a type A, and it requires a B[A] somewhere
      |  within the the implicit scope, for example like Ordered[T], or TypeTag[T], or Numeric[T],
      |  this provides a way to check that something is something can be implicitly defined but
      |  gives the end user no opportunity to the ability to inject a different implementation""".stripMargin) {

    it(
      """uses the signature [T:WrappedType], which is
        | equivalent to (t:T)(implicit w:WrappedType[T])
        | let's try it with """.stripMargin) {

      trait Loggable[T] {
        def log(t: T): String
      }

      class Employee(val firstName: String, val lastName: String)

      class Department(val name: String)

      object MyImplicitRecipes {

        //type classes!
        implicit val employeeLoggable: Loggable[Employee] = new Loggable[Employee] {
          override def log(e: Employee): String = s"Employee: ${e.firstName} ${e.lastName}"
        }

        implicit val employeeLoggableBrackets: Loggable[Employee] = new Loggable[Employee] {
          override def log(e: Employee): String = s"Employee[${e.firstName}, ${e.lastName}]"
        }

        implicit val deptLoggable: Loggable[Department] = new Loggable[Department] {
          override def log(d: Department): String = s"~~~Department~${d.name}~~~"
        }
      }

      //The recipe we are looking for is Loggable[T]
      def toStringz[T: Loggable](t: T): String = {
        val implicitLoggable = implicitly[Loggable[T]]
        implicitLoggable.log(t)
      }

      import MyImplicitRecipes.employeeLoggableBrackets
      import MyImplicitRecipes.deptLoggable
      toStringz(new Employee("Leonard", "Nimoy")) should be("Employee[Leonard, Nimoy]")
      toStringz(new Department("Toys")) should be("~~~Department~Toys~~~")
    }
  }

  describe(
    """Type Constraints are used to ensure that a particular method can run
      | if a particular generic is of a certain type, this is typically used for
      | one method""".stripMargin) {

    it(
      """uses one operator, =:= which is actually the full type =:=[A,B] that
        |  will to see if something is of the same type""".stripMargin) {

      class MyPair[A, B](val a: A, val b: B) {
        def first = a

        def second = b

        def toList(implicit ev: A =:= B): List[A] = List(a, b).asInstanceOf[List[A]]
      }

      val myPair = new MyPair(10, 50)
      myPair.toList should be(List(10, 50))
    }

    it("""uses the operator, <:< which will test if A is a subtype of B""") {
      pending
    }
  }

  describe("Getting around Erasure Using TypeTags") {
    it("used to use Manifest but now uses a type tag to retrieve what is erased") {

      import scala.reflect.runtime.universe._
      def matchList[A](list: List[A])(implicit tt: TypeTag[A]): String = {
        tt.tpe match {
          case t if t =:= typeOf[Long] => "List of Long"
          case t if t =:= typeOf[Int] => "List of Int"
          case t if t =:= typeOf[String] => "List of String"
          case _ => "List of Unknowns"
        }
      }

      matchList(List(1, 2, 3, 4, 5)) should be("List of Int")
    }
  }

  describe(
    """Typeclasses are a way of generating or extending behavior using Java-like interfaces,
      |  but operate as outside.  There is another term for this,
      |  and it's called ad-hoc polymorphism""".stripMargin) {

    it(
      """can be used to determine equality, so whether than make equals inside of an class,
        | it is now an outside concern""".stripMargin) {

      pending
    }

    it("can be used for ordering") {
      List(6, 4, 2, 1, 0, 3).sorted should be(List(0, 1, 2, 3, 4, 6))

      class Employee(val firstName: String, val lastName: String)  {
        override def equals(o:Any): Boolean = {
            if (!o.isInstanceOf[Employee]) false
            else {
                val other:Employee = o.asInstanceOf[Employee]
                this.firstName == other.firstName &&
                this.lastName == other.lastName
            }
        }
        override def toString:String = s"Employee($firstName, $lastName)"
      }
      
      object Employee {
        implicit val employeeOrderingByLastName: Ordering[Employee] = new Ordering[Employee] {
          override def compare(x: Employee, y: Employee): Int = {
            x.lastName.compareToIgnoreCase(y.lastName)
          }
        }

        implicit val employeeOrderingByFirstName: Ordering[Employee] = new Ordering[Employee] {
          override def compare(x: Employee, y: Employee): Int = {
            x.firstName.compareToIgnoreCase(y.firstName)
          }
        }
      }

      import Employee.employeeOrderingByLastName

      val sortedList = List(new Employee("Eric", "Clapton"),
        new Employee("Jeff", "Beck"),
        new Employee("Ringo", "Starr"),
        new Employee("Paul", "McCartney"),
        new Employee("John", "Lennon"),
        new Employee("George", "Harrison")).sorted


      sortedList should be (List(new Employee("Jeff", "Beck"),
        new Employee("Eric", "Clapton"),
        new Employee("George", "Harrison"),
        new Employee("John", "Lennon"),
        new Employee("Paul", "McCartney"),
        new Employee("Ringo", "Starr")))
    }
  }
}




