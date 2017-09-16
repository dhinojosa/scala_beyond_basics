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

      //val invisibleMap = {Class[Int]:100}

      implicit val rate = 100
      //bunch of code

      def calcPayment(hours: Int)(implicit r: Int) = hours * r

      calcPayment(50) should be(5000)
    }

    it("""will allow you to place something manually, if you want to override the implicit value""".stripMargin) {

      implicit val rate = 100
      //bunch of code

      def calcPayment(hours: Int)(implicit r: Int) = hours * r

      calcPayment(50)(150) should be(7500)
    }

    it(
      """will gripe at compile time if there are two implicit bindings of the same type.  It's
        |  worth noting that what Scala doing are compile time tricks for implicit. One strategy is to
        |  wrap a value in a type to avoid conflict""".stripMargin) {

      case class Rate(value: Int)
      case class Age(value: Int)

      implicit val rate = Rate(100)
      implicit val age = Age(40)

      def calcPayment(hours: Int)(implicit r: Rate) = r match {
        case Rate(x) => hours * x
        case _ => 0
      }

      calcPayment(50) should be(5000)
    }


    it(
      """test with implicits with type alias? No we need a different type""".stripMargin) {

      type Rate = Int
      type Age = Int

      implicit val rate: Rate = 100
      implicit val age: Age = 40

      def calcPayment(hours: Int)(implicit r: Rate) = hours * r

      //calcPayment(50) should be(5000)
    }


    it(
      """is really used to bind services that require something and
        |  you don't particularly need to inject everywhere explicitly, in this
        |  case let's discuss Future[+T]""".stripMargin) {

      import scala.concurrent._

      implicit val executionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(3))

      val f = Future.apply {
        Thread.sleep(2000)
        100 + 40
      }

      f.foreach(a => println(a))
    }


    it( """can bring up any implicit directly by merely calling up implicitly""") {
      case class IceCream(name: String)
      case class Scoops(num: Int, flavor: IceCream)
      implicit val flavorOfDay = IceCream("Springtime Bunny Chocolate")

      def orderIceCream(num: Int) = {
        Scoops(num, implicitly)
      }

      orderIceCream(3) should be(Scoops(3, flavorOfDay))
    }

    it("definition of closure") {
      class Foo(x: Int) {
        def bar = (y: Int) => x + y
      }

      val foo = new Foo(10)
      val fun = foo.bar _

      fun()(12) should be(22)
    }


    it(
      """the implicit group parameter list, can contain more than one parameter, but
        |  needs to be in the same implicit parameter group""".stripMargin) {
      implicit val bonus = 5000
      implicit val currency = "Euro"
      implicit val vacation: Option[Int] = Some(100)

      def calcYearRate(amount: Int)(implicit b: Int, c: String, d: Option[Int]) = amount + b + " " + c

      calcYearRate(50000) should be("55000 Euro")
    }

    it( """can also be replaced with default parameters, choose accordingly""") {
      def calcYearRate(amount: Int, b: Int = 5000, c: String = "Euro") = amount + b + " " + c

      calcYearRate(50000) should be("55000 Euro")
    }


    it("""Christopher A. Question: List[String] and List[Double]""") {
      implicit val listOfString = List("Foo", "Bar")
      implicit val listOfInt = List(1, 3, 4, 5)

      def foo(i: Int)(implicit col: List[String]) = col.size * i

      foo(4) should be(8)
    }


    it(
      """can be used for something like what Ruby has called
        |  monkey patching or Groovy calls mopping where we can add functionality to
        |  a class that we don't have access to, like isOdd/isEven
        |  in the Int class.  This is what we call implicit wrappers.
        |  First we will use a conversion method.""".stripMargin) {

      import scala.language.implicitConversions

      class IntWrapper(x: Int) {
        def isOdd: Boolean = x % 2 != 0

        def isEven: Boolean = !isOdd
      }

      implicit def int2IntWrapper(x: Int): IntWrapper = new IntWrapper(x)

      10.isOdd should be(false)
      11.isOdd should be(true)
      11.isEven should be(false)
      11.isOdd should be(true)
    }


    it( """Implicit wrappers can be created using a function and is often easier to mental map.""".stripMargin) {
      import scala.language.implicitConversions

      class IntWrapper(x: Int) {
        def isOdd: Boolean = x % 2 != 0

        def isEven: Boolean = !isOdd
      }

      //[Int => IntWrapper, int2IntWrapper]
      implicit val int2IntWrapper = (x: Int) => new IntWrapper(x)

      10.isOdd should be(false)
      11.isOdd should be(true)
      11.isEven should be(false)
      11.isOdd should be(true)
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

      10.isOdd should be(false)
      11.isOdd should be(true)
      11.isEven should be(false)
      11.isOdd should be(true)
    }

    it(
      """can also convert things to make it fit into a particular API,
        | this is called implicit conversion,
        | in this scenario we will use a method""".stripMargin) {

      import scala.language.implicitConversions

      sealed abstract class Currency
      case class Dollar(amt: Int) extends Currency
      case class Yen(amt: Int) extends Currency

      implicit def int2Dollar(x: Int): Dollar = Dollar(x)

      implicit def int2Yen(x: Int): Yen = Yen(x)

      def addDollars(x: Dollar, y: Dollar): Dollar = Dollar(x.amt + y.amt)

      def addYen(x: Yen, y: Yen): Yen = Yen(x.amt + y.amt + 100)

      addDollars(100, 150) should be(Dollar(250))
      addYen(100, 150) should be(Yen(350))
    }

    it(
      """can also convert things to make it fit into a particular API,
        | this is called implicit conversion,
        | in this scenario we will use a function""".stripMargin) {
      import scala.language.implicitConversions

      sealed abstract class Currency
      case class Dollar(amt: Int) extends Currency
      case class Yen(amt: Int) extends Currency

      implicit val int2Dollar = (x: Int) => Dollar(x)
      implicit val int2Yen = (x: Int) => Yen(x)

      def addDollars(x: Dollar, y: Dollar): Dollar = Dollar(x.amt + y.amt)

      def addYen(x: Yen, y: Yen): Yen = Yen(x.amt + y.amt + 100)

      addDollars(100, 150) should be(Dollar(250))
      addYen(100, 150) should be(Yen(350))
    }

    it(
      """is done automatically in Scala because what is inside of scala.Predef, for example,
        |  it explains how be can set a scala.Float , and there is java.lang.Float,
        |  java primitive float.
        |  We can investigate this by looking at
        |  the documentation.""".stripMargin) {

      val f: scala.Float = 400.3f
      val f2: scala.Float = 401.9f
      val result = java.lang.Math.min(f, f2)
      result should be(400.3f)
    }
  }

  object MyPredef {

    import scala.language.implicitConversions

    implicit class IntWrapper(x: Int) {
      def isOdd: Boolean = x % 2 != 0

      def isEven: Boolean = !isOdd
    }

  }

  describe("Locating implicits recipes") {
    it(
      """has a common way, to store that particular implicit
        |  recipe in an object that makes should make
        |  sense and then import that object""".stripMargin) {

      import MyPredef._
      10.isOdd should be(false)
    }

    it( """can also use a companion object to store any implicit recipes""".stripMargin) {
      class Artist(val firstName: String, val lastName: String)
      object Artist {

        import scala.language.implicitConversions

        implicit def tuple2Artist(t: (String, String)): Artist = new Artist(t._1, t._2)
      }

      def playPerformance(a: Artist) = s"${a.firstName} ${a.lastName} is playing tonight"

      val t2 = ("Celine", "Dion")
      playPerformance(t2)
    }

    it( """can also use a package object to store some of these implicits""") {
      def numItems(list:List[String]) = list.reduce((total, next) => next + total)

      val t = (3, "Whoa")
      numItems(t) should be ("WhoaWhoaWhoa")
    }

    it("""can use JavaConverters to convert a collection in Java to Scala and vice versa""") {

      import scala.collection.JavaConverters._
      import java.time.ZoneId

      ZoneId.getAvailableZoneIds.asScala.toSet.filter(s => s.startsWith("America"))
    }
  }

  describe("View Bounds are used to ensure that there is a particular recipe for a certain type") {
    it(
      """Uses <% inside of a parameterized type declaration to determine if there is a conversion available
        | then within you can treat an object as an object of that type. It is unorthodox, and has since been
        | deprecated.""".stripMargin) {

      class Employee(val firstName:String, val lastName:String)
      import scala.language.implicitConversions

      implicit def str2Employee(s:String):Employee = {
          s.split(" ").toList match {
            case Nil => new Employee("John", "Anon")
            case fn :: Nil => new Employee(fn, "Anon")
            case fn :: ln :: Nil => new Employee(fn, ln)
            case fn :: ln :: _ => new Employee(fn, ln)
          }
      }

      def hireEmployee[A <% Employee](a:A):String = {
        s"Hired ${a.firstName}!"
      }

      hireEmployee("Bob Henderson") should be ("Hired Bob!")
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
           def log(t:T):String
        }

        implicit val employee2String:Loggable[Employee] = new Loggable[Employee] {
          override def log(t: Employee): String = s"!${t.firstName} ${t.lastName}!"
        }

        class Employee(val firstName:String, val lastName:String)

        def toStringz[T:Loggable](t:T):String = {
           val lg = implicitly[Loggable[T]]
           lg.log(t)
        }

        toStringz(new Employee("Sarah", "Thompson")) should be ("!Sarah Thompson!")
    }
  }

  describe(
    """Type Constraints are used to ensure that a particular method can run
      | if a particular generic is of a certain type, this is typically used for
      | one method""".stripMargin) {

    it(
      """uses one operator, =:= which is actually the full type =:=[A,B] that
        |  will to see if something is of the same type""".stripMargin) {

       class MyTuple2[A, B](val a:A, val b:B) {
         def first: A = a
         def second:B = b
         def toList(implicit ev: A =:= B):List[A] = List(a, b).asInstanceOf[List[A]]
       }

       val myTuple2 = new MyTuple2(4.0, 5.0)
       val list = myTuple2.toList
       list.apply(1)
    }

    it("""uses the operator, <:< which will test if A is a subtype of B""") {
      pending
    }
  }

  describe("Getting around Erasure Using TypeTags") {
    it("used to use Manifest but now uses a type tag to retrieve what is erased") {

      import scala.reflect.runtime.universe._

      def matchList[A](list:List[A])(implicit tt:TypeTag[A]):String = {
         tt.tpe match {
           case y if y =:= typeOf[Int] => "List of Int"
           case x if x =:= typeOf[String] => "List of String"
           case z if z =:= typeOf[Double] => "List of Double"
           case _ => "List of Unknown"
         }
      }

      matchList(List("Foo", "Bar", "Baz")) should be ("List of String")
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
       pending
    }
  }
}
