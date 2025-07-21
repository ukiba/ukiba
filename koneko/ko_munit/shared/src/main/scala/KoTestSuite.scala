package jp.ukiba.koneko
package ko_munit

import munit.{FunSuite, Location, TestOptions, Compare}

/** A possible solution to [Grouping tests](https://github.com/scalameta/munit/issues/633) */
trait KoTestSuite extends FunSuite: // trait can extend abstract class
  /** The names of the nested tests */
  type NestedTestNames = List[String]
  given NestedTestNames = Nil

  /** Prefixes test names */
  def nest(name: String)(body: => NestedTestNames ?=> Any)(using nestedTestNames: NestedTestNames): Unit =
    given NestedTestNames = name :: nestedTestNames
    body

  /** Used instead of `test(name: String)(body: => Any)(implicit loc: Location)` */
  def test(name: String)(body: => Any)(using nestedTestNames: NestedTestNames, loc: Location): Unit =
    test(TestOptions(name))(body)

  /** Used instead of `test(options: TestOptions)(body: => Any)(implicit loc: Location)` */
  def test(options: TestOptions)(body: => Any)(using nestedTestNames: NestedTestNames): Unit =
    val nestedName = (nestedTestNames.reverse :+ options.name).mkString(".")
    super.test(new TestOptions(nestedName, options.tags, options.location))(body)

  /** Equivalent to utest's assertMatch */
  inline def assertMatch[A](obtained: A)(pf: PartialFunction[A, Unit])(using Location): Unit =
    pf.applyOrElse(obtained, _ => fail(s"Obtained value does not match the pattern: $obtained"))

  // munit 1.1.1
  // avoid the following compile error
  //
  //     [error]    |   Can't compare these two types:
  //     [error]    |     First type:  Long
  //     [error]    |     Second type: Int
  given Compare[Long, Int] with
    def isEqual(obtained: Long, expected: Int) = summon[Compare[Long, Long]].isEqual(obtained, expected)

  given Compare[Short, Int] with
    def isEqual(obtained: Short, expected: Int) = summon[Compare[Int, Int]].isEqual(obtained, expected)

  given Compare[Byte, Int] with
    def isEqual(obtained: Byte, expected: Int) = summon[Compare[Int, Int]].isEqual(obtained, expected)
