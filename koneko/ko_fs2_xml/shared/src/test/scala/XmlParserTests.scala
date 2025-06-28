package jp.ukiba.koneko
package ko_fs2
package xml

import ko_munit.KoCatsEffectSuite

import fs2.data.xml
import fs2.Stream
import cats.syntax.all.*

class XmlParserTests extends KoCatsEffectSuite:
  import XmlParser.*

  nest("Person"):
    case class Person(name: String, age: Int)
    object Person:
      def parser: Parser[F, Person] =
        for
          _    <- startTag[F]("person")
          name <- textOnlyTag[F]("name")
          age  <- textOnlyTag[F]("age")
          _    <- endTag[F]("person")
        yield Person(name, age.toInt)

    case class Persons(person: Seq[Person])
    object Persons:
      def parser: Parser[F, Persons] =
        for
          _       <- startTag[F]("persons")
          persons <- Person.parser.repUntilEndTag("persons")
        yield Persons(persons)

    test("<person>"):
      val xmlStr = """
        |<?xml version="1.1"?>
        |<person>
        |  <name>Taro</name>
        |  <age>10</age>
        |</person>
      """.stripMargin.trim
      for
        result <- Stream.emits[F, Char](xmlStr)
            .through(xml.events())
            .through(xml.namespaceResolver)
            .through(xml.referenceResolver()) // e.g. "&amp;" to "&"
            .through(xml.normalize) // e.g. merge adjacent texts
            .through(XmlTextTrimmer.pipe)
            //.through(XmlEventLog.pipe)
            .through(doc(Person.parser).pipe)
            .compile./*onlyOrError*/toVector
      yield
        println(s"result = $result")

    test("<persons>"):
      val xmlStr = """
        |<?xml version="1.1"?>
        |<persons>
        |  <person>
        |    <name>Taro</name>
        |    <age>10</age>
        |  </person>
        |  <person>
        |    <name>Jiro</name>
        |    <age>8</age>
        |  </person>
        |</persons>
      """.stripMargin.trim
      for
        result <- Stream.emits[F, Char](xmlStr)
            .through(xml.events())
            .through(xml.namespaceResolver)
            .through(xml.referenceResolver()) // e.g. "&amp;" to "&"
            .through(xml.normalize) // e.g. merge adjacent texts
            .through(XmlTextTrimmer.pipe)
            //.through(XmlEventLog.pipe)
            .through(doc(Persons.parser).pipe)
            .compile./*onlyOrError*/toVector
      yield
        println(s"result = $result")
