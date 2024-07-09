package jp.ukiba.koneko
package ko_html

import HtmlAst.{Elem, Attr, ArgCharMin, ArgCharMax}
import HtmlCodeGen.GenContext

import org.scalajs.dom

import scala.quoted.{Quotes, Expr, Type}
import scala.scalajs, scalajs.js
import scala.annotation.tailrec

/** For the browser and Node.js */
abstract private[ko_html] class HtmlCodeGenPlatform(args: Seq[Expr[Any]])(using Quotes):
  val quotes = summon[Quotes]
  import quotes.reflect.*

  def createElemTree(ast: Elem)(using ctx0: GenContext): Expr[Any] =
    given ctx: GenContext =
      if ast.name == "svg" then
        ctx0.copy(namespaceURI = Some(Expr("http://www.w3.org/2000/svg")))
      else
        ctx0

    val elemTypeRepr = elemTypeOf(ast.name)
    elemTypeRepr.asType match
      case '[elemType] =>
        '{
          val elem = ${createElem(ast)}.asInstanceOf[elemType]
          ${setAttrs('{ elem }.asInstanceOf[Expr[dom.Element]], elemTypeRepr, ast.attrs)}
          ${appendChildren('{ elem }.asInstanceOf[Expr[dom.Element]], ast.children)}
          elem
        }

  def createElem(ast: Elem)(using ctx: GenContext): Expr[dom.Element] =
    val options = ast.attrs.find(_.name == "is") match
      case Some(isAttr) => Some('{ new dom.ElementCreationOptions {
        val is = ${stringValueOf(attrValueOf(isAttr))}
      }})
      case None => None

    val elemName = Expr(ast.name) // no interpolation since it statically determines the return type
    ctx.namespaceURI match
      case Some(ns) =>
        options match
          case Some(options) => '{ dom.document.createElementNS($ns, $elemName, $options) }
          case None          => '{ dom.document.createElementNS($ns, $elemName) }
      case None =>
        options match
          case Some(options) => '{ dom.document.createElement($elemName, $options) }
          case None          => '{ dom.document.createElement($elemName) }

  def setAttrs(elem: Expr[dom.Element], elemTypeRepr: TypeRepr, attrs: Iterable[Attr]) = Expr.ofSeq:
    val setters = settersOf(elemTypeRepr)

    attrs.filter(_.name != "is").toSeq.map: attr =>
      val attrValue = attrValueOf(attr)

      val attrName = attr.name
      val setterName = s"${attrName}_="
      setters.find(_.name == setterName) match
        case Some(setter) =>
          /*
            scala 3.4.2: 
              setter.paramSymss.head.head.typeRef match
                case TypeRef(typeRepr, str) =>
            results in
              java.lang.ClassCastException: class dotty.tools.dotc.core.Names$SimpleName cannot be cast to class dotty.tools.dotc.core.Names$TypeName (dotty.tools.dotc.core.Names$SimpleName and dotty.tools.dotc.core.Names$TypeName are in unnamed module of loader sbt.internal.classpath.ClassLoaderCache$Key$CachedClassLoader @af9812a)
          */
          // trying to accept `onclick=${ev => ()}`
/*
          val attrValueTyped = setter.paramSymss.head.head.tree match
            case ValDef(name, typeTree, termOpt) =>
              val typeRepr = typeTree.tpe
              typeRepr.asType match
                case '[attrValueType] =>
                  //attrValue.asExprOf[attrValueType]
          val attrValueTyped = '{
            val x: Function1[dom.MouseEvent, ?] = ${attrValue.asExprOf[Function1[dom.MouseEvent, ?]]}
            x
          }
          setter.paramSymss.head.head.typeRef match
            case TypeRef(typeRepr, str) =>
              println(s"typeRepr = $typeRepr, str = $str")
*/

          //import js.*, js.Any.*
          //val searchResults = Implicits.search(attrValueTypeRepr)
          //println(s"searchResults = $searchResults")

          // apply scala.scalajs.js.Any
          // implicit def fromFunction1[T1, R](f: (T1) ⇒ R): scala.scalajs.js.Function1[T1, R]

          val attrValueConverted = convertAttrValue(attrValue, setter.typeRepr)
          elem.asTerm.select(setter.method).appliedToArgs(List(attrValueConverted.asTerm)).asExpr

        case None =>
          // call setAttribute
          '{ $elem.setAttribute(${Expr(attrName)}, ${stringValueOf(attrValue)}) }

  def convertAttrValue(value: Expr[Any], expected: TypeRepr): Expr[Any] =
    // Only js.Function will be called
    if expected <:< TypeReprs.js.Function1 then
      val typeRepr = value match // get the precise type
        case '{ $x: tpe } => TypeRepr.of[tpe]

      if typeRepr <:< TypeReprs.Function1 then
        val fromFunction1 = Selects.js.Any.fromFunction1.appliedToTypes(expected.typeArgs)
        fromFunction1.appliedToArgs(List(value.asTerm)).asExpr

      else
        value

    // Boolean: any value is true for disabled / checked
    else if expected <:< TypeReprs.Boolean then
      Expr(true)

    else
      value

  def appendChildren(elem: Expr[dom.Element], children: Seq[Elem | String])(using GenContext) = Expr.ofSeq:
    children.flatMap: child =>
      child match
        case child: Elem  =>
          Seq('{ $elem.appendChild(${ createElemTree(child).asExprOf[dom.Node] }) })

        case text: String =>
          interpolateArgs(text).map: expr =>
            // single Node
            if expr.isExprOf[dom.Node] then
              '{ $elem.appendChild(${expr.asExprOf[dom.Node]}) }

            // single String
            else if expr.isExprOf[String] then
              '{ $elem.appendChild(dom.document.createTextNode(${expr.asExprOf[String]})) }

            // Array of Nodes
            else if expr.isExprOf[Array[?]] then
              '{ ${expr.asExprOf[Array[dom.Node]]}.toList.map($elem.appendChild) }

            else
              // Seq/List of Nodes
              val typeRepr = expr match // get the precise type
                case '{ $x: tpe } => TypeRepr.of[tpe]
              if typeRepr <:< TypeReprs.IterableOnce then
                '{ ${expr.asExprOf[IterableOnce[dom.Node]]}.iterator.toSeq.map($elem.appendChild) }

              // anything else is converted to String
              else
                '{ $elem.appendChild(dom.document.createTextNode(${stringValueOf(expr)})) }

  def interpolateArgs(value: String): Seq[Expr[Any]] =
    if value.nonEmpty then
      @tailrec def loop(unconsumedFrom: Int, unconsumedUntil: Int, consumed: Seq[Expr[Any]]): Seq[Expr[Any]] =
        if unconsumedUntil < value.length then
          val ch = value(unconsumedUntil)
          if ArgCharMin <= ch && ch <= ArgCharMax then
            val generated = {
              if unconsumedFrom < unconsumedUntil then
                Seq(Expr(value.slice(unconsumedFrom, unconsumedUntil)))
              else
                Nil
            } :+ args(ch - ArgCharMin)
            loop(unconsumedUntil + 1, unconsumedUntil + 1, consumed ++ generated)

          else
            loop(unconsumedFrom, unconsumedUntil + 1, consumed)

        else
          if unconsumedFrom < unconsumedUntil then
            consumed :+ Expr(value.drop(unconsumedFrom))
          else
            consumed

      loop(0, 0, Nil)

    else
      Seq(Expr(""))

  def attrValueOf(attr: Attr): Expr[Any] =
    val attrValues = interpolateArgs(attr.value.getOrElse(""))
    if attrValues.size == 1 then
      attr.name.toLowerCase match
        case "disabled" =>
          val attrValue = stringValueOf(attrValues.head)
          // https://stackoverflow.com/a/24579932
          // TODO https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttribute#javascript
          '{
            // TODO is space false?
            $attrValue.isEmpty || $attrValue.equalsIgnoreCase("disabled")
          }

        case _ =>
          // https://html.spec.whatwg.org/multipage/syntax.html#attributes-2
          // Empty attribute syntax: The value is implicitly the empty string.
          attrValues.head

    else // concatenate as String
      attrValues.tail.foldLeft(stringValueOf(attrValues.head)): (s, t) =>
        '{ $s + ${stringValueOf(t)} }


  def elemTypeOf(elemName: String): TypeRepr =
    // taken from https://github.com/scala-js/scala-js-dom/tree/v2.8.0/dom/src/main/scala/org/scalajs/dom
    // and https://developer.mozilla.org/en-US/docs/Web/HTML/Element
    elemName.toLowerCase match
      case "a" => TypeRepr.of[dom.HTMLAnchorElement]
      case "area" => TypeRepr.of[dom.HTMLAreaElement]
      case "audio" => TypeRepr.of[dom.HTMLAudioElement]
      case "base" => TypeRepr.of[dom.HTMLBaseElement]
      case "blockquote" => TypeRepr.of[dom.HTMLQuoteElement]
      case "body" => TypeRepr.of[dom.HTMLBodyElement]
      case "br" => TypeRepr.of[dom.HTMLBRElement]
      case "button" => TypeRepr.of[dom.HTMLButtonElement]
      case "canvas" => TypeRepr.of[dom.HTMLCanvasElement]
      case "caption" => TypeRepr.of[dom.HTMLTableCaptionElement]
      case "datalist" => TypeRepr.of[dom.HTMLDataListElement]
      case "del" => TypeRepr.of[dom.HTMLModElement]
      case "dl" => TypeRepr.of[dom.HTMLDListElement]
      case "dialog" => TypeRepr.of[dom.HTMLDialogElement]
      case "div" => TypeRepr.of[dom.HTMLDivElement]
      case "embed" => TypeRepr.of[dom.HTMLEmbedElement]
      case "fieldset" => TypeRepr.of[dom.HTMLFieldSetElement]
      case "form" => TypeRepr.of[dom.HTMLFormElement]
      case "h1" => TypeRepr.of[dom.HTMLHeadingElement]
      case "h2" => TypeRepr.of[dom.HTMLHeadingElement]
      case "h3" => TypeRepr.of[dom.HTMLHeadingElement]
      case "h4" => TypeRepr.of[dom.HTMLHeadingElement]
      case "h5" => TypeRepr.of[dom.HTMLHeadingElement]
      case "h6" => TypeRepr.of[dom.HTMLHeadingElement]
      case "head" => TypeRepr.of[dom.HTMLHeadElement]
      case "hr" => TypeRepr.of[dom.HTMLHRElement]
      case "html" => TypeRepr.of[dom.HTMLHtmlElement]
      case "iframe" => TypeRepr.of[dom.HTMLIFrameElement]
      case "img" => TypeRepr.of[dom.HTMLImageElement]
      case "input" => TypeRepr.of[dom.HTMLInputElement]
      case "ins" => TypeRepr.of[dom.HTMLModElement]
      case "label" => TypeRepr.of[dom.HTMLLabelElement]
      case "legend" => TypeRepr.of[dom.HTMLLegendElement]
      case "li" => TypeRepr.of[dom.HTMLLIElement]
      case "link" => TypeRepr.of[dom.HTMLLinkElement]
      case "map" => TypeRepr.of[dom.HTMLMapElement]
      case "menu" => TypeRepr.of[dom.HTMLMenuElement]
      case "meta" => TypeRepr.of[dom.HTMLMetaElement]
      case "object" => TypeRepr.of[dom.HTMLObjectElement]
      case "ol" => TypeRepr.of[dom.HTMLOListElement]
      case "optgroup" => TypeRepr.of[dom.HTMLOptGroupElement]
      case "option" => TypeRepr.of[dom.HTMLOptionElement]
      case "p" => TypeRepr.of[dom.HTMLParagraphElement]
      case "param" => TypeRepr.of[dom.HTMLParamElement]
      case "pre" => TypeRepr.of[dom.HTMLPreElement]
      case "progress" => TypeRepr.of[dom.HTMLProgressElement]
      case "q" => TypeRepr.of[dom.HTMLQuoteElement]
      case "script" => TypeRepr.of[dom.HTMLScriptElement]
      case "select" => TypeRepr.of[dom.HTMLSelectElement]
      case "source" => TypeRepr.of[dom.HTMLSourceElement]
      case "span" => TypeRepr.of[dom.HTMLSpanElement]
      case "style" => TypeRepr.of[dom.HTMLStyleElement]
      case "td" | "th" => TypeRepr.of[dom.HTMLTableCellElement]
      case "col" => TypeRepr.of[dom.HTMLTableColElement]
      case "table" => TypeRepr.of[dom.HTMLTableElement]
      case "tr" => TypeRepr.of[dom.HTMLTableRowElement]
      case "thead" | "tbody" | "tfoot" => TypeRepr.of[dom.HTMLTableSectionElement]
      case "template" => TypeRepr.of[dom.HTMLTemplateElement]
      case "textarea" => TypeRepr.of[dom.HTMLTextAreaElement]
      case "title" => TypeRepr.of[dom.HTMLTitleElement]
      case "track" => TypeRepr.of[dom.HTMLTrackElement]
      case "ul" => TypeRepr.of[dom.HTMLUListElement]
      case "video" => TypeRepr.of[dom.HTMLVideoElement]
      case _ => TypeRepr.of[dom.HTMLElement]

  case class Setter(name: String, method: Symbol, typeTree: TypeTree):
    def typeRepr: TypeRepr = typeTree.tpe

  def settersOf(elemTypeRepr: TypeRepr): Iterable[Setter] = 
    elemTypeRepr.typeSymbol.methodMembers.filter(method =>
        method.name.endsWith("_=") &&
        method.paramSymss.size == 1 &&
        method.paramSymss.head.size == 1)
        .groupBy(_.name).map { (name, methods) =>
      val method =
        if methods.size > 1 then
          // choose style_=(value: String)
          // over   style_=(value: CSSStyleDeclaration)
          val stringParamMethod = methods.find: method =>
            method.paramSymss.head.head.tree match // scala 3.4.2: param.info is experimental
              case ValDef(name, typeTree, termOpt) => typeTree.tpe =:= TypeReprs.String
          stringParamMethod match
            case Some(method) => method
            case None => throw new IllegalStateException
                (s"${elemTypeRepr.typeSymbol.name}.$name: overloaded but there is no version that takes a String")
        else
          methods.head

      val typeTree = method.paramSymss.head.head.tree match
        case ValDef(name, typeTree, termOpt) => typeTree
      Setter(name, method, typeTree)
    }

  object TypeReprs:
    val Boolean      = TypeRepr.of[Boolean]
    val String       = TypeRepr.of[String]
    val IterableOnce = TypeRepr.of[IterableOnce[?]]
    val Function1    = TypeRepr.of[Function1[?, ?]] // not subclass of Predef.Function

    object js:
      val Function1 = TypeRepr.of[scalajs.js.Function1[?, ?]] // subclass of js.Function
      val Any       = TypeRepr.of[scalajs.js.Any]

  object Symbols:
    object js:
      object Any:
        // TODO check it is unique
        val fromFunction1 = TypeRepr.of[scalajs.js.Any.type].typeSymbol.methodMember("fromFunction1").head

  object Selects:
    object js:
      object Any:
        val term = '{ scalajs.js.Any }.asTerm
        val fromFunction1 = Select(term, Symbols.js.Any.fromFunction1)

  def stringValueOf(expr: Expr[Any]): Expr[String] =
    if expr.isExprOf[String] then
      expr.asExprOf[String]
    else
      '{ String.valueOf($expr) }
