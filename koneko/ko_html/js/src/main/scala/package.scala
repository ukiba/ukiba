package jp.ukiba.koneko
package ko_html

import org.scalajs.dom

import scala.math.{max, ceil}

extension (node: dom.Node)
  def appendChildren(childNodes: Iterable[dom.Node]): Unit =
    childNodes.foreach: childNode =>
      node.appendChild(childNode)

  def appendChildren(childNodes: dom.Node*): Unit = appendChildren(childNodes)

  def removeChildren(childNodes: dom.Node*): Unit =
    childNodes.foreach: childNode =>
      node.removeChild(childNode)

  def removeAllChildren(): Unit =
    removeChildren(node.childNodes.toSeq*)

extension (elem: dom.HTMLElement)
  def childHTMLElements: Seq[dom.HTMLElement] =
    elem.children.toSeq.collect:
      case htmlElem: dom.HTMLElement => htmlElem

extension (nodeList: dom.NodeList[dom.Node])
  def iterator: Iterator[dom.Node] = new Iterator[dom.Node] {
    var i = 0
    def hasNext = i < nodeList.length
    def next() =
      val ret = nodeList(i)
      i += 1
      ret
  }

  def find(f: dom.Node => Boolean): Option[dom.Node] = iterator.find(f)
  def exists(f: dom.Node => Boolean): Boolean = iterator.exists(f)

// find, exists are already provided for dom.HTMLCollection[dom.Element]

extension (elems: Iterable[dom.HTMLElement])
  def maxWidth: Double = elems.foldLeft(0d): (result, elem) =>
    val width = elem.getBoundingClientRect().width // offsetWidth is integer (rounded down)
    max(width, result)

  def setWidth(width: Double): Unit =
    // not rounding up sometimes reduces the width
    val widthPx = f"${ceil(width)}%fpx" // s"" can result in exponential representation like 1.0E-4
    elems.foreach: elem =>
      elem.style.width = widthPx

  // not possble with CSS flex / grid
  def growWidthToWidest: Unit =
    setWidth(maxWidth)

/*
extension (namedNodeMap: dom.NamedNodeMap)
  def iterator: Iterator[dom.Attr] = new Iterator[dom.Attr] {
    var i = 0
    def hasNext = i < namedNodeMap.length
    def next() =
      val ret = namedNodeMap(i)
      i += 1
      ret
  }

  def find(f: dom.Attr => Boolean): Option[dom.Attr] = iterator.find(f)
  def exists(f: dom.Attr => Boolean): Boolean = iterator.exists(f)
*/
