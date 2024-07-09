package jp.ukiba.koneko
package ko_html

import HtmlAst.{Elem, Attr}

class CharRefTests extends munit.FunSuite:
  test("text node"):
    assertEquals(HtmlParser().parseElem("""<div>&lt;</div>"""), Elem("div", children = Seq("<")))
    assertEquals(HtmlParser().parseElem("""<div> &apos; </div>"""), Elem("div", children = Seq("'")))
    assertEquals(HtmlParser().parseElem("""<div>(&copy;)</div>"""), Elem("div", children = Seq("(©)")))
    assertEquals(HtmlParser().parseElem("""<div>&lt; &gt;</div>"""), Elem("div", children = Seq("< >")))

  test("attribute values"):
    assertEquals(HtmlParser().parseElem("""<img alt="&amp;">"""), Elem("img", Seq(Attr("alt", "&"))))
