package jp.ukiba.koneko
package ko_html

import HtmlAst.{Elem, Attr}

class CommentRefTests extends munit.FunSuite:
  test("comment"):
    //assertEquals(HtmlParser().parseElem("""<div><!-- --></div>"""), Elem("div"))
    assertEquals(HtmlParser().comment.parseAll("""<!---->"""), Right((())))
    assertEquals(HtmlParser().comment.parseAll("""<!-- -->"""), Right((())))
    assertEquals(HtmlParser().comment.parseAll("""<!-- - -->"""), Right((())))
    assertEquals(HtmlParser().comment.parseAll("""<!-- -- -->"""), Right((())))
