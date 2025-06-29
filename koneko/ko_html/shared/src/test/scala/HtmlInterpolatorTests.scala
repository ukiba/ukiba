package jp.ukiba.koneko
package ko_html

class HtmlInterpolatorTests extends munit.FunSuite:
  test("div"):
    html"<div>Hello 世界</div>"

    val x = "世界"
    //html"<div>Hello $x</div>"

    val y = 3
    assertEquals(html"<div>${x}A${y}B9$x</div>".outerHTML, "<div>世界A3B9世界</div>")

/* Only compiles in js
  test("input"):
    import org.scalajs.dom
    assert(html"<input>".isInstanceOf[dom.HTMLInputElement])
*/

  test("trailingSpaces"):
    assertEquals(html"<div></div> ".outerHTML, "<div></div>")

  test("nested element"):
    assertEquals(html"<p><strong>bold texts</strong></p>".outerHTML, "<p><strong>bold texts</strong></p>")

  test("mixed content"):
    assertEquals(html"<p>a<b>b</b>c<i></i></p>".outerHTML, "<p>a<b>b</b>c<i></i></p>")
    assertEquals(html"<h1 style=s0>left<b>bold</b>right</h1>".outerHTML, """<h1 style="">left<b>bold</b>right</h1>""") // FIXME

  test("self-closing"):
    assertEquals(html"<div/>".outerHTML, "<div></div>")

  test("void elements"):
    assertEquals(html"<input>".outerHTML, "<input>")

  test("elemArg1"):
    val text = html"<input type=text>"
    val checkbox = html"<input type=checkbox>"
    assertEquals(html"""<div style="color: #000000">text: $text<br>checkbox: $checkbox</div>""".outerHTML,
        """<div style="color: rgb(0, 0, 0);">text: <input type="text"><br>checkbox: <input type="checkbox"></div>""")

  // attribute

  test("empty"):
    assertEquals(html"<input disabled>"  .outerHTML, """<input disabled="">""")
    assertEquals(html"<input disabled/>" .outerHTML, """<input disabled="">""")
    assertEquals(html"<input disabled />".outerHTML, """<input disabled="">""")

  test("unquoted"):
    assertEquals(html"<input value=yes>"    .outerHTML, """<input>""") // FIXME
    assertEquals(html"<input value=yes/>"   .outerHTML, """<input>""") // FIXME
    assertEquals(html"<input value = yes />".outerHTML, """<input>""") // FIXME

  test("single-quoted"):
    assertEquals(html"<input type='checkbox'>"    .outerHTML, """<input type="checkbox">""")
    assertEquals(html"<input type='checkbox'/>"   .outerHTML, """<input type="checkbox">""")
    assertEquals(html"<input type = 'checkbox' />".outerHTML, """<input type="checkbox">""")

  test("double-quoted"):
    assertEquals(html"""<input name="be evil">"""    .outerHTML, """<input name="be evil">""")
    assertEquals(html"""<input name="be evil"/>"""   .outerHTML, """<input name="be evil">""")
    assertEquals(html"""<input name = "be evil" />""".outerHTML, """<input name="be evil">""")
