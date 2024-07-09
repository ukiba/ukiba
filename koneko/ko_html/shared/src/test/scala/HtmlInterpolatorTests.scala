package jp.ukiba.koneko
package ko_html

class HtmlInterpolatorTests extends munit.FunSuite:
  test("div"):
    html"<div>Hello 世界</div>"

    val x = "世界"
    //html"<div>Hello $x</div>"

    val y = 3
    html"<div>${x}A${y}B9$x</div>"

/* Only compiles in js
  test("input"):
    import org.scalajs.dom
    assert(html"<input>".isInstanceOf[dom.HTMLInputElement])
*/

  test("trailingSpaces"):
    html"<div></div> "

  test("nested element"):
    html"<p><strong>bold texts</strong></p>"

  test("mixed content"):
    html"<p>a<b>b</b>c<i></i></p>"
    html"<h1 style=s0>left<b>bold</b>right</h1>"

  test("self-closing"):
    html"<div/>"

  test("void elements"):
    html"<input>"

  test("elemArg1"):
    val text = html"<input type=text>"
    val checkbox = html"<input type=checkbox>"
    val div = html"""<div style="color: #000000">text: $text<br>checkbox: $checkbox</div>"""

  // attribute

  test("empty"):
    html"<input disabled>"
    html"<input disabled/>"
    html"<input disabled />"

  test("unquoted"):
    html"<input value=yes>"
    html"<input value=yes/>"
    html"<input value = yes />"

  test("single-quoted"):
    html"<input type='checkbox'>"
    html"<input type='checkbox'/>"
    html"<input type = 'checkbox' />"

  test("double-quoted"):
    html"""<input name="be evil">"""
    html"""<input name="be evil"/>"""
    html"""<input name = "be evil" />"""
