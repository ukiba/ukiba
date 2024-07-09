package jp.ukiba.koneko
package ko_cats
package effect

package object syntax:
  object all extends AllSyntax
  trait AllSyntax
    extends FutureSyntax
    with    PromiseSyntax

  object future  extends FutureSyntax
  object promise extends PromiseSyntax

