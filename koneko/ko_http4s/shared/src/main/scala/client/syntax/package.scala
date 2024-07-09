package jp.ukiba.koneko
package ko_http4s
package client

package object syntax:
  object all extends AllSyntax
  trait AllSyntax
    extends ClientSyntax
    with KoHttpResponseSyntax

  object client extends ClientSyntax
  object httpResponse extends KoHttpResponseSyntax
