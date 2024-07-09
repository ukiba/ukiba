package jp.ukiba.koneko
package ko_http4s

package object syntax:
  object all extends AllSyntax
  trait AllSyntax
    extends HttpRoutesSyntax

  object httpRoutes extends HttpRoutesSyntax
