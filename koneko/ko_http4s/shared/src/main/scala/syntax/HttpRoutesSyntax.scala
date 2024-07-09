package jp.ukiba.koneko
package ko_http4s
package syntax

import org.http4s.{HttpRoutes, Request, Response, Uri}, Uri.Path
import org.http4s.syntax.*
import cats.data.{Kleisli, OptionT}
import cats.{Applicative, Monad}

trait HttpRoutesSyntax:
  extension [F[_]](routes: HttpRoutes[F])
    /** Rewrites the request paths */
    def mapPath(func: PartialFunction[Path, Path]): HttpRoutes[F] = Kleisli: req =>
      func.lift(req.pathInfo) match
        case Some(modifiedPath) => routes(req.withPathInfo(modifiedPath))
        case None               => routes(req)

    /**
     * Rewrites the request paths with trailing slash to `index.html`.
     * `FileService` automatically looks up `index.html` but `ResourceService` doesn't
     */
    def mapIndexHtml: HttpRoutes[F] = mapPath:
      case path if (path.endsWithSlash || path.isEmpty) => // "/" is empty
        path.addSegment(Path.Segment("index.html"))

  extension [F[_]: Monad](routes: HttpRoutes[F])
    /** Serve the given response on a given condition */
    def fixedResponseWhen(pred: Request[F] => Boolean, resp: Response[F]): HttpRoutes[F] = Kleisli: req =>
      routes.run(req).orElse:
        if pred(req) then
          OptionT.some(resp)
        else
          routes(req)

    /** Serve `NotFound` on a given condition */
    def notFoundWhen(pred: Request[F] => Boolean): HttpRoutes[F] = Kleisli: req =>
      routes.run(req).orElse:
        if pred(req) then
          OptionT.liftF(Response.notFoundFor(req))
        else
          routes(req)

    /**
     * Serve a fixed path for the unhundled requests.
     * Intended to be used instead of `notFound` for SPA that returns the fixed HTML on any path.
     */
    def fallbackPath(path: Path): Kleisli[F, Request[F], Response[F]] = Kleisli: req =>
      routes.run(req).orElse:
        val fallbackUri = req.uri.withPath(path)
        routes.run(req.withUri(fallbackUri))
      .getOrElse(Response.notFound)

    def fallbackPath(path: String): Kleisli[F, Request[F], Response[F]] = fallbackPath(Path.unsafeFromString(path))
