package jp.ukiba.koneko
package ko_http4s
package client

import org.http4s.{Request, Method, HttpVersion}
import org.http4s.Uri, Uri.{Scheme, Authority, UserInfo, Host, RegName, Path, Fragment}
import org.http4s.{Query, QueryParam, QueryParamKeyLike, QueryParamEncoder}
import org.http4s.{Headers, Header, Credentials, AuthScheme, BasicCredentials}
import org.http4s.{EntityDecoder, EntityEncoder, MediaType, UrlForm}
import org.http4s.headers.{Accept, Authorization, `Content-Type`}
import org.http4s.multipart.Multipart
import fs2.Stream
import cats.effect.Concurrent
import cats.syntax.all.*
import scodec.bits.ByteVector
import com.comcast.ip4s
import org.typelevel.vault.{Vault, Key}
import org.typelevel.ci.CIString

import scala.annotation.targetName

/**
  A combination of an HTTP request and the corresponding typed response decoder.

  Although it is primary intended to be used with [[KoHttpClient]],
  this class can be used independently.
 
  {{{
    val api = KoHttpRequest[F].withUri(uri"http://api.zippopotam.us/us/")
    EmberClientBuilder.default[F].build.use: client =>
      import jp.ukiba.koneko.ko_http4s.client.syntax.all.* // for client.run and toBody
      import org.http4s.circe.CirceEntityDecoder.*         // to decode the response body as JSON
      import io.circe.generic.auto.*                       // to map JSON to a case class
      client.run(api.GET(path"12561").acceptJson[PostCode]).decodeSuccess.toBody
  }}}

  1. [[KoHttpRequest$.apply KoHttpRequest[F]]] constructs an instance with
      1. the default (almost empty) request, and
      2. the expected response type being `ByteVector`
  2. [[withUri]] method gives the base URI
  3. [[GET]] sets the request method and appends a path to the base URI
  4. [[acceptJson]] method
      1. adds a header: `Accept: application/json`, and
      2. changes the expected response type to be the given type
  5. [[jp.ukiba.koneko.ko_http4s.client.syntax.ClientSyntax.run client.run]]
      1. sends the request, and
      2. starts receiving the response
      3. with the minimal logging
  6. [[KoHttpResponse.decodeSuccess decodeSuccess]] method
      1. raises an error if the response status is not successful
      2. decodes the response body to the expected type, releasing the `Resource`
  7. [[jp.ukiba.koneko.ko_http4s.client.syntax.KoHttpResponseSyntax.toBody toBody]] extracts the decoded body

  @tparam A The expected type of response body
  @param underlying The underlying request
  @param decoder The decoder for `A`
*/
case class KoHttpRequest[F[_]: Concurrent, A](
  underlying: Request[F],
  decoder: EntityDecoder[F, A],
  decodeStrict: Boolean = false, // TODO shuld this be true?
) extends KoHttpRequest.UnderlyingOp[F, KoHttpRequest[F, A]]:
  import KoHttpRequest.*

  def withUnderlying(underlying: Request[F]): KoHttpRequest[F, A] = copy(underlying = underlying)

  def withDecoder[B](decoder: EntityDecoder[F, B]): KoHttpRequest[F, B] = copy(decoder = decoder)
  def acceptJson[B](using decoder: EntityDecoder[F, B]): KoHttpRequest[F, B] =
    withHeader(Accept(MediaType.application.json)).withDecoder(decoder)
  def acceptString: KoHttpRequest[F, String] =
    withHeader(Accept(`text/plain;charset=utf-8`)).withDecoder(EntityDecoder.text)
  def acceptByteVector: KoHttpRequest[F, ByteVector] =
    withHeader(Accept(MediaType.application.`octet-stream`)).withDecoder(EntityDecoder.byteVector)

object KoHttpRequest:
  // lower cases as in https://www.rfc-editor.org/rfc/rfc9110.html#name-media-type
  val `text/plain;charset=utf-8` = MediaType.text.plain.withExtensions(Map("charset" -> "utf-8"))

  def apply[F[_]: Concurrent]: KoHttpRequest[F, ByteVector] = KoHttpRequest(Request[F](), EntityDecoder.byteVector)

  /** Methods to manipulate the request */
  trait UnderlyingOp[F[_]: Concurrent, Self <: UnderlyingOp[F, Self]]:
    def underlying: Request[F]
    def withUnderlying(req: Request[F]): Self

    /** method */
    def method: Method = underlying.method

    def withMethod(method: Method): Self = withUnderlying(underlying.withMethod(method))
    def mapMethod(func: Method => Method): Self = withMethod(func(method))

    // Methods from https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods
    // except CONNECT

    def GET    : Self = withMethod(Method.GET    )
    def HEAD   : Self = withMethod(Method.HEAD   )
    def POST   : Self = withMethod(Method.POST   )
    def PUT    : Self = withMethod(Method.PUT    )
    def PATCH  : Self = withMethod(Method.PATCH  )
    def DELETE : Self = withMethod(Method.DELETE )
    def OPTIONS: Self = withMethod(Method.OPTIONS)
    def TRACE  : Self = withMethod(Method.TRACE  )

    /** uri */
    def uri: Uri = underlying.uri

    def withUri(uri: Uri): Self = withUnderlying(underlying.withUri(uri))
    def mapUri(func: Uri => Uri): Self = withUri(func(uri)) // similar to RequestPrelude.mapUri

    def unsafeWithUri(uri: String): Self = withUri(Uri.unsafeFromString(uri))

    /** uri.scheme */
    def scheme: Option[Scheme] = uri.scheme

    def withScheme(scheme: Option[Scheme]): Self = mapUri(_.copy(scheme = scheme))
    def withScheme(scheme: Scheme): Self = withScheme(Some(scheme))
    def mapScheme(func: Scheme => Scheme): Self = withScheme(scheme.map(func))

    def unsafeWithScheme(scheme: Option[String]): Self = withScheme(scheme.map(Scheme.unsafeFromString))
    def unsafeWithScheme(scheme: String): Self = unsafeWithScheme(Some(scheme))

    // withoutScheme is not defined: call withScheme(None)

    /** uri.authority */
    def authority: Option[Authority] = uri.authority

    def withAuthority(authority: Option[Authority]): Self = mapUri(_.copy(authority = authority))
    def withAuthority(authority: Authority): Self = withAuthority(Some(authority))
    def mapAuthority(func: Authority => Authority): Self = withAuthority(authority.map(func))

    /** uri.authority.userInfo */
    def userInfo: Option[UserInfo] = authority.flatMap(_.userInfo)

    def withUserInfo(userInfo: Option[UserInfo]): Self = withAuthority:
      authority.map(_.copy(userInfo = userInfo)).getOrElse(Authority(userInfo = userInfo))
    def withUserInfo(userInfo: UserInfo): Self = withUserInfo(Some(userInfo))
    def mapUserInfo(func: UserInfo => UserInfo): Self = mapAuthority: authority =>
      authority.copy(userInfo = authority.userInfo.map(func))

    def unsafeWithUserInfo(userInfo: Option[String]): Self = withUserInfo:
      userInfo.map(str => UserInfo.fromString(str).fold(throw _, identity)) // UserInfo has no unsafeFromString
    def unsafeWithUserInfo(userInfo: String): Self = unsafeWithUserInfo(Some(userInfo))

    /** uri.authority.host */
    def host: Option[Host] = authority.map(_.host)

    def withHost(host: Host): Self = withAuthority:
      authority.map(_.copy(host = host)).getOrElse(Authority(host = host))
    def withHost(host: ip4s.Hostname): Self = withHost(RegName.fromHostname(host))
    def withHost(ipAddr: ip4s.IpAddress): Self = withHost(Host.fromIpAddress(ipAddr))
    def mapHost(func: Host => Host): Self = mapAuthority: authority =>
      authority.copy(host = func(authority.host))

    def unsafeWithHost(host: String): Self = ip4s.Host.fromString(host) match
      case Some(host: ip4s.Hostname) => withHost(host)
      case Some(i18n: ip4s.IDN) => withHost(i18n.hostname)
      case Some(ipAddr: ip4s.IpAddress) => withHost(ipAddr)
      case None => throw IllegalArgumentException(s"Failed to parse host name or IP address: $host")

    /** uri.authority.port */
    def port: Option[Int] = authority.flatMap(_.port)

    def withPort(port: Option[Int]): Self = withAuthority:
      authority.map(_.copy(port = port)).getOrElse(Authority(port = port))
    def withPort(port: Int): Self = withPort(Some(port))
    def mapPort(func: Int => Int): Self = mapAuthority: authority =>
      authority.copy(port = authority.port.map(func))

    /** uri.path */
    def path: Path = uri.path

    def withPath(path: Path): Self = mapUri(_.withPath(path))
    def mapPath(func: Path => Path): Self = withPath(func(path))
    def appendPath(relativePath: Path): Self = mapPath(concatPath(_, relativePath))

    def unsafeWithPath(path: String): Self = withPath(Path.unsafeFromString(path))
    def unsafeAppendPath(path: String): Self = appendPath(Path.unsafeFromString(path))

    // TODO can these, withPath, and withUri accept String then interpolate?
    def GET    (relativePath: Path): Self = GET    .appendPath(relativePath)
    def HEAD   (relativePath: Path): Self = HEAD   .appendPath(relativePath)
    def POST   (relativePath: Path): Self = POST   .appendPath(relativePath)
    def PUT    (relativePath: Path): Self = PUT    .appendPath(relativePath)
    def PATCH  (relativePath: Path): Self = PATCH  .appendPath(relativePath)
    def DELETE (relativePath: Path): Self = DELETE .appendPath(relativePath)
    def OPTIONS(relativePath: Path): Self = OPTIONS.appendPath(relativePath)
    def TRACE  (relativePath: Path): Self = TRACE  .appendPath(relativePath)

    // TODO make it safe
    def GET    (relativePath: String): Self = GET    .unsafeAppendPath(relativePath)
    def HEAD   (relativePath: String): Self = HEAD   .unsafeAppendPath(relativePath)
    def POST   (relativePath: String): Self = POST   .unsafeAppendPath(relativePath)
    def PUT    (relativePath: String): Self = PUT    .unsafeAppendPath(relativePath)
    def PATCH  (relativePath: String): Self = PATCH  .unsafeAppendPath(relativePath)
    def DELETE (relativePath: String): Self = DELETE .unsafeAppendPath(relativePath)
    def OPTIONS(relativePath: String): Self = OPTIONS.unsafeAppendPath(relativePath)
    def TRACE  (relativePath: String): Self = TRACE  .unsafeAppendPath(relativePath)

    /** uri.query */
    def query: Query = uri.query

    def withQuery(query: Query): Self = mapUri(_.copy(query = query))
    @targetName("withQueryOpt")
    def withQuery(pairs: (String, Option[String])*): Self = mapUri(_.copy(query = Query(pairs*)))
    def withQuery(pairs: (String, String)*): Self = mapUri(_.copy(query = Query.fromPairs(pairs*)))
    def withQuery(map: Map[String, Seq[String]]): Self = mapUri(_.copy(query = Query.fromMap(map)))
    def mapQuery(func: Query => Query): Self = withQuery(func(query))

    def unsafeWithQuery(query: String): Self = withQuery(Query.unsafeFromString(query))

    // query param

    def withParams[K, V](params: Map[K, V])(using QueryParamKeyLike[K], QueryParamEncoder[V]): Self =
      mapQuery(_.withQueryParams(params))

    def withParam[K, V](key: K, value: V)(using QueryParamKeyLike[K], QueryParamEncoder[V]): Self =
      mapQuery(_.withQueryParam(key, value))

    def withParamOpt[K, V](key: K, valueOpt: Option[V])(using QueryParamKeyLike[K], QueryParamEncoder[V]): Self =
      mapQuery(_.withOptionQueryParam(key, valueOpt))

    def withParam[K, V](head: (K, V), tail: (K, V)*)
        (using QueryParamKeyLike[K], QueryParamEncoder[V]): Self =
      mapQuery(query => (head +: tail).foldLeft(query) { (query, keyValue) =>
        val (key, value) = keyValue
        query.withQueryParam(key, value)
      })

    def withParamOpt[K, V](head: (K, Option[V]), tail: (K, Option[V])*)
        (using QueryParamKeyLike[K], QueryParamEncoder[V]): Self =
      mapQuery(query => (head +: tail).foldLeft(query) { (query, keyValueOpt) =>
        val (key, valueOpt) = keyValueOpt
        query.withOptionQueryParam(key, valueOpt)
      })

    // query param multiple values

    def withParamMultiValue[K, V](key: K, values: Seq[V])(using QueryParamKeyLike[K], QueryParamEncoder[V]): Self =
      mapQuery(_.withQueryParam(key, values))

    def withParamMultiValue[K, V](params: Map[K, Seq[V]])(using QueryParamKeyLike[K], QueryParamEncoder[V]): Self =
      mapQuery(_.withMultiValueQueryParams(params))

    // query param by keys

    def withParam[K](head: K, tail: K*)(using QueryParamKeyLike[K]): Self =
      mapQuery(query => (head +: tail).foldLeft(query): (query, key) =>
        query.withQueryParam(key)
      )

    def withoutParam[K](head: K, tail: K*)(using QueryParamKeyLike[K]): Self =
      mapQuery(query => (head +: tail).foldLeft(query): (query, key) =>
        query.removeQueryParam(key)
      )

    // query param by type

    def withParam[P](using QueryParam[P]): Self =
      mapQuery(_.withQueryParam[P])

    def withParamOpt[P](param: Option[P])(using QueryParam[P], QueryParamEncoder[P]): Self =
      mapQuery(_.withOptionQueryParam[P](param))

    /** uri.fragment */
    def fragment: Option[Fragment] = uri.fragment

    def withFragment(fragment: Option[Fragment]): Self = mapUri(_.copy(fragment = fragment))
    def withFragment(fragment: Fragment): Self = withFragment(Some(fragment))
    def mapFragment(func: Fragment => Fragment): Self = withFragment(fragment.map(func))

    def withoutFragment: Self = mapUri(_.withoutFragment)

    /** httpVersion */
    def httpVersion: HttpVersion = underlying.httpVersion

    def withHttpVersion(httpVersion: HttpVersion): Self = withUnderlying(underlying.withHttpVersion(httpVersion))
    def mapHttpVersion(func: HttpVersion=> HttpVersion): Self = withHttpVersion(func(httpVersion))

    /** headers */
    def headers: Headers = underlying.headers

    def withHeaders(headers: Headers): Self = withUnderlying(underlying.withHeaders(headers))
    def mapHeaders(func: Headers => Headers): Self = withHeaders(func(headers))

    // Request has withHeaders(headers: Header.ToRaw*)
    // but it is confusing if it replaces the whole headers or only overrides the conflicts

    /** Existing value is replaced if the same name already exists */
    def withHeader(headers: Header.ToRaw*): Self = mapHeaders(_ ++ Headers(headers))

    //def withHeader[H](header: H)(using Header[H, Header.Recurring]): Self =
    //  mapHeaders(_.add(header))

    def addHeaderIfNotExist(headers: Header.ToRaw*): Self = mapHeaders: orig =>
      orig ++ Headers(headers.values.filter(header => !orig.headers.exists(_.name == header.name)))

    def withoutHeader(names: CIString*): Self = mapHeaders(_.transform(_.filter(names.contains)))

    // auth header
    def withBasicAuth(user: String, password: String): Self =
        withHeader(Authorization(Credentials.Token(AuthScheme.Basic, BasicCredentials(user, password).token)))

    def withBearerAuth(token: String): Self =
        withHeader(Authorization(Credentials.Token(AuthScheme.Bearer, token)))

    /** attributes */
    def attrs: Vault = underlying.attributes

    def withAttrs(attrs: Vault): Self = withUnderlying(underlying.withAttributes(attrs))
    def mapAttrs(func: Vault => Vault): Self = withAttrs(func(attrs))

    /** Existing value is replaced if the same key already exists */
    def withAttr[A](key: Key[A], value: A): Self = mapAttrs(_.insert(key, value))

    def withoutAttr[A](key: Key[A]): Self = mapAttrs(_.delete(key))

    /** body */
    def body: Stream[F, Byte] = underlying.body

    def withBody(body: Stream[F, Byte]): Self = withUnderlying(underlying.withBodyStream(body))
    def mapBody(func: Stream[F, Byte] => Stream[F, Byte]): Self = withBody(func(body))

    /** Sets the body with accompanying header */
    def withBody[A](body: A)(using encoder: EntityEncoder[F, A]): Self =
        withUnderlying(underlying.withEntity(body)(encoder))

    // with Entity with hardcoded encoder (so that they need not be imported) */
    def withBody(body: ByteVector): Self = withBody[ByteVector](body)(using EntityEncoder.byteVectorEncoder)
    def withBody(body: Array[Byte]): Self = withBody[Array[Byte]](body)(using EntityEncoder.byteArrayEncoder)
    def withBody(body: String): Self = withBody[String](body)(using EntityEncoder.stringEncoder)
    def withBody(body: Array[Char]): Self = withBody[Array[Char]](body)(using EntityEncoder.charArrayEncoder)

    def withBody(form: UrlForm): Self = withBody[UrlForm](form)(using UrlForm.entityEncoder)
    def withUrlForm(pairs: (String, String)*): Self = withBody(UrlForm(pairs*))
    @targetName("withUrlFormOpt")
    def withUrlForm(pairs: (String, Option[String])*): Self = withUrlForm(pairs.flatMap { pair =>
      pair match
        case (key, Some(value)) => Some(key -> value)
        case (key, None)        => None
    }*)

    def withBody(multipart: Multipart[F]): Self =
        withBody[Multipart[F]](multipart)(using EntityEncoder.multipartEncoder)
        .withHeader(multipart.headers)

  // Path manipulation

  def concatPath(left: Path, right: Path): Path = {
    if (right.isEmpty)
      Path(left.segments, left.absolute, left.endsWithSlash | right.endsWithSlash)

    else if (left.isEmpty || right.absolute)
      Path(right.segments, left.absolute || right.absolute, right.endsWithSlash)

    else if (left.endsWithSlash)
      Path(left.segments ++ right.segments, left.absolute, right.endsWithSlash)

    // concatenate the last segment of the left and the first segment of the right
    else {
      val prefixSegments = left.segments.init
      val concatSegment  = Path.Segment.encoded(left.segments.last.encoded + right.segments.head.encoded)
      val suffixSegments = right.segments.tail
      Path((prefixSegments :+ concatSegment) ++ suffixSegments, left.absolute, right.endsWithSlash)
    }
  }
