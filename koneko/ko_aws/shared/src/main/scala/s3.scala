package jp.ukiba.koneko
package ko_aws

import jp.ukiba.koneko.ko_http4s.client.KoHttpClient
import jp.ukiba.koneko.ko_fs2.xml.{XmlParser, XmlTextTrimmer, XmlEventLog}

import org.http4s.{Uri, QueryParamEncoder, Header, Headers, Status, MediaType}
import org.http4s.headers.{ETag, `Accept-Ranges`, `Content-Range`, `Range`, `Content-Type`,
                          `Content-Disposition`, `Content-Encoding`, `Content-Language`, `Content-Length`,
                          `Last-Modified`, `Cache-Control`, `Expires`,
                          `If-Match`, `If-None-Match`, `If-Modified-Since`, `If-Unmodified-Since`}
import org.http4s.syntax.all.*
import fs2.data.xml, xml.{XmlEvent, QName}, XmlEvent.{StartTag, EndTag}
import fs2.{Stream, Pipe, Pull, text}
import org.typelevel.log4cats.Logger
import cats.effect.{Sync, Async, Resource}
import cats.syntax.all.*
import cats.data.Chain
import org.typelevel.ci.{CIStringSyntax, CIString}

import java.time.Instant

/*
  2025-07
    https://github.com/gnieh/fs2-data is used since
    https://github.com/http4s/http4s-fs2-data has no releases for 16 months
*/
package object s3:
  import XmlParser.{Parser, expect, optPat, repPat, startTag, endTag, textOnlyTag, textNonEmptyOnlyTag, textOnlyTagOpt}

  type Timestamp = String // TODO

  given QueryParamEncoder[Char] = QueryParamEncoder[String].contramap(_.toString) // TODO relocate

  import scala.language.implicitConversions
  import org.http4s.Header
  given optionConv[A, B](using conv: Conversion[A, B]): Conversion[Option[A], Option[B]] with
    def apply(o: Option[A]): Option[B] = o.map(conv)

  extension [F[_]: Sync] (headers: Headers)
    def getSingleText(key: CIString, label: String)(using log: Logger[F]): F[Option[String]] =
      headers.get(key) match
        case Some(nel) =>
          for
            _ <- if nel.tail.nonEmpty then
              log.warn(s"$label: $key has more than one value: ${nel.iterator.map(_.value).mkString(", ")}")
            else
              Sync[F].unit
          yield nel.head.value.pure

        case None =>
          None.pure

    /** Does not fail even if AWS stops sending the header */
    def getSingleTextRequired(key: CIString, label: String)(using log: Logger[F]): F[String] =
      for
        opt <- getSingleText(key, label)
        value <- opt match
          case Some(value) => value.pure
          case None        => log.warn(s"$label: $key is missing; treating it as the empty").as("")
      yield value

    /** Does not fail even if AWS sends an unexpected value */
    def getSingleBoolean(key: CIString, label: String)(using log: Logger[F]): F[Option[Boolean]] =
      for
        opt <- getSingleText(key, label)
        value <- opt match
          case Some("true" ) => Some(true ).pure
          case Some("false") => Some(false).pure
          case Some(other)   => log.warn(s"$label: $key: Unexpected value: $other; treating it as missing").as(None)
          case None          => None.pure
      yield value

  // MinIO (Vultr) and Wasabi seem to send both, but
  // Cloudflare R2, Ceph Rados Gateway (RGW), and DigitalOcean Spaces seem to send only `x-amz-request-id`
  trait S3Response:
    //Date
    //Server
    def `x-amz-request-id`: String
    def `x-amz-id-2`      : String

  /*
    The models could be generated with https://github.com/smithy-lang/smithy-java
    then amended with https://doc.s3.amazonaws.com/2006-03-01/AmazonS3.xsd

    Further considerations can be inspired by https://smithy.io/2.0/aws/customizations/s3-customizations.html
  */

  /*
    https://docs.aws.amazon.com/AmazonS3/latest/API/API_Owner.html has the the wrong order of elements,
    and says both of them are "Required: No"

      <Owner>
        <DisplayName>string</DisplayName>
        <ID>string</ID>
      </Owner>

    https://doc.s3.amazonaws.com/2006-03-01/AmazonS3.xsd agrees the actual response with CanonicalUser

      <xsd:sequence>
        <xsd:element name="ID" type="xsd:string"/>              
        <xsd:element name="DisplayName" type="xsd:string" minOccurs="0"/>
      </xsd:sequence>

    https://docs.aws.amazon.com/AmazonS3/latest/API/Welcome.html and
    https://docs.aws.amazon.com/AmazonS3/latest/API/WhatsNew.html says
    the latest API version is 2006-03-01 as of 2025-05.
  */
  case class CanonicalUser(
    ID: String, // not optional as in the XSD
    DisplayName: Option[String], // will be missing since 2025-10
  )
  object CanonicalUser:
    def parser[F[_]: Sync](local: String): Parser[F, CanonicalUser] =
      for
        _           <- startTag[F](local)
        ID          <- textNonEmptyOnlyTag[F]("ID")
        DisplayName <- textOnlyTagOpt[F]("DisplayName")
        _           <- endTag[F](local)
      yield CanonicalUser(ID, DisplayName)

  case class PrefixEntry(
    Prefix: String, // not optional as in the XSD
  )
  object PrefixEntry:
    def parser[F[_]: Sync](head: StartTag): Parser[F, PrefixEntry] =
      for
        Prefix <- textNonEmptyOnlyTag[F]("Prefix")
        _      <- expect[F](EndTag(head.name))
      yield PrefixEntry(Prefix)

  case class RestoreStatus(
    IsRestoreInProgress: Option[Boolean],
    RestoreExpiryDate: Option[Timestamp],
  )
  object RestoreStatus:
    def parser[F[_]: Sync](head: StartTag): Parser[F, RestoreStatus] =
      for
        IsRestoreInProgress <- textOnlyTagOpt[F]("IsRestoreInProgress").map(_.map(_.toBoolean))
        RestoreExpiryDate   <- textOnlyTagOpt[F]("RestoreExpiryDate")
        _                   <- expect[F](EndTag(head.name))
      yield RestoreStatus(IsRestoreInProgress, RestoreExpiryDate)

  case class ListAllMyBucketsResult(
    Owner: CanonicalUser,
    Buckets: Chain[ListAllMyBucketsResult.Buckets.Bucket],
    ContinuationToken: Option[String],
    Prefix: Option[String],
  )
  object ListAllMyBucketsResult:
    def parser[F[_]: Sync]: Parser[F, ListAllMyBucketsResult] =
      for
        _                 <- startTag[F]("ListAllMyBucketsResult")
        owner             <- CanonicalUser.parser("Owner")
        buckets           <- Buckets.parser("Buckets")
        continuationToken <- textOnlyTagOpt[F]("ContinuationToken")
        prefix            <- textOnlyTagOpt[F]("Prefix")
        _                 <- endTag[F]("ListAllMyBucketsResult")
      yield ListAllMyBucketsResult(owner, buckets, continuationToken, prefix)

    object Buckets:
      def parser[F[_]: Sync](local: String): Parser[F, Chain[Bucket]] =
        for
          _      <- startTag[F](local)
          bucket <- repPat:
            case head @ StartTag(QName(_, "Bucket"), _, _) => Bucket.parser(head)
          _      <- endTag[F](local)
        yield bucket

      // https://docs.aws.amazon.com/AmazonS3/latest/API/API_Bucket.html
      case class Bucket(
        Name: String,
        CreationDate: String, // timestamp (doc 2019-12-11T23:32:47+00:00, actual 2024-11-13T14:52:58.000Z)
        BucketRegion: Option[String],
      )
      object Bucket:
        def parser[F[_]: Sync](head: StartTag): Parser[F, Bucket] =
          for
            Name         <- textNonEmptyOnlyTag[F]("Name")
            CreationDate <- textNonEmptyOnlyTag[F]("CreationDate")
            BucketRegion <- textOnlyTagOpt[F]("BucketRegion") // present when Request.`bucket-region` is present
            _            <- expect[F](EndTag(head.name))
          yield Bucket(Name, CreationDate, BucketRegion)

  object ListBuckets:
    def apply[F[_]: Async](profile: Aws.Profile)(http: KoHttpClient[F, ?])(req: Request,
        dontRepeatWithContinuationToken: Boolean = false)
        (using Logger[F]): F[Response] =
      import profile.{credentials, region}
      for
        http <- http.withUri(profile.endpointUriOf("s3")).GET("")
          .withParamOpt("bucket-region"     , req.`bucket-region`)
          .withParamOpt("continuation-token", req.`continuation-token`)
          .withParamOpt("max-buckets"       , req.`max-buckets`)
          .withParamOpt("prefix"            , req.`prefix`)
          .evalMapRequest(AwsSigV4.`UNSIGNED-PAYLOAD`(_, credentials, region, "s3"))

        result <- http.acceptString.run.expectSuccess.resource.use: resp =>
          resp.body
            .through(text.utf8.decode)
            .through(xml.events())
            //.through(xml.namespaceResolver) // ignoring the namespace (AWS doc "Response Syntax" has no namespace)
            .through(xml.referenceResolver()) // e.g. "&amp;" to "&"
            .through(xml.normalize) // e.g. merge adjacent texts
            .through(XmlTextTrimmer.pipe)
            .through(XmlEventLog.pipe)
            .through(XmlParser.doc(ListAllMyBucketsResult.parser).pipe)
            .compile.onlyOrError

        // get more results when continuation-token exists
        result <- (result.ContinuationToken, dontRepeatWithContinuationToken) match
          case (Some(continuationToken), false) => apply(profile)(http)
              (req.copy(`continuation-token` = Some(continuationToken)), false)
              .map(next => result.copy(Buckets = result.Buckets ++ next.Buckets, ContinuationToken = None))
          case _ => result.pure[F]

      yield
        result

    case class Request(
      `bucket-region`     : Option[String] = None,
      `continuation-token`: Option[String] = None,
      `max-buckets`       : Option[Int]    = None,
      `prefix`            : Option[String] = None,
    ):
      require(`continuation-token`.forall(str => 0 <= str.length && str.length <= 1024),
          s"continuation-token = ${`continuation-token`}")
      require(`max-buckets`.forall(num => 1 <= num && num <= 10000),
          s"max-buckets = ${`max-buckets`}")

    type Response = ListAllMyBucketsResult // TODO extends S3Response

  case class ListBucketResult(
    Name: String,
    Prefix: String,
    StartAfter: Option[String],
    ContinuationToken: Option[String],
    NextContinuationToken: Option[String],
    KeyCount: Int,
    MaxKeys: Int,
    Delimiter: Option[String],
    EncodingType: Option[String],
    IsTruncated: Boolean,
    Contents: Chain[ListBucketResult.Contents],
    CommonPrefixes: Chain[PrefixEntry],
  )
  object ListBucketResult:
    def parser[F[_]: Sync]: Parser[F, ListBucketResult] =
      for
        _                     <- startTag[F]("ListBucketResult")
        Name                  <- textNonEmptyOnlyTag[F]("Name")
        Prefix                <- textOnlyTag[F]("Prefix").map(_.mkString)
        StartAfter            <- textOnlyTagOpt[F]("StartAfter")
        ContinuationToken     <- textOnlyTagOpt[F]("ContinuationToken")
        NextContinuationToken <- textOnlyTagOpt[F]("NextContinuationToken")
        KeyCount              <- textNonEmptyOnlyTag[F]("KeyCount").map(_.toInt)
        MaxKeys               <- textNonEmptyOnlyTag[F]("MaxKeys").map(_.toInt)
        Delimiter             <- textOnlyTagOpt[F]("Delimiter")
        EncodingType          <- textOnlyTagOpt[F]("EncodingType")
        IsTruncated           <- textNonEmptyOnlyTag[F]("IsTruncated").map(_.toBoolean)
        Contents              <- repPat:
          case head @ StartTag(QName(_, "Contents"), _, _) => Contents.parser(head)
        CommonPrefixes        <- repPat:
          case head @ StartTag(QName(_, "CommonPrefixes"), _, _) => PrefixEntry.parser(head)
        _                     <- endTag[F]("ListBucketResult")
      yield ListBucketResult(Name, Prefix, StartAfter, ContinuationToken, NextContinuationToken, KeyCount, MaxKeys,
          Delimiter, EncodingType, IsTruncated, Contents, CommonPrefixes)

    case class Contents(
      Key: String,
      LastModified: Timestamp,
      ETag: String,
      ChecksumAlgorithm: Option[String], // TODO maybe enum: CRC32 | CRC32C | SHA1 | SHA256 | CRC64NVME
      ChecksumType: Option[String], // TODO maybe enum: COMPOSITE | FULL_OBJECT
      Owner: Option[CanonicalUser],
      RestoreStatus: Option[RestoreStatus],
      Size: Long,
      StorageClass: String, // TODO maybe enum: STANDARD | REDUCED_REDUNDANCY | GLACIER | STANDARD_IA | ONEZONE_IA | INTELLIGENT_TIERING | DEEP_ARCHIVE | OUTPOSTS | GLACIER_IR | SNOW | EXPRESS_ONEZONE | FSX_OPENZFS 
    )
    object Contents:
      def parser[F[_]: Sync](head: StartTag): Parser[F, Contents] =
        for
          Key               <- textNonEmptyOnlyTag[F]("Key")
          LastModified      <- textNonEmptyOnlyTag[F]("LastModified")
          ETag              <- textNonEmptyOnlyTag[F]("ETag")
          ChecksumAlgorithm <- textOnlyTagOpt[F]("ChecksumAlgorithm")
          ChecksumType      <- textOnlyTagOpt[F]("ChecksumType")
          Owner             <- optPat:
            case head @ StartTag(QName(_, "Owner"), _, _) => CanonicalUser.parser("Owner")
          RestoreStatus     <- optPat:
            case head @ StartTag(QName(_, "RestoreStatus"), _, _) => RestoreStatus.parser(head)
          Size              <- textNonEmptyOnlyTag[F]("Size").map(_.toLong)
          StorageClass      <- textNonEmptyOnlyTag[F]("StorageClass")
          _                 <- expect[F](EndTag(head.name))
        yield Contents(Key, LastModified, ETag, ChecksumAlgorithm, ChecksumType, Owner,
            RestoreStatus, Size, StorageClass)

  object ListObjectsV2:
    def apply[F[_]: Async](profile: Aws.Profile)(http: KoHttpClient[F, ?])(req: Request,
        dontRepeatWithContinuationToken: Boolean = false)
        (using Logger[F]): F[Response] =
      import profile.{credentials, region}
      for
        http <- http.withUri(profile.endpointUriOfS3(req.bucket)).GET("")
          .withParam   ("list-type"         , 2)
          .withParamOpt("continuation-token", req.`continuation-token`)
          .withParamOpt("delimiter"         , req.`delimiter`)
          .withParamOpt("fetch-owner"       , req.`fetch-owner`)
          .withParamOpt("max-keys"          , req.`max-keys`)
          .withParamOpt("prefix"            , req.`prefix`)
          .withParamOpt("start-after"       , req.`start-after`)
          .evalMapRequest(AwsSigV4.`UNSIGNED-PAYLOAD`(_, credentials, region, "s3"))

        result <- http.acceptString.run.expectSuccess.resource.use: resp =>
          resp.body
            .through(text.utf8.decode)
            .through(xml.events())
            //.through(xml.namespaceResolver) // ignoring the namespace (AWS doc "Response Syntax" has no namespace)
            .through(xml.referenceResolver()) // e.g. "&amp;" to "&"
            .through(xml.normalize) // e.g. merge adjacent texts
            .through(XmlTextTrimmer.pipe)
            .through(XmlEventLog.pipe)
            .through(XmlParser.doc(ListBucketResult.parser).pipe)
            .compile.onlyOrError

        // get more results when continuation-token exists
        result <- (result.NextContinuationToken, dontRepeatWithContinuationToken) match
          case (Some(continuationToken), false) => apply(profile)(http)
              (req.copy(`continuation-token` = Some(continuationToken)), false)
              .map(next => result.copy(Contents = result.Contents ++ next.Contents, NextContinuationToken = None))
          case _ => result.pure[F]

      yield
        result

    case class Request(
      bucket              : String,
      `bucket-region`     : Option[String]  = None,
      `continuation-token`: Option[String]  = None,
      `delimiter`         : Option[Char]    = None,
      `encoding-type`     : Option[String]  = None,
      `fetch-owner`       : Option[Boolean] = None,
      `max-keys`          : Option[Int]     = None,
      `prefix`            : Option[String]  = None,
      `start-after`       : Option[String]  = None,
      //x-amz-expected-bucket-owner
      //x-amz-optional-object-attributes
      //x-amz-request-payer
    ):
      require(`continuation-token`.forall(str => 0 <= str.length && str.length <= 1024),
          s"continuation-token = ${`continuation-token`}")
      require(`max-keys`.forall(num => 1 <= num),
          s"max-keys = ${`max-keys`}")

    type Response = ListBucketResult // TODO extends S3Response

  object PutObject:
    def apply[F[_]: Async](profile: Aws.Profile)(http: KoHttpClient[F, ?])(req: Request[F],
        dontRepeatWithContinuationToken: Boolean = false)
        (using log: Logger[F]): F[Response] =
      import profile.{credentials, region}
      require(req.`Content-Length`.nonEmpty, "the payload length must be known") // even when streaming chunks
      for
        http <- http.withUri(profile.endpointUriOfS3(req.bucket)).PUT(req.key)
          .withBody(req.content)
          .withHeaderOpt(req.`Cache-Control`      .map(_.toRaw1))
          .withHeaderOpt(req.`Content-Disposition`.map(_.toRaw1))
          .withHeaderOpt(req.`Content-Encoding`   .map(_.toRaw1))
          .withHeaderOpt(req.`Content-Language`   .map(_.toRaw1))
          .withHeaderOpt(req.`Content-Length`     .map(_.toRaw1))
          .withHeaderOpt(req.`Content-MD5`.map("Content-MD5" -> _))
          .withHeaderOpt(req.`Content-Type`       .map(_.toRaw1))
          .withHeaderOpt(req.`Expires`            .map(_.toRaw1))
          .withHeaderOpt(req.`If-Match`           .map(_.toRaw1))
          .withHeaderOpt(req.`If-None-Match`      .map(_.toRaw1))
          .evalMapRequest(AwsSigV4.`UNSIGNED-PAYLOAD`(_, credentials, region, "s3"))

        result <- http.run.expectSuccess.resource.use: resp =>
          val label = "PutObject: response"
          inline def getSingleTextRequired(key: CIString) =
              resp.headers.getSingleTextRequired(key, label)
          for
            ETag               <- resp.headers.get[ETag] match // TODO hard to generalize
              case Some(value) => value.pure
              case None        => log.warn(s"$label: ETag is missing; treating it as the empty").as(ETag(""))
            `x-amz-request-id` <- getSingleTextRequired(ci"x-amz-request-id")
            `x-amz-id-2`       <- getSingleTextRequired(ci"x-amz-id-2")
          yield Response(
            ETag,
            `x-amz-request-id`,
            `x-amz-id-2`,
          )

      yield
        result

    case class Request[F[_]](
      bucket               : String,
      key                  : String,
      content              : Stream[F, Byte],
      `Cache-Control`      : Option[`Cache-Control`      ] = None, // propagates to GET, HEAD, CloudFront, ...
      `Content-Disposition`: Option[`Content-Disposition`] = None, // propagates to GET, HEAD, CloudFront, ...
      `Content-Encoding`   : Option[`Content-Encoding`   ] = None, // propagates to GET, HEAD, CloudFront, ...
      `Content-Language`   : Option[`Content-Language`   ] = None, // propagates to GET, HEAD, CloudFront, ...
      `Content-Length`     : Option[`Content-Length`     ] = None,
      `Content-MD5`        : Option[String               ] = None,
      `Content-Type`       : Option[`Content-Type`       ] = None, // propagates to GET, HEAD, CloudFront, ...
      `Expires`            : Option[`Expires`            ] = None, // propagates to GET, HEAD, CloudFront, ...
      `If-Match`           : Option[`If-Match`           ] = None,
      `If-None-Match`      : Option[`If-None-Match`      ] = None,
      //x-amz-acl
      //...
      //x-amz-checksum-sha256
      //...
    ):
      require(key.length >= 1)

    case class Response(
      ETag: ETag, // cannot be SHA-256
      //x-amz-expiration
      //...
      //x-amz-checksum-sha256
      //...
      `x-amz-request-id`     : String,
      `x-amz-id-2`           : String,
    ) extends S3Response

  object HeadObject:
    def apply[F[_]: Async](profile: Aws.Profile)(http: KoHttpClient[F, ?])(req: Request)
        (using log: Logger[F]): F[Response] =
      import profile.{credentials, region}
      for
        http <- http.withUri(profile.endpointUriOfS3(req.bucket)).HEAD(req.key)
          .withParamOpt("partNumber", req.partNumber)
          .withParamOpt("versionId" , req.versionId)
          .withParamOpt("response-cache-control"      , req.`response-cache-control`      .map(_.value))
          .withParamOpt("response-content-type"       , req.`response-content-type`       .map(_.value))
          .withParamOpt("response-content-disposition", req.`response-content-disposition`.map(_.value))
          .withParamOpt("response-content-encoding"   , req.`response-content-encoding`   .map(_.value))
          .withParamOpt("response-content-language"   , req.`response-content-language`   .map(_.value))
          .withParamOpt("response-expires"            , req.`response-expires`            .map(_.value))
          .withHeaderOpt(req.`Range`              .map(_.toRaw1))
          .withHeaderOpt(req.`If-Match`           .map(_.toRaw1))
          .withHeaderOpt(req.`If-None-Match`      .map(_.toRaw1))
          .withHeaderOpt(req.`If-Modified-Since`  .map(_.toRaw1))
          .withHeaderOpt(req.`If-Unmodified-Since`.map(_.toRaw1))
          .withHeaderOpt(req.`x-amz-server-side-encryption-customer-algorithm`
              .map("x-amz-server-side-encryption-customer-algorithm" -> _))
          .withHeaderOpt(req.`x-amz-server-side-encryption-customer-key`
              .map("x-amz-server-side-encryption-customer-key" -> _))
          .withHeaderOpt(req.`x-amz-server-side-encryption-customer-key-MD5`
              .map("x-amz-server-side-encryption-customer-key-MD5" -> _))
          .withHeaderOpt(req.`x-amz-request-payer`        .map("x-amz-request-payer"         -> _))
          .withHeaderOpt(req.`x-amz-expected-bucket-owner`.map("x-amz-expected-bucket-owner" -> _))
          .withHeaderOpt(req.`x-amz-checksum-mode`        .map("x-amz-checksum-mode"         -> _))
          .evalMapRequest(AwsSigV4.`UNSIGNED-PAYLOAD`(_, credentials, region, "s3"))

        result <- http.run.expectStatus(status => status.responseClass == Status.Successful ||
            status == Status.NotFound).resource.use: resp =>
          val label = "GetObject: response"
          inline def getSingleTextRequired(key: CIString) =
              resp.headers.getSingleTextRequired(key, label)
          inline def getSingleText(key: CIString) =
              resp.headers.getSingleText(key, label)
          inline def getSingleBoolean(key: CIString) =
              resp.headers.getSingleBoolean(key, label)
          for
            ETag                    <- resp.headers.get[ETag].pure
            `Content-Length`        <- resp.headers.get[`Content-Length`].pure
            `Content-Range`         <- resp.headers.get[`Content-Range`].pure
            `Content-Type`          <- resp.headers.get[`Content-Type`] match // TODO hard to generalize
              case Some(value) => value.pure
              case None        => log.warn(s"$label: Content-Type is missing; treating it as the empty")
                                      .as(`Content-Type`(MediaType.application.`octet-stream`))
            `Content-Disposition`   <- resp.headers.get[`Content-Disposition`].pure
            `Content-Encoding`      <- resp.headers.get[`Content-Encoding`].pure
            `Content-Language`      <- resp.headers.get[`Content-Language`].pure
            `Accept-Ranges`         <- resp.headers.get[`Accept-Ranges`].pure
            `Last-Modified`         <- resp.headers.get[`Last-Modified`].pure
            `Cache-Control`         <- resp.headers.get[`Cache-Control`].pure
            `Expires`               <- resp.headers.get[`Expires`].pure
            `x-amz-delete-marker`   <- getSingleBoolean     (ci"x-amz-delete-marker")
            `x-amz-version-id`      <- getSingleText        (ci"x-amz-version-id")
            `x-amz-request-charged` <- getSingleText        (ci"x-amz-request-charged")
            `x-amz-request-id`      <- getSingleTextRequired(ci"x-amz-request-id")
            `x-amz-id-2`            <- getSingleTextRequired(ci"x-amz-id-2")
          yield
            Response(
              ETag,
              `Content-Length`,
              `Content-Range`,
              `Content-Type`,
              `Content-Disposition`,
              `Content-Encoding`,
              `Content-Language`,
              `Accept-Ranges`,
              `Last-Modified`,
              `Cache-Control`,
              `Expires`,
              `x-amz-delete-marker`,
              `x-amz-version-id`,
              `x-amz-request-charged`,
              `x-amz-request-id`,
              `x-amz-id-2`,
            )

      yield
        result

    type Request = GetObject.Request
    val  Request = GetObject.Request

    case class Response( // has `x-amz-archive-status` in addition to GetObject.Response
      ETag                   : Option[ETag                 ],
      `Content-Length`       : Option[`Content-Length`     ],
      `Content-Range`        : Option[`Content-Range`      ],
      `Content-Type`         : `Content-Type`, // application/xml on error responses
      `Content-Disposition`  : Option[`Content-Disposition`],
      `Content-Encoding`     : Option[`Content-Encoding`   ],
      `Content-Language`     : Option[`Content-Language`   ],
      `Accept-Ranges`        : Option[`Accept-Ranges`      ],
      `Last-Modified`        : Option[`Last-Modified`      ],
      `Cache-Control`        : Option[`Cache-Control`      ],
      `Expires`              : Option[`Expires`            ],
      `x-amz-delete-marker`  : Option[Boolean],
      `x-amz-version-id`     : Option[String],
      `x-amz-request-charged`: Option[String],
      `x-amz-request-id`     : String,
      `x-amz-id-2`           : String,
      //...
    ) extends S3Response:
      def exists: Boolean = ETag.nonEmpty

  object GetObject:
    def apply[F[_]: Async](profile: Aws.Profile)(http: KoHttpClient[F, ?])(req: Request)
        (using log: Logger[F]): Resource[F, (Stream[F, Byte], Response)] =
      import profile.{credentials, region}
      for
        http <- Resource.eval(http.withUri(profile.endpointUriOfS3(req.bucket)).GET(req.key)
          .withParamOpt("partNumber", req.partNumber)
          .withParamOpt("versionId" , req.versionId)
          .withParamOpt("response-cache-control"      , req.`response-cache-control`      .map(_.value))
          .withParamOpt("response-content-type"       , req.`response-content-type`       .map(_.value))
          .withParamOpt("response-content-disposition", req.`response-content-disposition`.map(_.value))
          .withParamOpt("response-content-encoding"   , req.`response-content-encoding`   .map(_.value))
          .withParamOpt("response-content-language"   , req.`response-content-language`   .map(_.value))
          .withParamOpt("response-expires"            , req.`response-expires`            .map(_.value))
          .withHeaderOpt(req.`Range`              .map(_.toRaw1))
          .withHeaderOpt(req.`If-Match`           .map(_.toRaw1))
          .withHeaderOpt(req.`If-None-Match`      .map(_.toRaw1))
          .withHeaderOpt(req.`If-Modified-Since`  .map(_.toRaw1))
          .withHeaderOpt(req.`If-Unmodified-Since`.map(_.toRaw1))
          .withHeaderOpt(req.`x-amz-server-side-encryption-customer-algorithm`
              .map("x-amz-server-side-encryption-customer-algorithm" -> _))
          .withHeaderOpt(req.`x-amz-server-side-encryption-customer-key`
              .map("x-amz-server-side-encryption-customer-key" -> _))
          .withHeaderOpt(req.`x-amz-server-side-encryption-customer-key-MD5`
              .map("x-amz-server-side-encryption-customer-key-MD5" -> _))
          .withHeaderOpt(req.`x-amz-request-payer`        .map("x-amz-request-payer"         -> _))
          .withHeaderOpt(req.`x-amz-expected-bucket-owner`.map("x-amz-expected-bucket-owner" -> _))
          .withHeaderOpt(req.`x-amz-checksum-mode`        .map("x-amz-checksum-mode"         -> _))
          .evalMapRequest(AwsSigV4.`UNSIGNED-PAYLOAD`(_, credentials, region, "s3"))
        )

        result <- http.run.expectSuccess.resource.evalMap: resp => // TODO Not Modified status
          val label = "HeadObject: response"
          inline def getSingleTextRequired(key: CIString) =
              resp.headers.getSingleTextRequired(key, label)
          inline def getSingleText(key: CIString) =
              resp.headers.getSingleText(key, label)
          inline def getSingleBoolean(key: CIString) =
              resp.headers.getSingleBoolean(key, label)
          for
            ETag                    <- resp.headers.get[ETag].pure
            `Content-Length`        <- resp.headers.get[`Content-Length`].pure
            `Content-Range`         <- resp.headers.get[`Content-Range`].pure
            `Content-Type`          <- resp.headers.get[`Content-Type`] match // TODO hard to generalize
              case Some(value) => value.pure
              case None        => log.warn(s"$label: Content-Type is missing; treating it as the empty")
                                      .as(`Content-Type`(MediaType.application.`octet-stream`))
            `Content-Disposition`   <- resp.headers.get[`Content-Disposition`].pure
            `Content-Encoding`      <- resp.headers.get[`Content-Encoding`].pure
            `Content-Language`      <- resp.headers.get[`Content-Language`].pure
            `Accept-Ranges`         <- resp.headers.get[`Accept-Ranges`].pure
            `Last-Modified`         <- resp.headers.get[`Last-Modified`].pure
            `Cache-Control`         <- resp.headers.get[`Cache-Control`].pure
            `Expires`               <- resp.headers.get[`Expires`].pure
            `x-amz-delete-marker`   <- getSingleBoolean     (ci"x-amz-delete-marker")
            `x-amz-version-id`      <- getSingleText        (ci"x-amz-version-id")
            `x-amz-request-charged` <- getSingleText        (ci"x-amz-request-charged")
            `x-amz-request-id`      <- getSingleTextRequired(ci"x-amz-request-id")
            `x-amz-id-2`            <- getSingleTextRequired(ci"x-amz-id-2")
          yield
            (resp.body, Response(
              ETag,
              `Content-Length`,
              `Content-Range`,
              `Content-Type`,
              `Content-Disposition`,
              `Content-Encoding`,
              `Content-Language`,
              `Accept-Ranges`,
              `Last-Modified`,
              `Cache-Control`,
              `Expires`,
              `x-amz-delete-marker`,
              `x-amz-version-id`,
              `x-amz-request-charged`,
              `x-amz-request-id`,
              `x-amz-id-2`,
            ))

      yield
        result

    case class Request(
      bucket                        : String,
      key                           : String,
      partNumber                    : Option[String               ] = None,
      versionId                     : Option[String               ] = None,
      `response-cache-control`      : Option[`Cache-Control`      ] = None,
      `response-content-type`       : Option[`Content-Type`       ] = None,
      `response-content-disposition`: Option[`Content-Disposition`] = None,
      `response-content-encoding`   : Option[`Content-Encoding`   ] = None,
      `response-content-language`   : Option[`Content-Language`   ] = None,
      `response-expires`            : Option[`Expires`            ] = None,
      `Range`                       : Option[`Range`              ] = None,
      `If-Match`                    : Option[`If-Match`           ] = None,
      `If-None-Match`               : Option[`If-None-Match`      ] = None,
      `If-Modified-Since`           : Option[`If-Modified-Since`  ] = None,
      `If-Unmodified-Since`         : Option[`If-Unmodified-Since`] = None,
      `x-amz-server-side-encryption-customer-algorithm`: Option[String] = None,
      `x-amz-server-side-encryption-customer-key`      : Option[String] = None,
      `x-amz-server-side-encryption-customer-key-MD5`  : Option[String] = None,
      `x-amz-request-payer`                            : Option[String] = None,
      `x-amz-expected-bucket-owner`                    : Option[String] = None,
      `x-amz-checksum-mode`                            : Option[String] = None,
    )

    case class Response(
      ETag                   : Option[ETag                 ],
      `Content-Length`       : Option[`Content-Length`     ],
      `Content-Range`        : Option[`Content-Range`      ],
      `Content-Type`         : `Content-Type`, // default application/octet-stream
      `Content-Disposition`  : Option[`Content-Disposition`],
      `Content-Encoding`     : Option[`Content-Encoding`   ],
      `Content-Language`     : Option[`Content-Language`   ],
      `Accept-Ranges`        : Option[`Accept-Ranges`      ],
      `Last-Modified`        : Option[`Last-Modified`      ],
      `Cache-Control`        : Option[`Cache-Control`      ],
      `Expires`              : Option[`Expires`            ],
      `x-amz-delete-marker`  : Option[Boolean],
      `x-amz-version-id`     : Option[String],
      `x-amz-request-charged`: Option[String],
      `x-amz-request-id`     : String,
      `x-amz-id-2`           : String,
      //...
    ) extends S3Response

  /*
   * 1. Unversioned bucket: permanently deletes the object
   * 2. Versioned bucket
   *     1. Without versionId: adds a delete marker (soft delete)
   *     2. with versionId: permanently deletes that version 
   */
  object DeleteObject:
    def apply[F[_]: Async](profile: Aws.Profile)(http: KoHttpClient[F, ?])(req: Request)
        (using Logger[F]): F[Response] =
      import profile.{credentials, region}
      for
        http <- http.withUri(profile.endpointUriOfS3(req.bucket)).DELETE(req.key)
          .withParamOpt("versionId", req.versionId)
          .withHeaderOpt(req.`If-Match`           .map(_.toRaw1))
          .evalMapRequest(AwsSigV4.`UNSIGNED-PAYLOAD`(_, credentials, region, "s3"))

        result <- http.run.expectSuccess.resource.use: resp =>
          val label = "DeleteObject: response"
          inline def getSingleTextRequired(key: CIString) =
              resp.headers.getSingleTextRequired(key, label)
          inline def getSingleText(key: CIString) =
              resp.headers.getSingleText(key, label)
          inline def getSingleBoolean(key: CIString) =
              resp.headers.getSingleBoolean(key, label)
          for
            `x-amz-delete-marker`   <- getSingleBoolean     (ci"x-amz-delete-marker")
            `x-amz-version-id`      <- getSingleText        (ci"x-amz-version-id")
            `x-amz-request-charged` <- getSingleText        (ci"x-amz-request-charged")
            `x-amz-request-id`      <- getSingleTextRequired(ci"x-amz-request-id")
            `x-amz-id-2`            <- getSingleTextRequired(ci"x-amz-id-2")
          yield Response(
            `x-amz-delete-marker`,
            `x-amz-version-id`,
            `x-amz-request-charged`,
            `x-amz-request-id`,
            `x-amz-id-2`,
          )

      yield
        result

    case class Request(
      bucket    : String,
      key       : String,
      versionId : Option[String    ] = None,
      `If-Match`: Option[`If-Match`] = None,
    )

    case class Response(
      `x-amz-delete-marker`  : Option[Boolean],
      `x-amz-version-id`     : Option[String],
      `x-amz-request-charged`: Option[String],
      `x-amz-request-id`     : String,
      `x-amz-id-2`           : String,
    ) extends S3Response

  object DeleteObjects: // TODO XML rendering
    type Request = Delete

    case class Delete(
      Object: Chain[Delete.Object],
      Quite: Option[Boolean] = None,
    ):
      require(Quite.forall(_ == true)) // the value must be ture if added
    object Delete:
      case class Object(
        Key: String,
        ETag: Option[String],
        LastModifiedTime: Option[Timestamp],
        Size: Option[Long],
        VersionId: Option[String],
      )
