package jp.ukiba.koneko
package ko_aws

import jp.ukiba.koneko.ko_http4s.client.KoHttpClient
import jp.ukiba.koneko.ko_fs2.xml.{XmlParser, XmlTextTrimmer, XmlEventLog}

import org.http4s.Uri
import fs2.data.xml, xml.{XmlEvent, QName}, XmlEvent.{StartTag, EndTag}
import fs2.{Stream, Pipe, Pull, text}
import org.typelevel.log4cats.Logger
import cats.effect.{Sync, Async}
import cats.syntax.all.*
import cats.data.Chain

import java.time.Instant

/*
  2025-07
    https://github.com/gnieh/fs2-data is used since
    https://github.com/http4s/http4s-fs2-data has no releases for 16 months
*/
package object s3:
  def endpointOf(regionCode: String): Uri =
    Uri.unsafeFromString(s"https://s3.$regionCode.amazonaws.com/")

  // 
  /*
    https://docs.aws.amazon.com/AmazonS3/latest/API/API_Owner.html has the the wrong order of elements

      <Owner>
        <DisplayName>string</DisplayName>
        <ID>string</ID>
      </Owner>

    https://doc.s3.amazonaws.com/2006-03-01/AmazonS3.xsd agrees the actual response

      <xsd:sequence>
        <xsd:element name="ID" type="xsd:string"/>              
        <xsd:element name="DisplayName" type="xsd:string" minOccurs="0"/>
      </xsd:sequence>

    https://docs.aws.amazon.com/AmazonS3/latest/API/Welcome.html and
    https://docs.aws.amazon.com/AmazonS3/latest/API/WhatsNew.html says
    the latest API version is 2006-03-01 as of 2025-05.
  */
  import XmlParser.{Parser, expect, repPat, startTag, endTag, textOnlyTag, textOnlyTagOpt}

  case class ListAllMyBucketsResult(
    Owner: CanonicalUser,
    Buckets: Chain[ListAllMyBucketsEntry],
    ContinuationToken: Option[String],
    Prefix: Option[String],
  )
  object ListAllMyBucketsResult:
    def parser[F[_]: Sync]: Parser[F, ListAllMyBucketsResult] =
      for
        _                 <- startTag[F]("ListAllMyBucketsResult")
        owner             <- CanonicalUser.parser("Owner")
        buckets           <- ListAllMyBucketsList.parser("Buckets")
        continuationToken <- textOnlyTagOpt[F]("ContinuationToken")
        prefix            <- textOnlyTagOpt[F]("Prefix")
        _                 <- endTag[F]("ListAllMyBucketsResult")
      yield ListAllMyBucketsResult(owner, buckets.Bucket, continuationToken, prefix)

  case class ListAllMyBucketsList(
    Bucket: Chain[ListAllMyBucketsEntry],
  )
  object ListAllMyBucketsList:
    def parser[F[_]: Sync](local: String): Parser[F, ListAllMyBucketsList] =
      for
        _      <- startTag[F](local)
        bucket <- repPat:
          case head @ StartTag(QName(_, "Bucket"), _, _) => ListAllMyBucketsEntry.parser(head)
        _      <- endTag[F](local)
      yield ListAllMyBucketsList(bucket)

  // https://docs.aws.amazon.com/AmazonS3/latest/API/API_Bucket.html
  case class ListAllMyBucketsEntry(
    Name: String,
    CreationDate: String, // timestamp (doc 2019-12-11T23:32:47+00:00, actual 2024-11-13T14:52:58.000Z)
    BucketRegion: Option[String],
  )
  object ListAllMyBucketsEntry:
    def parser[F[_]: Sync](head: StartTag): Parser[F, ListAllMyBucketsEntry] =
      for
        name         <- textOnlyTag[F]("Name")
        creationDate <- textOnlyTag[F]("CreationDate")
        bucketRegion <- textOnlyTagOpt[F]("BucketRegion") // present when Request.`bucket-region` is present
        _            <- expect[F](EndTag(head.name))
      yield ListAllMyBucketsEntry(name, creationDate, bucketRegion)

  case class CanonicalUser(
    ID: String,
    DisplayName: Option[String], // will be missing since 2025-10 https://docs.aws.amazon.com/AmazonS3/latest/API/API_Owner.html
  )
  object CanonicalUser:
    def parser[F[_]: Sync](local: String): Parser[F, CanonicalUser] =
      for
        _           <- startTag[F](local)
        id          <- textOnlyTag[F]("ID")
        displayName <- textOnlyTagOpt[F]("DisplayName")
        _           <- endTag[F](local)
      yield CanonicalUser(id, displayName)

  object ListBuckets:
    def apply[F[_]: Async](profile: Aws.Profile)(http: KoHttpClient[F, ?])(req: Request,
        dontRepeatWithContinuationToken: Boolean = false)
        (using Logger[F]): F[Response] =
      import profile.{credentials, region}
      for
        http <- http.GET("")
          .withParamOpt("bucket-region"     , req.`bucket-region`)
          .withParamOpt("continuation-token", req.`continuation-token`)
          .withParamOpt("max-buckets"       , req.`max-buckets`)
          .withParamOpt("prefix"            , req.`prefix`)
          .evalMapRequest(AwsSigV4.unsignedPayload(_, credentials, region, "s3"))
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

    type Response = ListAllMyBucketsResult

  case class ListBucketResult(
    IsTruncated: Boolean,
    Contents: Chain[ListBucket.Contents],
    Name: String,
    Prefix: Option[String],
    Delimiter: Option[String],
    MaxKeys: Option[Int],
    CommonPrefixes: Chain[ListBucket.CommonPrefixes],
    EncodingType: Option[String],
    KeyCount: Option[String],
    ContinuationToken: Option[String],
    NextContinuationToken: Option[String],
    StartAfter: Option[String],
  ):
    
  object ListBucketResult:
    def parser[F[_]: Sync]: Parser[F, ListBucketResult] =
      for
        _                     <- startTag[F]("ListAllMyBucketsResult")
        isTruncated           <- textOnlyTag[F]("IsTruncated").map(_.toBoolean)
        contents              <- ListBucket.Content.parser("Contents")
        name                  <- textOnlyTag[F]("Name")
        prefix                <- textOnlyTagOpt[F]("Prefix")
        delimiter             <- textOnlyTagOpt[F]("Delimiter")
        maxKeys               <- textOnlyTagOpt[F]("MaxKeys").map(_.toInt)
        commonPrefixes        <- ListBucket.CommonPrefixes.parser("CommonPrefixes")
        encodingType          <- textOnlyTagOpt[F]("EncodingType")
        keyCount              <- textOnlyTagOpt[F]("KeyCount")
        continuationToken     <- textOnlyTagOpt[F]("ContinuationToken")
        nextContinuationToken <- textOnlyTagOpt[F]("NextContinuationToken")
        startAfter            <- textOnlyTagOpt[F]("StartAfter")
        _                     <- endTag[F]("ListAllMyBucketsResult")
      yield ListAllMyBucketsResult(isTruncated, contents, name, prefix, delimiter, maxKeys,
          )

    case class Contents()
    object Contents:
      def parser[F[_]: Sync]: Parser[F, Contents] = ???

    case class CommonPrefixes()
    object CommonPrefixes:
      def parser[F[_]: Sync]: Parser[F, CommonPrefixes] = ???

  object ListObjectsV2:
    def apply[F[_]: Async](profile: Aws.Profile)(http: KoHttpClient[F, ?])(req: Request,
        dontRepeatWithContinuationToken: Boolean = false)
        (using Logger[F]): F[Response] =
      import profile.{credentials, region}
      for
        http <- http.GET("")
          .withParam   ("list-type"         , 2)
          .withParamOpt("continuation-token", req.`continuation-token`)
          .withParamOpt("delimiter"         , req.`delimiter`)
          .withParamOpt("fetch-owner"       , req.`fetch-owner`)
          .withParamOpt("max-keys"          , req.`max-keys`)
          .withParamOpt("prefix"            , req.`prefix`)
          .withParamOpt("start-after"       , req.`start-after`)
          .evalMapRequest(AwsSigV4.unsignedPayload(_, credentials, region, "s3"))
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
        result <- (result.ContinuationToken, dontRepeatWithContinuationToken) match
          case (Some(continuationToken), false) => apply(profile)(http)
              (req.copy(`continuation-token` = Some(continuationToken)), false)
              .map(next => result.copy(Buckets = result.Buckets ++ next.Buckets, ContinuationToken = None))
          case _ => result.pure[F]

      yield
        result

    case class Request(
      Bucket              : String,
      `bucket-region`     : Option[String]  = None,
      `continuation-token`: Option[String]  = None,
      `delimiter`         : Option[Char]    = Some('/'),
      `encoding-type`     : Option[String]  = None,
      `fetch-owner`       : Option[Boolean] = None,
      `max-keys`          : Option[Int]    = None,
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

    type Response = ListBucketResult
