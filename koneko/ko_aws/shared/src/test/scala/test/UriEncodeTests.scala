package jp.ukiba.koneko
package ko_aws

import jp.ukiba.koneko.ko_munit.KoCatsEffectSuite

import org.http4s.Uri
import org.typelevel.log4cats.{Logger, LoggerFactory}
import cats.syntax.all.*

class UriEncodeTests extends KoCatsEffectSuite:
  given Logger[F] = LoggerFactory[F].getLogger

  nest("http4s.Uri.pathEncode"):
    // https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html
    // RFC 3986 unreserved characters only, uppercase hex
    test("should be the same as AWS encoding rule"):
      for
        encoded <- F.pure:
          Aws.uriEncode:
            " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
      yield
        assertEquals(Seq(
          // encode except the unreserved characters: 'A'-'Z', 'a'-'z', '0'-'9', '-', '.', '_', and '~'
          // space is a reserved character and encoded as "%20" (and not as "+")
          "%20%21%22%23%24%25%26%27%28%29%2A%2B%2C-.%2F",
          "0123456789",
          "%3A%3B%3C%3D%3E%3F%40",
          "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
          "%5B%5C%5D%5E_%60",
          "abcdefghijklmnopqrstuvwxyz",
          "%7B%7C%7D~",
        ).mkString, encoded)
