package jp.ukiba.koneko
package ko_aws

// https://docs.aws.amazon.com/global-infrastructure/latest/regions/aws-regions.html
// this is not enum since
// 1. it wouldn't be exhaustive over time
// 2. there could be a custom code by S3 compatible service
object AwsRegion:
  // Canonical
  inline val CapeTown        = "af-south-1"
  inline val HongKong        = "ap-east-1"
  inline val Tokyo           = "ap-northeast-1"
  inline val Seoul           = "ap-northeast-2"
  inline val Osaka           = "ap-northeast-3"
  inline val Mumbai          = "ap-south-1"
  inline val Hyderabad       = "ap-south-2"
  inline val Singapore       = "ap-southeast-1"
  inline val Sydney          = "ap-southeast-2"
  inline val Jakarta         = "ap-southeast-3"
  inline val Melbourne       = "ap-southeast-4"
  inline val Malaysia        = "ap-southeast-5"
  inline val Thailand        = "ap-southeast-7"
  //  Central Canada  = "ca-central-1"
  inline val Calgary         = "ca-west-1"
  inline val Frankfurt       = "eu-central-1"
  inline val Zurich          = "eu-central-2"
  inline val Stockholm       = "eu-north-1"
  inline val Milan           = "eu-south-1"
  inline val Spain           = "eu-south-2"
  inline val Ireland         = "eu-west-1"
  inline val London          = "eu-west-2"
  inline val Paris           = "eu-west-3"
  inline val TelAviv         = "il-central-1"
  inline val UAE             = "me-central-1"
  inline val Bahrain         = "me-south-1"
  //  Central Mexico  = "mx-central-1"
  inline val SãoPaulo        = "sa-east-1"
  inline val NorthVirginia   = "us-east-1" // N. Virginia
  inline val Ohio            = "us-east-2"
  inline val NorthCalifornia = "us-west-1" // N. California
  inline val Oregon          = "us-west-2"

  // by country, for "ca-central" and "mx-central"
  object Australia:
    val Sydney          = "ap-southeast-2"
    val Melbourne       = "ap-southeast-4"
  // Bahrain
  object Brazil:
    val SãoPaulo        = "sa-east-1"
  object Canada:
    val Central         = "ca-central-1"
    val Calgary         = "ca-west-1"
  object France:
    val Paris           = "eu-west-3"
  object Germany:
    val Frankfurt       = "eu-central-1"
  // HongKong
  object India:
    val Mumbai          = "ap-south-1"
    val Hyderabad       = "ap-south-2"
  object Indonesia:
    val Jakarta         = "ap-southeast-3"
  // Ireland
  object Israel:
    val TelAviv         = "il-central-1"
  object Italy:
    val Milan           = "eu-south-1"
  object Japan:
    val Tokyo           = "ap-northeast-1"
    val Osaka           = "ap-northeast-3"
  // Malaysia
  object Mexico:
    val Central         = "mx-central-1"
  // Singapore
  object SouthAfrica:
    val CapeTown        = "af-south-1"
  object SouthKorea:
    val Seoul           = "ap-northeast-2"
  // Spain
  object Sweden:
    val Stockholm       = "eu-north-1"
  object Switzerland:
    val Zurich          = "eu-central-2"
  // Thailand
  // UAE
  object UK:
    val London          = "eu-west-2"
  object US:
    val NorthVirginia   = "us-east-1"
    val Ohio            = "us-east-2"
    val NorthCalifornia = "us-west-1"
    val Oregon          = "us-west-2"

  object GovCloud:
    object US:
      val East          = "us-gov-east-1"
      val West          = "us-gov-west-1"

  /** Japanese names */
  object Jp:
    inline val ケープタウン     = CapeTown
    inline val 香港             = HongKong
    inline val 東京             = Tokyo
    inline val ソウル           = Seoul
    inline val 大阪             = Osaka
    inline val ムンバイ         = Mumbai
    inline val ハイデラバード   = Hyderabad
    inline val シンガポール     = Singapore
    inline val シドニー         = Sydney
    inline val ジャカルタ       = Jakarta
    inline val メルボルン       = Melbourne
    inline val マレーシア       = Malaysia
    inline val タイ             = Thailand
    //  Central Canada   = "ca-central-1"
    inline val カルガリー       = Calgary
    inline val フランクフルト   = Frankfurt
    inline val チューリッヒ     = Zurich
    inline val ストックホルム   = Stockholm
    inline val ミラノ           = Milan
    inline val スペイン         = Spain
    inline val アイルランド     = Ireland
    inline val ロンドン         = London
    inline val パリ             = Paris
    inline val テルアビブ       = TelAviv
    inline val アラブ首長国連邦 = UAE
    inline val バーレーン       = Bahrain
    //  Central Mexico   = "mx-central-1"
    inline val サンパウロ       = SãoPaulo
    inline val 北バージニア     = NorthVirginia
    inline val オハイオ         = Ohio
    inline val 北カリフォルニア = NorthCalifornia
    inline val オレゴン         = Oregon
