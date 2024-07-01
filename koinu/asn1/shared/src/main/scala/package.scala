package jp.ukiba.koinu

import jp.ukiba.koinu.ko_java.ExceptionBase

import scodec.bits.ByteVector

/** Abstract Syntax Notation One */
package object asn1:
  trait ASN1Encoding[A]:
    /** @return An encoded binary */
    def encode(value: A): ByteVector

    /** @return The decoded value and the undecoded remaining */
    def decode(bin: ByteVector): (A, ByteVector)

  //type IdentifierOctet = Byte // byte is 8-bit as octet in Java

  enum TagClass(val value: Int):
    case Universal       extends TagClass(0x00)
    case Application     extends TagClass(0x40)
    case ContextSpecific extends TagClass(0x80)
    case Private         extends TagClass(0xc0)

  enum TagConstructed(val value: Int):
    case Primitive   extends TagConstructed(0x00)
    case Constructed extends TagConstructed(0x20)

  enum TagNumber(val value: Int):
    // reserved                                            0x00
    case BooleanType                     extends TagNumber(0x01)
    case IntegerType                     extends TagNumber(0x02)
    case BitStringType                   extends TagNumber(0x03)
    case OctetStringType                 extends TagNumber(0x04)
    case NullType                        extends TagNumber(0x05)
    case ObjectIdentifierType            extends TagNumber(0x06)
    case ObjectDescriptorType            extends TagNumber(0x07)
    case ExternalType                    extends TagNumber(0x08)
    case RealType                        extends TagNumber(0x09)
    case EnumeratedType                  extends TagNumber(0x0a) // 10
    case EmbeddedPDVType                 extends TagNumber(0x0b) // 11
    case UTF8StringType                  extends TagNumber(0x0c) // 12
    case RelativeOIDType                 extends TagNumber(0x0d) // 13
    case TimeType                        extends TagNumber(0x0e) // 14
    // reserved                                            0x0f  // 15
    case SequenceType                    extends TagNumber(0x10) // 16
    case SetType                         extends TagNumber(0x11) // 17
    case NumericStringType               extends TagNumber(0x12) // 18
    case PrintableStringType             extends TagNumber(0x13) // 19
    case TeletexStringType               extends TagNumber(0x14) // 20 // T61String
    case VideotexStringType              extends TagNumber(0x15) // 21
    case IA5StringType                   extends TagNumber(0x16) // 22
    case UTCTimeType                     extends TagNumber(0x17) // 23
    case GeneralizedTimeType             extends TagNumber(0x18) // 24
    case GraphicStringType               extends TagNumber(0x19) // 25
    case VisibleStringType               extends TagNumber(0x1a) // 26 // ISO646String
    case GeneralStringType               extends TagNumber(0x1b) // 27
    case UniversalStringType             extends TagNumber(0x1c) // 28
    case UnrestrictedCharacterStringType extends TagNumber(0x1d) // 29
    case BMPStringType                   extends TagNumber(0x1e) // 30
    case DateType                        extends TagNumber(0x1f) // 31
    case TimeOfDayType                   extends TagNumber(0x20) // 32
    case DateTimeType                    extends TagNumber(0x21) // 33
    case IRIType                         extends TagNumber(0x22) // 34
    case RelativeIRIType                 extends TagNumber(0x23) // 35
    case DurationType                    extends TagNumber(0x24)
    // reserved                                            0x25

    val InstanceOfType = ExternalType
    val SequenceOfType = SequenceType
    val SetOfType = SetType

  class ASN1DecodeException(message: String, cause: Throwable = null) extends ExceptionBase(message, cause)
