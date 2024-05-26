package grist

import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.semiauto.deriveCodec

case class Record[A](id: Int, fields: A) {
  def ref = Reference[A](id)
}
object Record                            {
  implicit def codecRecord[A: Decoder: Encoder]: Codec[Record[A]] = deriveCodec[Record[A]]
}
