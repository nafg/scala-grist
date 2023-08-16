package grist

import io.circe.generic.JsonCodec

@JsonCodec
case class Record[A](id: Int, fields: A) {
  def ref = Reference[A](id)
}
