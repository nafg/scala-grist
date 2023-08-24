package grist

import io.circe.{Decoder, Encoder, Json}

case class RefList(ids: Seq[Int]) extends AnyVal

object RefList {
  implicit val decodeRefList: Decoder[RefList] =
    Decoder[Seq[Int]]
      .prepare { cursor =>
        if (cursor.downArray.focus.contains(Json.fromString("L")))
          cursor.downArray.delete
        else
          cursor
      }
      .map(RefList(_))

  implicit val encodeRefList: Encoder[RefList] =
    Encoder[Seq[Int]]
      .contramap[RefList](_.ids)
      .mapJson(_.mapArray(arr => Json.fromString("L") +: arr))
}
