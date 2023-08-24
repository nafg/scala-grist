package grist

import cats.implicits.toInvariantOps
import io.circe.{Codec, Decoder, Encoder}

//noinspection ScalaUnusedSymbol
case class Reference[A](id: Int) extends AnyVal {
  def get(records: Seq[Record[A]]): Option[Record[A]] = records.find(_.id == id)

  def is(record: Record[A]) = record.id == id
}

object Reference {
  implicit def codecRef[A]: Codec[Reference[A]] = Codec.from(Decoder[Int], Encoder[Int]).imap(Reference[A])(_.id)

  implicit def decodeSeqRef[A]: Decoder[Seq[Reference[A]]] =
    Decoder[Option[RefList]].map {
        case Some(RefList(ids)) => ids.map(Reference(_))
        case None               => Nil
    }
}
