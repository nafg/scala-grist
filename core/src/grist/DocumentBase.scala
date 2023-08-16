package grist

import io.circe.{Decoder, Encoder, Json}
import zio.Task

trait DocumentBase {
  trait TableBase[A] {
    def getRecords(filter: Map[String, Set[Json]] = Map.empty)(implicit decoder: Decoder[A]): Task[List[Record[A]]]

    def getRecordsById(ids: Set[Reference[A]])(implicit decoder: Decoder[A]): Task[List[Record[A]]]

    def getRecordById(id: Reference[A])(implicit decoder: Decoder[A]): Task[A]

    def addRecords(records: Seq[A])(implicit encoder: Encoder.AsObject[A]): Task[List[Reference[A]]]

    def addRecordRaw(record: Map[String, Json]): Task[Int]

    def deleteRecords(ids: Set[Reference[A]]): Task[Unit]
  }
}
