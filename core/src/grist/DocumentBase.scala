package grist

import io.circe.Json
import zio.Task

trait DocumentBase {
  trait TableBase[A] {
    def getRecords(filter: Map[String, Set[Json]] = Map.empty): Task[List[Record[A]]]

    def getRecordsById(ids: Set[Reference[A]]): Task[List[Record[A]]]

    def getRecordById(id: Reference[A]): Task[A]

    def addRecords(records: Seq[A]): Task[List[Reference[A]]]

    def addRecordRaw(record: Map[String, Json]): Task[Int]

    def deleteRecords(ids: Set[Reference[A]]): Task[Unit]
  }
}
