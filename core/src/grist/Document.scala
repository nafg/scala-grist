package grist

import cats.implicits.{toShow, toTraverseOps}
import io.circe.*
import io.circe.syntax.EncoderOps
import zio.{Task, ZIO}

//noinspection ScalaUnusedSymbol
class Document(gristClient: GristClient, docId: String) extends DocumentBase {
  private def docApi = gristClient.docs(docId)

  case class Table[A](tableName: String) extends TableBase[A] {
    private def tableApi = docApi.tables(tableName)

    override def getRecords(
      filter: Map[String, Set[Json]] = Map.empty
    )(implicit decoder: Decoder[A]): Task[List[Record[A]]] =
      for {
        records <- tableApi.records.list(filter)
        records <- ZIO.fromEither(
                     records.records
                       .traverse({ case Models.Record(id, fields) =>
                         decoder
                           .decodeAccumulating(HCursor.fromJson(fields.asJson))
                           .map(a => Record(id, a))
                       })
                       .leftMap { failures =>
                         failures.toList.foreach(e => Console.err.println(e.show))
                         failures.head
                       }
                       .toEither
                   )
      } yield records

    override def getRecordsById(ids: Set[Reference[A]])(implicit decoder: Decoder[A]): Task[List[Record[A]]] =
      if (ids.isEmpty)
        ZIO.succeed(Nil)
      else
        getRecords(filter = Map("id" -> ids.map(_.asJson)))

    override def getRecordById(id: Reference[A])(implicit decoder: Decoder[A]): Task[A] =
      getRecordsById(Set(id))
        .flatMap(response => ZIO.getOrFail(response.lastOption).map(_.fields))

    override def addRecords(records: Seq[A])(implicit encoder: Encoder.AsObject[A]): Task[List[Reference[A]]] =
      for {
        ids <- tableApi.records.add(Models.RecordsWithoutId(records.toList.map { a =>
                 Models.RecordWithoutId(a.asJsonObject.toMap)
               }))
      } yield ids.records.map { case Models.RecordWithoutFields(id) =>
        Reference[A](id)
      }

    override def addRecordRaw(record: Map[String, Json]): Task[Int] =
      for {
        results <- tableApi.records.add(Models.RecordsWithoutId(List(Models.RecordWithoutId(record))))
        result  <- ZIO.getOrFail(results.records.headOption)
      } yield result.id

    override def deleteRecords(ids: Set[Reference[A]]): Task[Unit] =
      for {
        _ <- tableApi.data.delete(ids.map(_.id))
      } yield ()
  }
}
