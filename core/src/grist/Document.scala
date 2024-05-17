package grist

import cats.data.Validated
import cats.implicits.{toFunctorFilterOps, toShow}
import io.circe.*
import io.circe.syntax.EncoderOps
import zio.http.MediaType
import zio.stream.UStream
import zio.{Task, ZIO}

//noinspection ScalaUnusedSymbol
class Document(gristClient: GristClient, docId: String) extends DocumentBase {
  private def docApi = gristClient.docs(docId)

  case class Table[A: Decoder: Encoder](tableName: String) extends TableBase[A] {
    private def tableApi = docApi.tables(tableName)

    override def getRecords(filter: Map[String, Set[Json]] = Map.empty): Task[List[Record[A]]] =
      for {
        records <- tableApi.records.list(filter)
        res     <- ZIO.foreach(records.records) { case Models.Record(id, fields) =>
                     val json = fields.asJson
                     Decoder[A].decodeAccumulating(HCursor.fromJson(json)) match {
                       case Validated.Valid(a)          => ZIO.some(Record(id, a))
                       case Validated.Invalid(failures) =>
                         ZIO.foreachDiscard(failures.toList) { e =>
                           ZIO.logError(s"${e.show}, original JSON: ${json.spaces2}")
                         } *>
                           ZIO.none
                     }
                   }
      } yield res.flattenOption

    override def getRecordsById(ids: Set[Reference[A]]): Task[List[Record[A]]] =
      if (ids.isEmpty)
        ZIO.succeed(Nil)
      else
        getRecords(filter = Map("id" -> ids.map(_.asJson)))

    override def getRecordById(id: Reference[A]): Task[A] =
      getRecordsById(Set(id))
        .flatMap(response => ZIO.getOrFail(response.lastOption).map(_.fields))

    override def addRecords(records: Seq[A]): Task[List[Reference[A]]] =
      for {
        ids <- tableApi.records.add(Models.RecordsWithoutId(records.toList.map { a =>
                 Models.RecordWithoutId(a.asJson.asObject.map(_.toMap).getOrElse(Map.empty))
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

  def uploadAttachment(data: UStream[Byte], mediaType: MediaType, filename: Option[String] = None) =
    docApi.attachments.upload(GristClient.AttachmentInfo(data, mediaType, filename))
}
