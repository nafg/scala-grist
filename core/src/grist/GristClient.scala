package grist

import io.circe.Json
import io.circe.syntax.EncoderOps
import zio.http.*
import zio.{Task, ZIO, ZLayer}

class GristClient(client: Client) extends Api(client) {
  object docs extends WithPath("docs") {
    def apply(docId: String) = new DocApi(docId)

    class DocApi(docId: String) extends WithPath(docId) {
      object tables extends WithPath("tables") {
        def apply(tableName: String) = new TableApi(tableName)

        class TableApi(tableName: String) extends WithPath(tableName) {
          object records extends WithPath("records") {
            def list(filter: Map[String, Set[Json]]): Task[Models.Records] =
              get[Models.Records](QueryParams("filter" -> filter.asJson.noSpaces))

            def add(records: Models.RecordsWithoutId): Task[Models.RecordsWithoutFields] =
              post[Models.RecordsWithoutId, Models.RecordsWithoutFields](records)
          }

          object data extends WithPath("data") {
            object delete extends WithPath("delete") {
              def apply(ids: Set[Int]): Task[Unit] =
                post[Set[Int], Unit](ids)
            }
          }

          object columns extends WithPath("columns") {
            def list = get[Models.Columns]()
          }
        }
      }
    }
  }
}

object GristClient {
  def apply(teamName: String, apiToken: String): ZLayer[Client, Nothing, GristClient] =
    ZLayer.fromFunction { (client: Client) =>
      new GristClient(
        client
          .url(
            URL(
              kind = URL.Location
                .Absolute(Scheme.HTTPS, s"$teamName.getgrist.com", 443),
              path = Path.root / "api"
            )
          )
          .addHeaders(Headers(Header.Authorization.Bearer(apiToken)))
      )
    }

  def client = ZIO.service[GristClient]
}
