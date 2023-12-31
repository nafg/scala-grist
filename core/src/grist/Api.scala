package grist

import cats.implicits.toShow
import io.circe.syntax.EncoderOps
import io.circe.*
import zio.http.*
import zio.{Task, ZIO}

abstract class Api(baseClient: Client) {
  private val printer    = Printer.noSpaces.copy(dropNullValues = true)
  private val jsonClient = baseClient.contramap[Json](json => Body.fromString(printer.print(json)))

  private def withQueryParams[Env, In, Err, Out](client: ZClient[Env, In, Err, Out], queryParams: QueryParams) =
    client.url(client.url.queryParams(queryParams))

  private def parse[R: Decoder](client: ZClient[?, ?, ?, ?])(response: Response): Task[R] =
    response.body.asString.flatMap { str =>
      ZIO.fromEither(
        jawn
          .decodeAccumulating(str)
          .leftMap { error =>
            new Exception(s"""${error.map(_.show).toList.mkString("\n")}
                 |URL: ${client.url.encode}
                 |Response: $str""".stripMargin)
          }
          .toEither
      )
    }

  protected def get[R: Decoder](queryParams: QueryParams = QueryParams.empty): Task[R] = ZIO.scoped {
    val client = withQueryParams(baseClient, queryParams)
    client.get("").flatMap(parse[R](client))
  }

  protected def post[P: Encoder, R: Decoder](body: P, queryParams: QueryParams = QueryParams.empty): Task[R] =
    ZIO.scoped {
      val client = withQueryParams(jsonClient, queryParams)
      client.post("")(body.asJson).flatMap(parse[R](client))
    }

  abstract class WithPath(suffix: String) extends Api(baseClient.addPath(suffix))
}
