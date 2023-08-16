package grist

import io.circe.{Decoder, Encoder, Json, parser}
import io.circe.generic.JsonCodec
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

import scala.util.Success

object Models {
  @JsonCodec
  case class Record(id: Int, fields: Map[String, Json])

  @JsonCodec
  case class Records(records: List[Record])

  @JsonCodec
  case class RecordWithoutId(fields: Map[String, Json])

  @JsonCodec
  case class RecordsWithoutId(records: List[RecordWithoutId])

  @JsonCodec
  case class RecordWithoutFields(id: Int)

  @JsonCodec
  case class RecordsWithoutFields(records: List[RecordWithoutFields])

  sealed trait FieldType
  // noinspection ScalaUnusedSymbol
  object FieldType {
    private case object Any                       extends FieldType
    private case object Text                      extends FieldType
    private case object Numeric                   extends FieldType
    private case object Int                       extends FieldType
    private case object Bool                      extends FieldType
    private case object Date                      extends FieldType
    private case class DateTime(timezone: String) extends FieldType
    private case object Choice                    extends FieldType
    private case object ChoiceList                extends FieldType
    private case class Ref(tableId: String)       extends FieldType
    private case class RefList(tableId: String)   extends FieldType
    private case object Attachments               extends FieldType

    implicit val decodeFieldType: Decoder[FieldType]          = Decoder[String].emap {
      case "Any"                 => Right(Any)
      case "Text"                => Right(Text)
      case "Numeric"             => Right(Numeric)
      case "Int"                 => Right(Int)
      case "Bool"                => Right(Bool)
      case "Date"                => Right(Date)
      case s"DateTime:$timezone" => Right(DateTime(timezone))
      case "Choice"              => Right(Choice)
      case s"ChoiceList"         => Right(ChoiceList)
      case s"Ref:$tableId"       => Right(Ref(tableId))
      case s"RefList:$tableId"   => Right(RefList(tableId))
      case "Attachments"         => Right(Attachments)
      case other                 => Left(s"Unknown field type: $other")
    }
    implicit val encodeFieldType: io.circe.Encoder[FieldType] =
      io.circe.Encoder[String].contramap {
        case Any                => "Any"
        case Text               => "Text"
        case Numeric            => "Numeric"
        case Int                => "Int"
        case Bool               => "Bool"
        case Date               => "Date"
        case DateTime(timezone) => s"DateTime:$timezone"
        case Choice             => "Choice"
        case ChoiceList         => "ChoiceList"
        case Ref(tableId)       => s"Ref:$tableId"
        case RefList(tableId)   => s"RefList:$tableId"
        case Attachments        => "Attachments"
      }
  }

  sealed trait RecalcWhen
  // noinspection ScalaUnusedSymbol
  object RecalcWhen    {
    private case object OnNewRecordsOrRecalcDepsChange extends RecalcWhen
    private case object Never                          extends RecalcWhen
    private case object OnNewRecordsOrDataChange       extends RecalcWhen

    implicit val decodeRecalcWhen: Decoder[RecalcWhen]          = Decoder[Int].emap {
      case 0     => Right(OnNewRecordsOrRecalcDepsChange)
      case 1     => Right(Never)
      case 2     => Right(OnNewRecordsOrDataChange)
      case other =>
        Left(s"Unknown recalc when: $other")
    }
    implicit val encodeRecalcWhen: io.circe.Encoder[RecalcWhen] =
      io.circe.Encoder[Int].contramap {
        case OnNewRecordsOrRecalcDepsChange => 0
        case Never                          => 1
        case OnNewRecordsOrDataChange       => 2
      }
  }

  case class WidgetOptions(
    widget: String,
    isCustomDateFormat: Option[Boolean],
    isCustomTimeFormat: Option[Boolean],
    alignment: Option[WidgetOptions.Alignment],
    dateFormat: Option[String],
    timeFormat: Option[String],
    choices: Option[Set[String]],
    choiceOptions: Option[WidgetOptions.ChoiceOptions],
    wrap: Option[Boolean]
  )
  object WidgetOptions {
    @JsonCodec
    case class ChoiceOptions()
    sealed trait Alignment
    // noinspection ScalaUnusedSymbol
    object Alignment {
      private case object Left extends Alignment
      implicit val decodeAlignment: Decoder[Alignment] = Decoder[String].emap {
        case "left" => Right(Left)
        case other  => scala.Left(s"Unknown alignment: $other")
      }
      implicit val encodeAlignment: Encoder[Alignment] =
        Encoder[String].contramap { case Left =>
          "left"
        }
    }
    implicit val decodeWidgetOptions: Decoder[Option[WidgetOptions]] =
      Decoder[String].emapTry {
        case ""     => Success(None)
        case string =>
          parser
            .parse(string)
            .flatMap { json =>
              val keys  = json.asObject.map(_.keys.toSet).getOrElse(Set.empty)
              val extra =
                keys -- Set(
                  "widget",
                  "isCustomDateFormat",
                  "isCustomTimeFormat",
                  "alignment",
                  "dateFormat",
                  "timeFormat",
                  "choices",
                  "choiceOptions",
                  "wrap"
                )
              assert(extra.isEmpty, s"Unknown keys: ${extra.mkString(", ")}")
              deriveDecoder[WidgetOptions]
                .decodeJson(json)
                .map(Some(_))
            }
            .toTry
      }
    implicit val encodeWidgetOptions: Encoder[WidgetOptions] =
      Encoder[String].contramap { widgetOptions =>
        deriveEncoder[WidgetOptions].apply(widgetOptions).noSpaces
      }
  }

  @JsonCodec
  case class GetFields(
    `type`: FieldType,
    label: String,
    formula: String,
    isFormula: Boolean,
    widgetOptions: Option[WidgetOptions],
    untieColIdFromLabel: Boolean,
    recalcWhen: RecalcWhen,
    visibleCol: Int,
    recalcDeps: Option[List[String]],
    colRef: Int
  )

  @JsonCodec
  case class Column(id: String, fields: GetFields)

  @JsonCodec
  case class Columns(columns: List[Column])
}
