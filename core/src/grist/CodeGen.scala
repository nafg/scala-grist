package grist

import scala.annotation.unused

import zio.ZIO

//noinspection ScalaUnusedSymbol
trait CodeGen {
  // noinspection ScalaWeakerAccess
  protected def includeTable(@unused tableId: String): Boolean = true

  protected def beforeEachClass: String
  protected def afterEachClass(className: String, columnInfos: ColumnInfo*): String

  case class Choices(typeName: String, definition: String)

  protected def mkChoices(className: String, fieldName: String, choices: Seq[String]): Choices

  case class ColumnInfo(name: String, `type`: String, outsideCode: Option[String])

  def run(documentId: String): ZIO[GristClient, Throwable, List[String]] =
    ZIO
      .serviceWithZIO[GristClient] { gristClient =>
        val docApi = gristClient.docs(documentId)

        def tableCode(tableId: String): ZIO[Any, Throwable, String] =
          docApi
            .tables(tableId)
            .columns
            .list
            .map { columns =>
              val columnInfos =
                columns.columns.collect {
                  case Models.Column(columnId, fields) if !fields.isFormula =>
                    def choices =
                      mkChoices(
                        className = tableId,
                        fieldName = columnId,
                        choices = fields.widgetOptions
                          .flatMap(_.choices)
                          .getOrElse(Set.empty[String])
                          .toSeq
                          .sorted
                      )

                    val (scalaType, maybeExtra) =
                      fields.`type` match {
                        case Models.FieldType.Any              => "Any"           -> None
                        case Models.FieldType.Text             => "String"        -> None
                        case Models.FieldType.Numeric          => "Double"        -> None
                        case Models.FieldType.Int              => "Int"           -> None
                        case Models.FieldType.Bool             => "Boolean"       -> None
                        case Models.FieldType.Date             => "LocalDate"     -> None
                        case Models.FieldType.DateTime(_)      => "LocalDateTime" -> None
                        case Models.FieldType.Choice           =>
                          choices.typeName -> Some(choices.definition)
                        case Models.FieldType.ChoiceList       =>
                          s"Seq[${choices.typeName}]" -> Some(choices.definition)
                        case Models.FieldType.Ref(tableId)     =>
                          s"Reference[${tableId}Fields]" -> None
                        case Models.FieldType.RefList(tableId) =>
                          s"Seq[Reference[${tableId}Fields]]" -> None
                        case Models.FieldType.Attachments      =>
                          "RefList" -> None
                      }
                    val optionType              = s"Option[$scalaType]"
                    ColumnInfo(columnId, optionType, maybeExtra)
                }

              val className = s"${tableId}Fields"
              beforeEachClass + "\n" +
                columnInfos
                  .map { case ColumnInfo(id, optionType, _) =>
                    val sep = if (id.last.isLetterOrDigit) "" else " "
                    s"$id$sep: $optionType = None"
                  }
                  .mkString(s"case class $className(\n  ", ",\n  ", "\n)\n") +
                afterEachClass(className, columnInfos*)
            }

        for {
          docs <- docApi.tables.list
          res  <- ZIO.foreach(docs.tables) { case Models.Table(tableId, _) =>
                    ZIO.when(includeTable(tableId)) {
                      tableCode(tableId)
                    }
                  }
        } yield res.flatten
      }
}
