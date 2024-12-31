import $ivy.`io.chris-kipp::mill-ci-release::0.2.0`
import $ivy.`io.github.nafg.scalac-options::scalac-options:0.3.0`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import io.kipp.mill.ci.release.CiReleaseModule
import mill.scalalib.publish.{Developer, License, PomSettings, VersionControl}
import mill.scalalib.scalafmt.ScalafmtModule
import mill.scalalib.{DepSyntax, ScalaModule}
import mill.{Agg, Cross, T}
import io.github.nafg.scalacoptions.{ScalacOptions, WarningsConfig, options}

object core      extends Cross[CoreModule]("3.4.2", "2.13.14")
trait CoreModule extends Cross.Module[String] with ScalaModule with CiReleaseModule with ScalafmtModule {
  override def scalaVersion = crossValue

  override def scalacOptions =
    super.scalacOptions() ++
      ScalacOptions.all(scalaVersion())(
        (o: options.Common) =>
          o.deprecation ++
            o.feature ++
            o.unchecked,
        (o: options.V2) => o.Xsource("3"),
        (o: options.V2_13) => o.Xlint("_"),
        WarningsConfig(
          (WarningsConfig.Filter.Category(WarningsConfig.Category.`lint-byname-implicit`) &
            WarningsConfig.Filter.Site(".*\\.((decode|encode|codec).+)".r)).silent
        )
      )

  override def ivyDeps =
    Agg(ivy"io.circe::circe-parser:0.14.10", ivy"io.circe::circe-generic:0.14.10", ivy"dev.zio::zio-http:3.0.1")

  override def artifactName = "grist-core"

  override def publishVersion: T[String] = T {
    VcsVersion
      .vcsState()
      .format(dirtyHashDigits = 0, untaggedSuffix = "-SNAPSHOT")
  }

  override def pomSettings = PomSettings(
    description = "Bundler for Mill",
    organization = "io.github.nafg.grist",
    url = "https://github.com/nafg/scala-grist",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github(owner = "nafg", repo = "scala-grist"),
    developers = Seq(Developer("nafg", "Naftoli Gugenheim", "https://github.com/nafg"))
  )
}
