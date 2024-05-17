import $ivy.`io.chris-kipp::mill-ci-release::0.1.10`

import de.tobiasroeser.mill.vcs.version.VcsVersion
import io.kipp.mill.ci.release.CiReleaseModule
import mill.{Agg, T}
import mill.scalalib.{DepSyntax, ScalaModule}
import mill.scalalib.publish.{Developer, License, PomSettings, VersionControl}
import mill.scalalib.scalafmt.ScalafmtModule

object core extends ScalaModule with CiReleaseModule with ScalafmtModule {
  override def scalaVersion = "2.13.14"

  override def scalacOptions =
    super.scalacOptions() ++ Seq(
      "-Ymacro-annotations",
      "-Xsource:3",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xlint",
      "-Wconf:cat=lint-byname-implicit&site=.*\\.((decode|encode|codec).+):silent"
    )

  override def ivyDeps =
    Agg(ivy"io.circe::circe-parser:0.14.7", ivy"io.circe::circe-generic:0.14.6", ivy"dev.zio::zio-http:3.0.0-RC7")

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
