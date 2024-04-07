ThisBuild / version := "0.0.2"

ThisBuild / scalaVersion := "3.3.3"

githubOwner      := "rinotc"
githubRepository := "smatrix"
lazy val root = (project in file("."))
  .settings(
    name := "smatrix",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.17" % Test
    ),
    publishMavenStyle    := true,
    pomIncludeRepository := { _ => false },
    publishTo            := Some("rinotc Apache Maven Packages" at "https://maven.pkg.github.com/rinotc/smatrix"),
    credentials += Credentials(
      "GitHub Package Registry",
      "https://maven.pkg.github.com",
      "rinotc",
      sys.env.getOrElse("GITHUB_TOKEN", "N/A")
    )
  )
