lazy val root = project
  .in(file("."))
  .settings(
    name         := "cats-playground",
    version      := "0.1",
    scalaVersion := "2.13.7",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.7.0"
    ),
  )

Compile / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "Xlog-reflective-calls",
  "-Xlint",
)
