val rpn = (project in file("rpn"))
  .settings(
    name := "rpn-scala",
    version := "0.01",
    scalaVersion := "2.12.5",
    organization := "com.github.veinhorn",
    scalacOptions ++= Seq(
      "-deprecation"
    ),
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.0.5" % Test,
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )
