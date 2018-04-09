val commonSettings = Seq(
  scalaVersion := "2.12.5"
)

val root = project in file(".")

val rpnCore = (project in file("rpn"))
  .settings(
    commonSettings,
    name := "rpn-core",
    version := "0.0.1",
    organization := "com.github.veinhorn",
    crossScalaVersions := Seq("2.11.12", "2.12.5"),
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.0.5" % Test,
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    ),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
  )

val rpnRepl = (project in file("rpn-repl"))
  .settings(
    name := "rpn-repl",
    scalaVersion := "2.11.12",
    resolvers += Resolver.bintrayRepo("veinhorn", "maven"),
    libraryDependencies ++= Seq(
      "com.github.veinhorn" %% "rpn-core" % "0.0.1"
    )
  )
  .enablePlugins(ConscriptPlugin)
