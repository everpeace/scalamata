import sbt._
import sbt.Keys._

object ScalamataBuild extends Build {

  lazy val buildSettings = Project.defaultSettings ++ Seq(
      // name := "Automata in Scala",
      organization := "org.everpeace",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.9.1"
      // add other settings here
    )

  lazy val root = Project(
    id = "scalamata-root",
    base = file("."),
    settings = buildSettings,
    aggregate = Seq(core, examples) )

  lazy val core = Project(
    id = "scalamata",
    base = file("core"),
    settings = buildSettings )

  lazy val examples = Project(
    id = "scalamata-examples",
    base = file("examples"),
    dependencies = Seq(core),
    settings = buildSettings )
}
