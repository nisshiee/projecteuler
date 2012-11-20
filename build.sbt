import AssemblyKeys._

name := "projecteuler"

organization := "org.nisshiee"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies := Seq(
   "org.scalaz" %% "scalaz-core" % "7.0.0-M4"
  ,"com.typesafe.akka" % "akka-actor" % "2.0.2"
  ,"com.github.scala-incubator.io" %% "scala-io-core" % "0.4.1"
  ,"com.github.scala-incubator.io" %% "scala-io-file" % "0.4.1"
  ,"org.scala-tools.time" % "time_2.9.1" % "0.5"
  ,"joda-time" % "joda-time" % "2.1"
  ,"org.joda" % "joda-convert" % "1.2"
  ,"org.specs2" %% "specs2" % "1.12.3" % "test"
  ,"org.mockito" % "mockito-all" % "1.9.0" % "test"
  ,"junit" % "junit" % "4.10" % "test"
  ,"org.pegdown" % "pegdown" % "1.1.0" % "test"
)

testOptions in (Test, test) += Tests.Argument("console", "html", "junitxml")

initialCommands := """
import scalaz._
import Scalaz._
import scalax.io._
import scalax.file._
import ImplicitConversions._
import org.scala_tools.time.Imports._
"""


// ========== for sonatype oss publish ==========

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/nisshiee/projecteuler</url>
  <licenses>
    <license>
      <name>The MIT License (MIT)</name>
      <url>http://opensource.org/licenses/mit-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:nisshiee/projecteuler.git</url>
    <connection>scm:git:git@github.com:nisshiee/projecteuler.git</connection>
  </scm>
  <developers>
    <developer>
      <id>nisshiee</id>
      <name>Hirokazu Nishioka</name>
      <url>http://nisshiee.github.com/</url>
    </developer>
  </developers>)


// ========== for scaladoc ==========

// scaladocOptions in (Compile, doc) <++= (baseDirectory in LocalProject("core")).map {

scaladocOptions in (Compile, doc) <++= baseDirectory.map {
  bd => Seq("-sourcepath", bd.getAbsolutePath,
            "-doc-source-url", "https://github.com/nisshiee/projecteuler/blob/master/core€{FILE_PATH}.scala")
}


// ========== for sbt-assembly ==========

seq(assemblySettings: _*)

jarName in assembly <<= (name, version) { (name, version) => name + "-" + version + ".jar" }

// test in assembly := {}

mainClass in assembly := Some("App")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "application.conf" => MergeStrategy.concat
    case x => old(x)
  }
}
