name := "sgitl"

organization := "com.github.reggert"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:implicitConversions")

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

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"