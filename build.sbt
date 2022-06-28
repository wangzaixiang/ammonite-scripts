organization := "com.github.wangzaixiang"

name := "ammonite-scripts"

version := "0.1.3"

scalaVersion := "2.13.5"

libraryDependencies ++= Seq(
    "com.lihaoyi" %% "ammonite-ops" % "2.3.8",
)

publishMavenStyle := true

publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
    else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishConfiguration  := publishConfiguration.value.withOverwrite(true)

test/publishArtifact := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/wangzaixiang/ammonite-scripts</url>
    <scm>
        <connection>scm:git:https://github.com/wangzaixiang/ammonite-scripts.git</connection>
        <developerConnection>scm:git:https://github.com/wangzaixiang/ammonite-scripts.git</developerConnection>
        <url>https://github.com/wangzaixiang/ammonite-scripts</url>
    </scm>
    <licenses>
        <license>
            <name>BSD-style</name>
            <url>http://www.opensource.org/licenses/bsd-license.php</url>
            <distribution>repo</distribution>
        </license>
    </licenses>
    <developers>
        <developer>
            <id>wangzaixiang</id>
            <name>wangzaixiang</name>
            <url>http://wangzaixiang.github.io</url>
        </developer>
    </developers>)
