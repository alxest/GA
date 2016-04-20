organization := "generator"

name := "default"

version := "0.1-SNAPSHOT"

libraryDependencies += "com.github.pathikrit" %% "better-files" % "2.13.0"
// http://mvnrepository.com/artifact/com.github.pathikrit/better-files_2.11/2.13.0

val buildSettings = Defaults.defaultSettings ++ Seq(
   javaOptions += "-Xmx4G")
