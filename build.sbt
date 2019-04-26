name := "scala-playground"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.7" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")
