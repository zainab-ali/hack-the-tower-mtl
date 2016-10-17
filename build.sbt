scalaOrganization := "org.typelevel"

scalacOptions ++= Seq(
  "-Ypartial-unification", 
  "-Yliteral-types"
)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "0.7.2",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
