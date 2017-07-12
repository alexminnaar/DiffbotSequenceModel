name := "DiffbotSequenceModel"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies++=Seq(
  "cc.mallet" % "mallet" % "2.0.7",
  "org.jsoup" % "jsoup" % "1.10.2",
  "org.scalatest" %% "scalatest" % "3.2.0-SNAP4" % "test"
)
    