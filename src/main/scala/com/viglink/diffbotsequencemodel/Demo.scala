package com.viglink.diffbotsequencemodel

import java.io.File

object Demo extends App {

  //train model
  val myCrf = Crf.train(trainingFilename = "training_data.txt", trainTestSplit = Array(100.0, 0.0))

  //parse page using trained model
  val examples = Preprocessing.htmlFile2Examples(new File("/users/alexminnaar/merchantOrPublisher_evaluation/barneys_1.html"))
  val result = Crf.htmlParse(htmlTags = examples, model = myCrf)

  println("\n\n\n\nFINAL RESULTS:")

  //print results
  println("Title: ")
  println(result.title)
  println("Price: ")
  println(result.price)
  println("Image: ")
  println(result.image)
  println("Sku: ")
  println(result.sku)
  println("Availability: ")
  println(result.availability)
  println("Canonical Url: ")
  println(result.canonicalUrl)
  println("MPN: ")
  println(result.mpn)

}
