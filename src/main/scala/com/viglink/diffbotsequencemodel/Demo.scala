package com.viglink.diffbotsequencemodel


object Demo extends App {

  //train model
  val myCrf = DiffbotModel.train(trainingFilename = "training_data.txt", trainTestSplit = Array(100.0, 0.0))

  //extract info from url
  val url = "https://www.radioshack.com/collections/walkie-talkies/products/midland-20-mile-22ch"
  val result = DiffbotModel.urlParse(url, myCrf)

  //print results
  println("\n\n\n\nFINAL RESULTS:")
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
