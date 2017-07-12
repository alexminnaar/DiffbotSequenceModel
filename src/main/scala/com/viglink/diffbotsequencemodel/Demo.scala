package com.viglink.diffbotsequencemodel

import java.io.File

object Demo extends App{

  val examples = Preprocessing.htmlFile2TrainingExamples(new File("/users/alexminnaar/diffbot_examples/onnit.html"))

  //train model
  val myCrf = Crf.train("training_data.txt")

  //parse page
  val result = Crf.htmlParse(htmlTags = examples,model = myCrf)

  //print results
  println(result.title)
  println(result.price)
  println(result.image)


}
