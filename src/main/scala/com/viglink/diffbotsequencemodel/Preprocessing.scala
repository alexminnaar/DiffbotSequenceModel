package com.viglink.diffbotsequencemodel

import java.io.File
import org.jsoup.Jsoup
import scala.collection.JavaConversions._

object Preprocessing {

  def htmlFile2TrainingExamples(fileLocation: File): Vector[Tag] = {

    val validTags = Set("a", "span", "img", "meta", "h4", "p", "input", "h3", "h2", "div", "title", "dd", "h1", "li", "strong", "sup", "ul")
    var seenTags: Set[String] = Set()
    val html = Jsoup.parse(fileLocation, "UTF-8")
    val tags = html.select("*")
    var examples: Vector[Tag] = Vector()

    tags.foreach { tag =>

      val numTagLines = tag
        .outerHtml()
        .split("\n")
        .toList
        .size

      if (validTags.contains(tag.tagName()) && numTagLines < 10) {

        if (!seenTags.contains(tag.toString)) {

          var tagVec: Vector[Array[String]] = Vector()
          val tagTokens = tag.toString.split("<|>| |\">|\"")

          tagTokens.foreach { tt =>
            if (tt.trim.size > 0) {
              tagVec :+= Array(tt.trim())
            }
          }
          examples :+= Tag(tagVec)
        }
        seenTags += tag.toString
      }
    }
    examples
  }

}

//object Test extends App {
//
//  val examples = Preprocessing.htmlFile2TrainingExamples(new File("/users/alexminnaar/diffbot_examples/dillards.html"))
//
//
//  val myCrf = Crf.train("training_data.txt")
//
//  examples.foreach { ex =>
//
//    val prediction = Crf.predict(myCrf,ex.tokens.toArray)
//
//    var printPred = true
//    var idx=0
//
//    while(printPred && idx < prediction._1.length){
//
//      if(prediction._1(idx)._2=="P"){
//        println("<================================ FOUND PREDICTION ================================>")
//        println(prediction._1)
//        println(prediction._2)
//        println(prediction._3)
//        printPred=false
//      }
//
//      idx+=1
//
//
//    }
//
//  }
//
//
//}