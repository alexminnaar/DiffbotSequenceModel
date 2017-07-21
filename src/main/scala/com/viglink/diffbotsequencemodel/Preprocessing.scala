package com.viglink.diffbotsequencemodel

import java.io.File
import org.jsoup.Jsoup
import scala.collection.JavaConversions._

object Preprocessing {

  def htmlFile2Examples(fileLocation: File): Vector[Tag] = {

    val validTags = Set("a", "span", "img", "meta", "h4", "p", "input", "h3",
      "h2", "div", "title", "dd", "h1", "li", "strong", "sup", "ul", "link", "b", "td", "strike","em")
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
