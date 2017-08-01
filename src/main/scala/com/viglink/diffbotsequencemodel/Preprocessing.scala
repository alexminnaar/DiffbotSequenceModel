package com.viglink.diffbotsequencemodel

import java.io.File

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Node}

import scala.collection.JavaConversions._

object Preprocessing {

  /**
    * Given an Jsoup document extract the html tags
    * @param html Jsoup document
    * @return a vector of html tags
    */
  def htmlFile2Examples(html: Document): Vector[Tag] = {

    val validTags = Set("a", "span", "img", "meta", "h4", "p", "input", "h3",
      "h2", "div", "title", "dd", "h1", "li", "strong", "sup", "ul", "link", "b", "td", "strike", "em")
    var seenTags: Set[String] = Set()

    val tags = html.select("*")
    var examples: Vector[Tag] = Vector()
    var indexCounter = 0

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
          examples :+= Tag(tokens = tagVec, index = indexCounter)
          indexCounter += 1
        }
        seenTags += tag.toString
      }
    }
    examples
  }

  /**
    * Remove comments from html
    * @param node Jsoup node with comments
    * @return Jsoup node without comments
    */
  def removeComments(node: Node): Node = {

    node.childNodes().foreach { cn =>

      if (cn.nodeName().equals("#comment")) {
        cn.remove()
      }
      else {
        removeComments(cn)
      }
    }

    node
  }

}
