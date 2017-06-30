package com.viglink.diffbotsequencemodel

import java.io.{File, FileFilter}
import java.util.regex.Pattern

import cc.mallet.pipe._
import cc.mallet.pipe.iterator.FileIterator
import cc.mallet.types.InstanceList

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._


class TxtFilter extends FileFilter {

  override def accept(pathname: File): Boolean = {
    pathname.toString.endsWith(".txt")
  }

}

object ImportExample extends App {

  def readDirectories(directories: File, pipe:Pipe): Unit = {


    val iterator = new FileIterator(directories, new TxtFilter, FileIterator.LAST_DIRECTORY)

    val instances = new InstanceList(pipe)
    instances.addThruPipe(iterator)

    instances.foreach{i =>
      println("==================================new example==================================")
      println("NAME:")
      println(i.getName)
      println("DATA:")
      println(i.getData)
      println("LABEL:")
      println(i.getLabeling)
    }

  }

  def buildPipe(): Pipe = {

    var pipeList = new ListBuffer[Pipe]()

    pipeList += new Input2CharSequence("UTF-8")

    val tokenPattern = Pattern.compile("[\\p{L}\\p{N}_]+")

    pipeList += new CharSequence2TokenSequence(tokenPattern)
    pipeList += new TokenSequenceLowercase()
    pipeList += new TokenSequenceRemoveStopwords(false, false)
    pipeList += new TokenSequence2FeatureSequence()
    pipeList += new Target2Label()
    pipeList += new FeatureSequence2FeatureVector()
    //pipeList += new PrintInputAndTarget()

    new SerialPipes(pipeList.toArray)
  }


  println("hello world")
  val pipe = buildPipe()

  val instances = readDirectories(new File("/users/alexminnaar/downloads/sample-data/web/en"), pipe)

}
