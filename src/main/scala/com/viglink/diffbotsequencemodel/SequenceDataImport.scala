package com.viglink.diffbotsequencemodel

import java.io.File
import java.util.regex.Pattern

import cc.mallet.pipe._
import cc.mallet.pipe.iterator.SimpleFileLineIterator
import cc.mallet.share.casutton.ner.ConllNer2003Sentence2TokenSequence
import cc.mallet.types.InstanceList

import scala.collection.mutable.ListBuffer


object SequenceDataImport extends App{


  def readTrainingFile(file:File, pipe:Pipe)={

    val iterator = new SimpleFileLineIterator(file)
    val instances = new InstanceList(pipe)

    instances.addThruPipe(iterator)

    instances
  }

  def buildPipe(): Pipe = {

    var pipeList = new ListBuffer[Pipe]()

    pipeList+= new SvmLight2FeatureVectorAndLabel()

//    pipeList += new Input2CharSequence("UTF-8")
//
//    val tokenPattern = Pattern.compile("[\\p{L}\\p{N}_]+")
//
//    pipeList += new CharSequence2TokenSequence(tokenPattern)
//    pipeList += new TokenSequenceLowercase()
//    pipeList += new TokenSequenceRemoveStopwords(false, false)
//    pipeList += new TokenSequence2FeatureSequence()
//    pipeList += new Target2Label()
//    pipeList += new FeatureSequence2FeatureVector()
    pipeList += new PrintInputAndTarget()

    new SerialPipes(pipeList.toArray)
  }

  val pipe = buildPipe()

  val instances = readTrainingFile(new File("training_data.txt"), pipe)


}
