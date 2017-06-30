package com.viglink.diffbotsequencemodel

import java.io._
import java.util.Random
import java.util.regex.Pattern
import java.util.zip.GZIPInputStream

import cc.mallet.fst._
import cc.mallet.optimize.Optimizable
import cc.mallet.optimize.Optimizable.ByGradientValue
import cc.mallet.pipe.iterator.{LineGroupIterator, LineIterator}
import cc.mallet.pipe.tsf._
import cc.mallet.pipe.{Pipe, SerialPipes, SimpleTaggerSentence2TokenSequence, TokenSequence2FeatureVectorSequence}
import cc.mallet.types.InstanceList

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._


object Crf extends App {

  def train(trainingFilename: String): Unit = {

    var pipes = new ListBuffer[Pipe]()

    //val out = new PrintWriter("test.out")

    val conjunctions: Array[Array[Int]] = Array(Array(-1), Array(1), Array(-2, -1))

    pipes += new SimpleTaggerSentence2TokenSequence()
    pipes += new OffsetConjunctions(conjunctions)
    pipes += new TokenTextCharSuffix("C1=", 1)
    pipes += new TokenTextCharSuffix("C2=", 1)
    pipes += new TokenTextCharSuffix("C3=", 1)
    pipes += new RegexMatches("CAPITALIZED", Pattern.compile("^\\p{Lu}.*"))
    pipes += new RegexMatches("STARTSNUMBER", Pattern.compile("^[0-9].*"))
    pipes += new RegexMatches("HYPHENATED", Pattern.compile(".*\\-.*"))
    pipes += new RegexMatches("DOLLARSIGN", Pattern.compile(".*\\$.*"))
    pipes += new TokenFirstPosition("FIRSTTOKEN")
    pipes += new TokenSequence2FeatureVectorSequence()
    //pipes += new SequencePrintingPipe(out)

    val pipe = new SerialPipes(pipes.toArray)

    val allTrainingInstances = new InstanceList(pipe)
    //val testingInstances = new InstanceList(pipe)

    allTrainingInstances.addThruPipe(new LineGroupIterator(new FileReader(new File(trainingFilename)), Pattern.compile("^\\s*$"), true))
    //        new BufferedReader(
    //          new InputStreamReader(
    //            new FileInputStream(trainingFilename))), Pattern.compile("^\\s*$"), true))


    val r = new Random(System.currentTimeMillis())

    val trainingLists = allTrainingInstances.split(r, Array(0.8, 0.2))
    val trainingInstances = trainingLists(0)
    val testInstances = trainingLists(1)

    //    println("Number of training instances: "+trainingInstances.size())
    //    println("Number of test instances: "+testInstances.size())


    //    testingInstances.addThruPipe(
    //      new LineGroupIterator(
    //        new BufferedReader(
    //          new InputStreamReader(
    //            new FileInputStream(testFilename))), Pattern.compile("^\\s*$"), true))

    val crf = new CRF(pipe, null)

    crf.addStatesForThreeQuarterLabelsConnectedAsIn(trainingInstances)
    crf.addStartState()

    val trainer = new CRFTrainerByLabelLikelihood(crf)
    trainer.setGaussianPriorVariance(10.0)

    trainer.addEvaluator(new PerClassAccuracyEvaluator(testInstances, "testing"))
    //trainer.addEvaluator(new TokenAccuracyEvaluator(testInstances, "testing"))
    trainer.train(trainingInstances)

    println("Number of training instances: " + trainingInstances.size())
    println("Number of test instances: " + testInstances.size())

  }

  train("training_data.txt")


}
