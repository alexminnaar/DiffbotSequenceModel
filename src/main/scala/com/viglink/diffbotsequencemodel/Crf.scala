package com.viglink.diffbotsequencemodel

import java.io._
import java.util.Random
import java.util.regex.Pattern

import cc.mallet.fst._
import cc.mallet.pipe.iterator.LineGroupIterator
import cc.mallet.pipe.tsf._
import cc.mallet.pipe.{Pipe, SerialPipes, SimpleTaggerSentence2TokenSequence, TokenSequence2FeatureVectorSequence}
import cc.mallet.types.{Instance, InstanceList, Sequence}
import com.viglink.diffbotsequencemodel.features.{TagName, TokenTextLength}

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._


object Crf {

  def predictedTokens2String(predictedTokens: List[(String, String)]): Vector[String] = {

    var stringPredictions = Vector.empty[String]
    var predictedString = new StringBuilder()
    var lastPrediction = ""

    //iterate through token predictions and concatenate contiguous predictions into a single string
    predictedTokens.foreach { tokenPrediction =>

      val token = tokenPrediction._1
      val prediction = tokenPrediction._2

      if (prediction != "O" && prediction == lastPrediction) {
        predictedString.append(s" $token")
      }
      else if (prediction != "O" && prediction != lastPrediction) {
        if (!predictedString.isEmpty) {
          stringPredictions :+= predictedString.toString()
        }
        predictedString = new StringBuilder()
        predictedString.append(token)
      }
      lastPrediction = prediction
    }

    //if tag ends in a prediction make sure to append it
    if (!predictedString.isEmpty) {
      stringPredictions :+= predictedString.toString()
    }

    stringPredictions
  }

  def predict(crf: CRF, tokens: Array[Array[String]]): Prediction = {

    //We want to predict here so let the pipes know there are now targets
    crf.getInputPipe.setTargetProcessing(false)
    val instance = crf.getInputPipe().instanceFrom(new Instance(tokens, null, null, null))
    val input = instance.getData().asInstanceOf[Sequence[Object]]
    val output = crf.transduce(input)

    if (tokens.length != output.size()) {
      println("Number of tokens and predicted labels does not match!")
    }

    //val prediction = new SequencePrediction()
    val lattice = new SumLatticeDefault(crf, input)

    val predictedLabels = new Array[(String, String)](tokens.length)

    (0 to tokens.length - 1).foreach { idx =>
      predictedLabels(idx) = (tokens(idx)(0), output.get(idx).toString)
    }

    //sequence confidence
    val logZ = lattice.getTotalWeight()
    val logS = new SumLatticeDefault(crf, input, output).getTotalWeight()
    val p = Math.exp(logS - logZ)

    //token confidence
    val ps = new Array[Double](output.size())
    (0 to output.size() - 1).foreach { idx =>
      val pred = output.get(idx).asInstanceOf[String]
      ps(idx) = lattice.getGammaProbability(idx + 1, crf.getState(pred))
    }

    Prediction(
      tokenPredictions = predictedLabels.toList,
      predictionAsString = predictedTokens2String(predictedLabels.toList),
      confidence = p
    )

  }


  def train(trainingFilename: String, trainTestSplit: Array[Double]): CRF = {

    var pipes = new ListBuffer[Pipe]()

    val conjunctions: Array[Array[Int]] = Array(Array(-1), Array(1), Array(-2), Array(-2, -1))

    pipes += new SimpleTaggerSentence2TokenSequence()
    pipes += new OffsetConjunctions(conjunctions)
    pipes += new TokenTextCharPrefix("charprefix=", 1)
    pipes += new TokenTextLength()
    pipes += new TagName()
    pipes += new RegexMatches("CAPITALIZED", Pattern.compile("^\\p{Lu}.*"))
    pipes += new RegexMatches("STARTSNUMBER", Pattern.compile("^[0-9].*"))
    pipes += new RegexMatches("HYPHENATED", Pattern.compile(".*\\-.*"))
    pipes += new RegexMatches("DOLLARSIGN", Pattern.compile(".*\\$.*"))
    pipes += new RegexMatches("URL", Pattern.compile("(https?:\\/\\/(?:www\\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|www\\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|https?:\\/\\/(?:www\\.|(?!www))[a-zA-Z0-9]\\.[^\\s]{2,}|www\\.[a-zA-Z0-9]\\.[^\\s]{2,})"))
    pipes += new RegexMatches("PRICE", Pattern.compile("^(?!0+$)\\d{0,5}(.\\d{1,2})?$"))
    pipes += new TokenFirstPosition("FIRSTTOKEN")
    pipes += new TokenSequence2FeatureVectorSequence()
    //pipes += new SequencePrintingPipe(out)

    val pipe = new SerialPipes(pipes.toArray)

    val allTrainingInstances = new InstanceList(pipe)

    allTrainingInstances.addThruPipe(new LineGroupIterator(new FileReader(new File(trainingFilename)), Pattern.compile("^\\s*$"), true))

    allTrainingInstances.foreach { inst =>
      println("<=======================================new instance=======================================>")
      //println(s"instance target alphabet: ${inst.getTargetAlphabet}")
      println(s"instance label: ${inst.getTarget}")
      println(s"instance data: ${inst.getData}")
    }

    val r = new Random(System.currentTimeMillis())

    val trainingLists = allTrainingInstances.split(r, trainTestSplit)
    val trainingInstances = trainingLists(0)
    val testInstances = trainingLists(1)

    val crf = new CRF(pipe, null)

    crf.addStatesForThreeQuarterLabelsConnectedAsIn(trainingInstances)
    crf.addStartState()

    val trainer = new CRFTrainerByLabelLikelihood(crf)
    trainer.setGaussianPriorVariance(10.0)

    trainer.addEvaluator(new PerClassAccuracyEvaluator(testInstances, "testing"))
    trainer.train(trainingInstances)

    println("Number of training instances: " + trainingInstances.size())
    println("Number of test instances: " + testInstances.size())

    crf
  }

  def htmlParse(htmlTags: Vector[Tag], model: CRF): ParseResult = {

    var titlePredictions = Vector.empty[Prediction]
    var pricePredictions = Vector.empty[Prediction]
    var imagePredictions = Vector.empty[Prediction]
    var skuPredictions = Vector.empty[Prediction]
    var canonicalUrlPredictions = Vector.empty[Prediction]
    var inStockPredictions = Vector.empty[Prediction]
    var mpnPredictions = Vector.empty[Prediction]

    //predict each tag
    htmlTags.foreach { t =>

      val prediction = Crf.predict(model, t.tokens.toArray)
      var predNotFound = true
      var idx = 0

      //search tag tokens for predictions
      while (predNotFound && idx < prediction.tokenPredictions.length) {

        if (prediction.tokenPredictions(idx)._2 == "T") {
          titlePredictions :+= prediction
          predNotFound = false
        }

        if (prediction.tokenPredictions(idx)._2 == "P") {
          pricePredictions :+= prediction
          predNotFound = false
        }

        if (prediction.tokenPredictions(idx)._2 == "I") {
          imagePredictions :+= prediction
          predNotFound = false
        }

        if (prediction.tokenPredictions(idx)._2 == "S") {
          skuPredictions :+= prediction
          predNotFound = false
        }

        if (prediction.tokenPredictions(idx)._2 == "CU") {
          canonicalUrlPredictions :+= prediction
          predNotFound = false
        }

        if (prediction.tokenPredictions(idx)._2 == "INSTOCK") {
          inStockPredictions :+= prediction
          predNotFound = false
        }

        if (prediction.tokenPredictions(idx)._2 == "M") {
          mpnPredictions :+= prediction
          predNotFound = false
        }

        idx += 1
      }

    }

    //TODO: just for testing, remove later
    println("TOP TITLES")
    titlePredictions.sortBy(-_.confidence).foreach(println)
    println("TOP PRICES")
    pricePredictions.sortBy(-_.confidence).foreach(println)
    println("TOP IMAGES")
    imagePredictions.sortBy(-_.confidence).foreach(println)
    println("TOP SKU")
    skuPredictions.sortBy(-_.confidence).foreach(println)
    println("TOP CANONICAL URL")
    canonicalUrlPredictions.sortBy(-_.confidence).foreach(println)
    println("TOP INSTOCK")
    inStockPredictions.sortBy(-_.confidence).foreach(println)
    println("TOP MPN")
    mpnPredictions.sortBy(-_.confidence).foreach(println)



    //sort by confidence to find the most likely predictions for each item extracted
    val mostLikelyTitle: Option[Prediction] = if (titlePredictions.length > 0) Some(titlePredictions.sortBy(-_.confidence).head) else None
    val mostLikelyPrice: Option[Prediction] = if (pricePredictions.length > 0) Some(pricePredictions.sortBy(-_.confidence).head) else None
    var mostLikelyImage: Option[Prediction] = if (imagePredictions.length > 0) Some(imagePredictions.sortBy(-_.confidence).head) else None
    val mostLikelySku: Option[Prediction] = if (skuPredictions.length > 0) Some(skuPredictions.sortBy(-_.confidence).head) else None
    val mostLikelyAvailability: Option[Prediction] = if (inStockPredictions.length > 0) Some(inStockPredictions.sortBy(-_.confidence).head) else None
    val mostLikelyCanonicalUrl: Option[Prediction] = if (canonicalUrlPredictions.length > 0) Some(canonicalUrlPredictions.sortBy(-_.confidence).head) else None
    val mostLikelyMpn: Option[Prediction] = if (mpnPredictions.length > 0) Some(mpnPredictions.sortBy(-_.confidence).head) else None


    //The following is an Amazon-specific heuristic which says that the image tag with the most tokens in common with the title is the correct image tag
    if (mostLikelyImage.isDefined && !mostLikelyImage.get.tokenPredictions.map(_._1).contains("og:image")) {
      val titleTokens = mostLikelyTitle.get.tokenPredictions.map(_._1)
      val imageTokens = imagePredictions.filter(_.confidence > 0.7)

      val intersection = imageTokens.map(it => (it, titleTokens.intersect(it.tokenPredictions.map(_._1)).size))

      if (intersection.length > 0) {
        mostLikelyImage = Some(intersection.sortBy(-_._2).head._1)
      }

    }





    ParseResult(
      title = mostLikelyTitle,
      price = mostLikelyPrice,
      image = mostLikelyImage,
      sku = mostLikelySku,
      availability = mostLikelyAvailability,
      canonicalUrl = mostLikelyCanonicalUrl,
      mpn = mostLikelyMpn
    )
  }

}
