package main.scala

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

object TFIDFCalc {
  def tf(term: String, doc: Map[String, Int]): Double = {
    var wordCount = 0d
    doc.foreach((x => wordCount += x._2))
    doc(term) / wordCount
  }

  def idf(term: String, allDocs: ArrayBuffer[Map[String, Int]]): Double = {
    var n = 0d
    allDocs.foreach(doc => {
      if (doc.contains(term)) n += 1
    })

    return Math.log10(allDocs.length / n)
  }

  def tfIdf(word: String, docIndex: Int, allDocs: ArrayBuffer[Map[String, Int]]): Double = {
    val doc = allDocs(docIndex)
    return tf(word, doc) * idf(word, allDocs)
  }
}