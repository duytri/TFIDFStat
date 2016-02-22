package main.scala

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.collection.mutable.Map
import java.io.BufferedWriter
import java.io.FileWriter

object tfidfStat {
  def main(args: Array[String]): Unit = {
    val inputPath = args(0)
    val outputPath = args(1)
    val inputFiles = (new File(inputPath)).listFiles()

    //~~~~~~~~~~ Get input files ~~~~~~~~~~
    var wordSetByFile = new ArrayBuffer[Map[String, Int]](inputFiles.length) // Map[word, frequency in document]
    // Foreach text file
    for (i <- 0 to inputFiles.length - 1) {
      var wordsTmpArr = new ArrayBuffer[String]
      val source = Source.fromFile(inputFiles(i).getAbsolutePath, "utf-8")
      source.getLines.foreach { x => wordsTmpArr.append(x) }
      // Fixed too many open files exception
      source.close
      wordSetByFile.append(addOrIgnore(wordsTmpArr))
    }

    //~~~~~~~~~~ Calculate TFIDF ~~~~~~~~~~
    var tfidfWordSet = new ArrayBuffer[Map[String, Double]](inputFiles.length) // Map[word, TF*IDF-value]

    for (i <- 0 to inputFiles.length - 1) {
      for (oneWord <- wordSetByFile(i)) {
        tfidfWordSet.append(Map(oneWord._1 -> TFIDFCalc.tfIdf(oneWord._1, i, wordSetByFile)))
      }
    }

    //~~~~~~~~~~ Write to files ~~~~~~~~~~~
    for (i <- 0 to inputFiles.length - 1) {
      val outputFile = outputPath + File.separator + inputFiles(i).getName
      write2File(tfidfWordSet(i), outputFile)
    }
  }

  def addOrIgnore(someWords: ArrayBuffer[String]): Map[String, Int] = {
    var eachWordSet = Map[String, Int]()
    someWords.foreach { x =>
      {
        if (!eachWordSet.contains(x))
          eachWordSet += (x -> 1)
        else eachWordSet.update(x, eachWordSet(x) + 1)
      }
    }
    eachWordSet
  }

  def write2File(array: Map[String, Double], filePath: String): Unit = {
    val file = new File(filePath)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.flush()
    array.foreach { x =>
      bw.write(x._1 + ", " + x._2 + "\n")
    }
    bw.close()
  }
}