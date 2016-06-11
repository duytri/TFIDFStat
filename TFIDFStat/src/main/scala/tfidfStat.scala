package main.scala

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.collection.mutable.Map
import java.io.BufferedWriter
import java.io.FileWriter

object tfidfStat {
  /* Cac tham so cua chuong trinh:
   * param 0: duong dan den thu muc input va libs (chua file thu vien stopword). CO splash o cuoi.
   * param 1: duong dan thu muc xuat file ket qua.
   * param 2: ten thu muc ben trong thu muc input (la 0 hay 1).
   * */
  def main(args: Array[String]): Unit = {
    args.foreach { println }
    val inputPath = args(0) + "input" + File.separator + args(2)
    val outputPath = args(1)
    val inputFiles = (new File(inputPath)).listFiles()
    val outputFile = outputPath + File.separator + "result.csv"

    println("Input path: " + inputPath)
    println("Output path: " + outputFile)
    println("Start analyze ...")

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

    var arrStopwords = new ArrayBuffer[String]
    val stopwordFilePath = args(0) + "libs/vietnamese-stopwords.txt"
    val swSource = Source.fromFile(stopwordFilePath)
    swSource.getLines.foreach { x => arrStopwords.append(x) }
    swSource.close

    wordSetByFile.foreach(oneSet => oneSet --= arrStopwords)

    //~~~~~~~~~~ Calculate TFIDF ~~~~~~~~~~
    var tfidfWordSet = new ArrayBuffer[Map[String, Double]](inputFiles.length) // Map[word, TF*IDF-value]

    for (i <- 0 to inputFiles.length - 1) {
      var tfidfOneDoc = Map[String, Double]()
      for (oneWord <- wordSetByFile(i)) {
        tfidfOneDoc += oneWord._1 -> TFIDFCalc.tfIdf(oneWord._1, i, wordSetByFile)
      }
      tfidfWordSet.append(tfidfOneDoc)
    }

    //~~~~~~~~~~ Write to files ~~~~~~~~~~~
    for (i <- 0 to inputFiles.length - 1) {
      write2File(tfidfWordSet(i), outputFile)
    }

    println("Finished!!!")
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
    val bw = new BufferedWriter(new FileWriter(file, true))
    bw.flush()
    array.foreach { x =>
      bw.write(x._1 + ", " + x._2 + "\n")
    }
    bw.close()
  }
}