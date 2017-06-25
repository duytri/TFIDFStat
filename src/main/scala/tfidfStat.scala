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
    try {
      if (args.length < 4 || args.length == 0 || args(0).equals("-h") || args(0).equals("--help"))
        printHelp
      else {
        // input arguments
        println("Getting user parameters...")
        val params = new ParamsHelper
        val input = params.checkAndGetInput(args, "-i", "--input", ParamsHelperType.STRING).asInstanceOf[String]
        val output = params.checkAndGetInput(args, "-o", "--output", ParamsHelperType.STRING).asInstanceOf[String]
        if (input == null || output == null) throw new Exception("ERROR: You must declare input and output")
        // Processing
        println("Processing...")
        val inputPath = input + File.separator + "corpus"
        val inputFiles = (new File(inputPath)).listFiles()
        val outputFile = output + File.separator + "result.csv"
        val stopwordFilePath = input + File.separator + "stopwords/vietnamese-stopwords.txt"

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
        val swSource = Source.fromFile(stopwordFilePath)
        swSource.getLines.foreach { x => arrStopwords.append(x) }
        swSource.close

        wordSetByFile.foreach(oneSet => oneSet --= arrStopwords)

        //~~~~~~~~~~ Calculate TFIDF ~~~~~~~~~~
        var tfidfWordSet = new ArrayBuffer[Map[String, Double]](inputFiles.length) // Map[word, TF*IDF-value]

        for (i <- 0 to inputFiles.length - 1) {
          var tfidfOneDoc = Map[String, Double]()
          for (oneWord <- wordSetByFile(i)) {
            if (oneWord._1.equals("theverge")) {
              var wordCount = 0d
              wordSetByFile(i).foreach((x => wordCount += x._2))
              println("Freq = " + oneWord._2 + " WordNo. = " + wordCount + " TF = " + oneWord._2 / wordCount)

              var n = 0d
              wordSetByFile.foreach(doc => {
                if (doc.contains(oneWord._1)) n += 1
              })

              println("DocNo. = " + wordSetByFile.length + " DocContainsNo. = " + n + " IDF = " + Math.log10(wordSetByFile.length / n))

              //wordSetByFile(i).foreach(x => println(x._1 + " ++++ " + x._2))

            }
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
    } catch {
      case e: Exception => {
        e.printStackTrace()
        printHelp()
      }
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
    val bw = new BufferedWriter(new FileWriter(file, true))
    bw.flush()
    array.foreach { x =>
      bw.write(x._1 + "|" + x._2 + "\n")
    }
    bw.close()
  }

  def printHelp() = {
    println("Usage: TFIDFStat [Arguments]")
    println("       Arguments:")
    println("              -i --input [path]  : Path of corpus and stopwords folder")
    println("              -o --output [path] : Output file path")
    println("              -h --help          : Print this help")
  }
}