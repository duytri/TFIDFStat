package main.scala

object ParamsHelperType {
  val INTEGER: Int = 1
  val LONG: Int = 2
  val DOUBLE: Int = 3
  val STRING: Int = 4
}

class ParamsHelper {

  /**
   * Checking and get user input from array of arguments
   * @param args array of arguments
   * @param shortArg short argument name
   * @param longArg long argument name
   * @param typeOfArg ParamsHelperType ENUM
   * @return value from user or <code>null</code> if error occur
   */
  @throws(classOf[IllegalArgumentException])
  def checkAndGetInput(args: Array[String], shortArg: String, longArg: String, typeOfArg: Int): Any = {
    if (args.contains(shortArg)) {
      val res = {
        if (typeOfArg == ParamsHelperType.INTEGER)
          args(args.indexOf(shortArg) + 1).toInt
        else if (typeOfArg == ParamsHelperType.LONG)
          args(args.indexOf(shortArg) + 1).toLong
        else if (typeOfArg == ParamsHelperType.DOUBLE)
          args(args.indexOf(shortArg) + 1).toDouble
        else if (typeOfArg == ParamsHelperType.STRING)
          args(args.indexOf(shortArg) + 1)
        else throw new IllegalArgumentException("ERROR!!!")
      }
      return res
    } else if (args.contains(longArg)) {
      val res = {
        if (typeOfArg == ParamsHelperType.INTEGER)
          args(args.indexOf(longArg) + 1).toInt
        else if (typeOfArg == ParamsHelperType.LONG)
          args(args.indexOf(longArg) + 1).toLong
        else if (typeOfArg == ParamsHelperType.DOUBLE)
          args(args.indexOf(longArg) + 1).toDouble
        else if (typeOfArg == ParamsHelperType.STRING)
          args(args.indexOf(longArg) + 1)
        else throw new IllegalArgumentException("ERROR!!!")
      }
      return res
    }
    return null
  }
}