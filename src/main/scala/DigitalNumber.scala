import scala.io.Source
import java.io._

class DigitalElement(dig: Seq[Char]) {
    def toNum(): Option[Int] = {
      val tupres: Option[(Seq[Char], Int)] = DigitalElement.cseq.zip(Stream from 0).find {tup => dig == tup._1}
      val (_, ix) = tupres.map {tup => (Some(tup._1), Some(tup._2)) } getOrElse (None, None)
      ix
    }
}

object DigitalElement {
  private val cseq =
  Seq( // 0
    Seq(' ', '_', ' ',
        '|', ' ', '|',
        '|', '_', '|'),
    // 1
    Seq(' ', ' ', ' ',
        ' ', ' ', '|',
        ' ', ' ', '|'),
    // 2
    Seq(' ', '_', ' ',
      ' ', '_', '|',
      '|', '_', ' '),
    //3
    Seq(' ', '_', ' ',
      ' ', '_', '|',
      ' ', '_', '|'),
    //4
    Seq(' ', ' ', ' ',
      '|', '_', '|',
      ' ', ' ', '|'),
    //5
    Seq(' ', '_', ' ',
      '|', '_', ' ',
      ' ', '_', '|'),
    //6
    Seq(' ', '_', ' ',
        '|', '_', ' ',
        '|', '_', '|'),
    //7
    Seq(' ', '_', ' ',
      ' ', ' ', '|',
      ' ', ' ', '|'),
    //8
    Seq(' ', '_', ' ',
      '|', '_', '|',
      '|', '_', '|'),
    //9
    Seq(' ', '_', ' ',
      '|', '_', '|',
      ' ', '_', '|')
  )

  def printChars1 = {
    cseq.zip(Stream from 0).foreach { e =>
      print(e._2)
      e._1.zip(Stream from 0).foreach {c =>
        if (c._2 % 3 == 0)
          println
        print(c._1)
      }
      println
    }
  }
}

class DigitalNumber(lines: List[String]) {
  // A data structure to hold a number consists of n digits each described by a 3X3 array
  private def validateInput(lines: List[String]): Boolean = {
    lines.length == 3 && lines.forall(s => s.length == lines(0).length && s.length % 3 == 0)
  }

  private def textLinesToDigitalNumber(): (Vector[Option[Int]], Boolean) = {
    require(validateInput(lines))
    val len = lines(0).length
    val digArray =
      for (i <- Range(0, len, 3))
        yield {
          for (ln <- 0 until 3;
               ch <- lines(ln).slice(i, i + 3))
            yield  ch
        }
    val vecDigits: Vector[Option[Int]] =
      for (dig <- digArray.toVector)
        yield new DigitalElement(dig).toNum()
    if (vecDigits.contains(None))
      (vecDigits, false)
    else
      (vecDigits, true)
  }

  def toNumberStr(): (String, Boolean) = {
    val (vec, isValid) = textLinesToDigitalNumber()
    val res = vec.map(e =>
      e match {
        case None => "?"
        case dig => dig.getOrElse("")
    }).mkString("")
    (res, isValid)
  }
}

class TextNumbersResolver(inpFileName: String, resultFileName: String) {
  def fileProcessor() /*: Vector[Vector[String]] */ = {
    // Read input file into a vector, each element consists of 3 lines of text digits
    // write into file(resultFileName)

    val restulFile = new PrintWriter(resultFileName)
    val lines: Vector[String] = (for (line <- Source.fromFile(inpFileName) getLines()) yield line).toVector

    val vecTextNumbers: Vector[Vector[String]] =
      (for (ix <- Range(0, lines.length, 4))
        yield {
          val vecLines: Vector[String] = lines.slice(ix, ix + 3)
          vecLines
        }).toVector

    vecTextNumbers.foreach { textChunk =>
      val (numStr, isValid) = new DigitalNumber(textChunk.toList).toNumberStr()
      if (!isValid)
        restulFile.write(s"$numStr ILLEGAL\n")
      else
        restulFile.write(s"$numStr\n")
    }
    restulFile.close()
  }
}

object UnitTests {
  var isDebug: Boolean = false

  def testSuite(debug: Boolean) = {
    isDebug = debug
    if (isDebug)
      DigitalElement.printChars1
    testDig()
    testNum()
  }

  def equalContent(inpFile1Name: String, inpFile2Name: String): Boolean = {
    val lines1 = Source.fromFile(inpFile1Name).getLines().toList
    val lines2 = Source.fromFile(inpFile2Name).getLines().toList
    lines1 == lines2
  }

  private def testDig() = {
    val digits = Vector(
      Seq(' ', '_', ' ',
          '|', '_', '|',
          '|', '_', '|'),
      Seq(' ', '_', ' ',
          '|', '_', '|',
          '|', ' ', '|'),
      Seq())

    val results = Vector(8, None, None)

    for (i <- 0 until digits.length) {
      val dig = new DigitalElement(digits(i))
      val num = dig.toNum().getOrElse(None)
      if (isDebug)
        println(s"\nfoundDig = $num")
      assert(num == results(i))
    }
  }

  private def testNum() = {
    val linesVec = Vector(
      List(
        " _     _  _  _  _  _     _ ",
        "|_|  ||_| _||_ |_  _||_||_|",
        "|_|  | _||_ |_||_||_   ||_|"),
      List(
        " _  _  _  _  _  _     _    ",
        "  | _||_  _||_|  ||_||_||_|",
        "  | _| _| _||_|  |  | _|  |"),
      List(
        "    _  _  _  _  _  _  _  _ ",
        "|_| _||_||_|| ||_ |_ |_ |_|",
        "  | _||_||_||_| _||_| _||_|"),
        List(
        " _     _  _  _  _  _     _ ",
        "| |  || | _| _|| | _|  | _|",
        "|_|  ||_||_ |_ |_| _   | _|")
    )
    val results = Vector("819266248", "735387494", "438805658", "010220?13")

    for (ix <- 0 until linesVec.length) {
      val (numStr, isValid) = new DigitalNumber(linesVec(ix)).toNumberStr()
      assert(isValid && numStr == results(ix) || !isValid && (numStr.isEmpty || numStr.contains("?")))
    }

  }
}

object MainDigitalNumber extends App {
  // command line arguments
  // --debug: print debug information
  // --unittest: run unit test
  // --inpdir
  // --outdir
  
  def checkArg(parmName: String): Boolean = args.map(_.toLowerCase).contains("--" + parmName)

  def extractArg(parmName: String): Option[String] = {
	  val ix = args.map(_.toLowerCase).indexOf("--" + parmName.toLowerCase)
	  ix match {
		  case -1 => None
		  case n => Some(args(n + 1))	// return the value of the next element
	  }
  }

  def processFile(inpFile: String, outFile: String, testFile: String): Unit = {
    val textNumbersResolver = new TextNumbersResolver(inpFile, outFile)
    textNumbersResolver.fileProcessor()
	if (isDebug)
		println(s"processing file: $inpFile")
    if (isUnitTest)
      assert(UnitTests.equalContent(testFile, outFile))
  }

  val isDebug = checkArg("debug")
  val isUnitTest = checkArg("unittest")
  val inpDir = extractArg("inpdir").getOrElse("engineeringtest - invoices/")
  val outDir = extractArg("outdir").getOrElse("engineeringtest - invoices/output-test")


  if (isUnitTest)
    UnitTests.testSuite(isDebug)

  val ifile1 = inpDir + "input_Q1a.txt"
  val ofile1 = outDir + "output_Q1a.txt"
  val testfile1 = inpDir + "output_Q1a.txt"
  processFile(ifile1, ofile1, testfile1)


  val ifile2 = inpDir + "input_Q1b.txt"
  val ofile2 = outDir + "output_Q1b.txt"
  val testfile2 = inpDir + "output_Q1b.txt"
  processFile(ifile2, ofile2, testfile2)
}


