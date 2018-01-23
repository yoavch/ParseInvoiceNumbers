import scala.io.Source
import scala.collection.immutable.ListMap
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class WordsCounter(inpFileName:String) {

  def wc(lofw: List[String]): Map[String, Int] = {
    lofw.groupBy(word => word.toLowerCase).
      map {case(w, ls) => (w -> ls.size)}
  }

  def countWordsInFile(): Future[Map[String, Int]] = Future {
    val words = Source.fromFile(inpFileName).
      getLines.flatMap(line => line.split("\\W+")).toList
    wc(words)
  }
}

object WordsCounter {
  def apply(inpFileName: String) = new WordsCounter(inpFileName)

  def extractSeqMapsFromFuture(seqf: Seq[Future[Map[String, Int]]]): Seq[Map[String, Int]] = {
    val fseq = Future.sequence(seqf)
    val res = Await.result[Seq[Map[String, Int]]](fseq, 10.seconds)
    res
  }

  def mergeMaps(maps: Seq[Map[String, Int]]): Map[String, Int] = {
    maps.foldLeft(Map[String, Int]()) {
      case (m1, m2) => m1 ++ m2.map(e => (e._1 -> (e._2 + m1.getOrElse(e._1, 0))))
    }
  }


  def printSortedWc(wc: Map[String, Int]): Unit = {
    ListMap(wc.toSeq.sortBy(_._1):_*).foreach {case (w, cnt) => println(s"$w: $cnt")}
  }

}

object MainWordCount extends App {
  // --inpdir

  def extractArg(parmName: String): Option[String] = {
    val ix = args.map(_.toLowerCase).indexOf("--" + parmName.toLowerCase)
    ix match {
      case -1 => None
      case n => Some(args(n + 1))	// return the value of the next element
    }
  }

  val inpDir = extractArg("inpdir").getOrElse("C:\\DevProj\\Taboola\\testfiles\\")

  val fmaps = Seq(
    WordsCounter(inpDir + "File1.txt").countWordsInFile(),
    WordsCounter(inpDir + "File2.txt").countWordsInFile(),
    WordsCounter(inpDir + "File3.txt").countWordsInFile())

  val maps = WordsCounter.extractSeqMapsFromFuture(fmaps)
  val resMap = WordsCounter.mergeMaps(maps)
  WordsCounter.printSortedWc(resMap)
}
