import java.io.BufferedReader
import java.io.File
import java.io.FileNotFoundException
import java.io.FileReader
import java.io.IOException
import java.util.ArrayList
import java.util.Collection
import java.util.Collections
import java.util.HashMap
import java.util.Iterator
import java.util.List
import java.util.Map
import java.util.Map.Entry
import java.util.Set
//remove if not needed
import scala.collection.JavaConversions._

object CrunchifyFindMaxOccurance {

  def main(args: Array[String]) {
    val file = new File("D:\\Scala.txt")
    var bufferedReader: BufferedReader = null
    bufferedReader = new BufferedReader(new FileReader(file))
    var inputLine: String = null
    val crunchifyMap = new HashMap[String, Integer]()
    try {
      while ((inputLine = bufferedReader.readLine()) != null) {
        val words = inputLine.split("[ \n\t\r.,;:!?(){}]")
        for (counter <- 0 until words.length) {
          val key = words(counter).toLowerCase()
          if (key.length > 0) {
            if (crunchifyMap.get(key) == null) {
              crunchifyMap.put(key, 1)
            } else {
              var value = crunchifyMap.get(key).intValue()
              value += 1
              crunchifyMap.put(key, value)
            }
          }
        }
      }
      val entrySet = crunchifyMap.entrySet()
      println("Words" + "\t\t" + "# of Occurances")
      for (entry <- entrySet) {
        println(entry.getKey + "\t\t" + entry.getValue)
      }
      val myTopOccurrence = crunchifyFindMaxOccurance(crunchifyMap, 1)
      println("\nMaixmum Occurance of Word in file: ")
      for (result <- myTopOccurrence) {
        println("==> " + result)
      }
    } catch {
      case error: IOException => println("Invalid File")
    } finally {
      bufferedReader.close()
    }
  }

  def crunchifyFindMaxOccurance(map: Map[String, Integer], n: Int): List[String] = {
    val l = new ArrayList[CrunchifyComparable]()
    for ((key, value) <- map) l.add(new CrunchifyComparable(key, value))
    Collections.sort(l)
    val list = new ArrayList[String]()
    for (w <- l.subList(0, n)) list.add(w.wordFromFile + ":" + w.numberOfOccurrence)
    list
  }
}

class CrunchifyComparable(var wordFromFile: String, var numberOfOccurrence: Int)
  extends Comparable[CrunchifyComparable]() {

  override def compareTo(arg0: CrunchifyComparable): Int = {
    val crunchifyCompare = java.lang.Integer.compare(arg0.numberOfOccurrence, this.numberOfOccurrence)
    if (crunchifyCompare != 0) crunchifyCompare else wordFromFile.compareTo(arg0.wordFromFile)
  }

  override def hashCode(): Int = {
    val uniqueNumber = 19
    var crunchifyResult = 9
    crunchifyResult = uniqueNumber * crunchifyResult + numberOfOccurrence
    crunchifyResult = uniqueNumber * crunchifyResult +
      (if ((wordFromFile == null)) 0 else wordFromFile.hashCode)
    crunchifyResult
  }

  override def equals(crunchifyObj: Any): Boolean = {
    if (this == crunchifyObj) return true
    if (crunchifyObj == null) return false
    if (getClass != crunchifyObj.getClass) return false
    val other = crunchifyObj.asInstanceOf[CrunchifyComparable]
    if (numberOfOccurrence != other.numberOfOccurrence) return false
    if (wordFromFile == null) {
      if (other.wordFromFile != null) return false
    } else if (wordFromFile != other.wordFromFile) return false
    true
  }
}
