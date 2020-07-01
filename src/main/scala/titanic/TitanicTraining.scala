package titanic
import java.io._
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import au.com.bytecode.opencsv.CSVWriter
import scala.collection.mutable.ArrayBuffer

object TitanicTraining {
  val trainingDataSetVer1 = TitanicDataSet.getPreparedDataSetSixColumns()
  val trainingDataSetVer2 = TitanicDataSet.getPreparedDataSetSevenColumns()
  val trainingDataSetVer3 = TitanicDataSet.getPreparedDatasetEigthColumns()
 def main(args: Array[String]) {
    println("Start training")
    predictAndCreateCsv(trainingDataSetVer1, "trainingVer1.csv")
    predictAndCreateCsv(trainingDataSetVer2, "trainingVer2.csv")
    predictAndCreateCsv(trainingDataSetVer3, "trainingVer3.csv")
  }

  /**
   * create new classificator and train it with train data
   * then predict all data in the test datasets
   * then create new csv file with prediction, of a person survived or not.
   * (csv with column "survived")
   * @param train
   * @return
   */
  def predictAndCreateCsv(train:(List[Map[String,Any]], String, List[String]), fileName: String) = {
    val outputFile = new BufferedWriter(new FileWriter(fileName))
    val csvWriter = new CSVWriter(outputFile)

    val csvTableHeaderFields = ArrayBuffer[String]()
    train._3.foreach(title => csvTableHeaderFields += title)
    csvTableHeaderFields += train._2

    val listOfRecords = new ListBuffer[Array[String]]()
    listOfRecords += csvTableHeaderFields.toArray

    train._1.foreach(map => {
      val rowFieldArrayBuffer = ArrayBuffer[String]()
      for ((key,value) <- map) {
        rowFieldArrayBuffer += value.toString
      }
      listOfRecords += rowFieldArrayBuffer.toArray
    })

    csvWriter.writeAll(listOfRecords.toList)
    outputFile.close()
  }
}
