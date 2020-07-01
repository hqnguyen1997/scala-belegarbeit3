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
  // No idea why the function loadData always adds survived column while loading "test.csv"
  // And fills this column with -1
  // So this column should be filtered out
  val testData = Utils.loadDataCSV("test.csv")

 def main(args: Array[String]) {
    println("Start training")
    predictAndCreateCsv(trainingDataSetVer1, testData,"trainingVer1.csv")
    predictAndCreateCsv(trainingDataSetVer2, testData,"trainingVer2.csv")
    predictAndCreateCsv(trainingDataSetVer3, testData,"trainingVer3.csv")
  }

  /**
   * create new classificator and train it with train data
   * then predict all data in the test datasets
   * then create new csv file with prediction, of a person survived or not.
   * (csv with column "survived")
   * @param train (trainning data, targetcolumn, featurecolumn)
   * @param test Test data sets
   * @param outputFileName
   * @return
   */
  def predictAndCreateCsv(train:(List[Map[String,Any]], String, List[String]), test:List[Map[String,Any]], outputFileName: String) = {
    val outputFile = new BufferedWriter(new FileWriter(outputFileName))
    val csvWriter = new CSVWriter(outputFile)
    val featureColumn = train._2
    // Train model
    val clf = new NaiveBayesClassificator(train._1, train._2, train._3)
    clf.train()

    // Create CSV
    val csvTableHeaderFields = ArrayBuffer[String]()
    // No idea why the function loadData always adds survived column while loading "test.csv"
    // And fills this column with -1
    // So this column should be filtered out
    val filteredTestData = test.map(item => item.filter(column => column._1 != featureColumn))

    filteredTestData(0).foreach(item => csvTableHeaderFields += item._1)
    csvTableHeaderFields += featureColumn // target class column

    val records = new ListBuffer[Array[String]]()
    records += csvTableHeaderFields.toArray

    filteredTestData.foreach(data => {
      val record = ArrayBuffer[String]()

      csvTableHeaderFields.foreach(header => {
        if (header != featureColumn)
          record += data.getOrElse(header, "NaN").toString
      })
      record += clf.predict(data).toString

      records += record.toArray
    })
    // Write to file
    csvWriter.writeAll(records.toList)
    outputFile.close()
  }
}
