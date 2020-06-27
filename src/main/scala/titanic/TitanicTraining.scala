package titanic

object TitanicTraining {
  val trainingDataSetVer1 = TitanicDataSet.getPreparedDataSetSixColumns()
  val trainingDataSetVer2 = TitanicDataSet.getPreparedDataSetSevenColumns()
  val trainingDataSetVer3 = TitanicDataSet.getPreparedDatasetEigthColumns()


  def main(args: Array[String]) {
    println("Hallo, Welt!")
    predictAndCreateCsv(trainingDataSetVer1)
    predictAndCreateCsv(trainingDataSetVer2)
    predictAndCreateCsv(trainingDataSetVer3)
  }

  /**
   * create new classificator and train it with train data
   * then predict all data in the test datasets
   * then create new csv file with prediction, of a person survived or not.
   * (csv with column "survived")
   * @param train
   * @return
   */
  def predictAndCreateCsv(train:(List[Map[String,Any]], String, List[String])) = {
    ???
  }
}
