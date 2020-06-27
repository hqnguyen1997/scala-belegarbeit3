package titanic

object TitanicDataSet {
  val train = Utils.loadDataCSV("train.csv")
  val total = train.size.toDouble
  val pSurvivedInfant = train.filter(map => map("survived")==1 && getAgeCategory(map)=="Infant").size/total
  val pSurvivedChild = train.filter(map => map("survived")==1 && getAgeCategory(map)=="Child").size/total
  val pSurvivedAdult = train.filter(map => map("survived")==1 && getAgeCategory(map)=="Adult").size/total
  val pSurvivedOld = train.filter(map => map("survived")==1 && getAgeCategory(map)=="Old").size/total

  val INFANT = "Infant"
  val CHILD = "Child"
  val ADULT = "Adult"
  val OLD = "Old"

  val CHEAP = "cheap" //< 15 $
  val STANDARD = "standard" // 15 < x < 70
  val EXPENSIVE = "ex" // > 70

  val NaN = "NaN"

  val targetColumn = "survived"
  /**
   * 1.Variant -> 8 columns (7 features, 1 target) should be use for training and predict
   * @return list of data sets, target column and feature column
   */
  def getPreparedDatasetEigthColumns():(List[Map[String,Any]], String, List[String]) = {
    val cleanedData = cleanUpDataWithEigthColumns()
    val featureColumns = getFeaturesColumns(cleanedData)

    val preparedData = cleanedData.map(passenger => {
      passenger.updated("age", getAgeCategory(passenger)).updated("fare", getFareCategory(passenger))
    })

    (preparedData, targetColumn, featureColumns)
  }

  /**
   * 2.Variant -> 7 columns (6 features, 1 target) should be use for training and predict
   * @return list of data sets, target column and feature column
   */
  def getPreparedDataSetSevenColumns():(List[Map[String,Any]], String, List[String]) = {
    val cleanedData = cleanUpDataWithSevenColumns()
    val featureColumns = getFeaturesColumns(cleanedData)
    val preparedData = cleanedData.map(passenger => {
      passenger.updated("age", getAgeCategory(passenger)).updated("fare", getFareCategory(passenger))
    })

    (preparedData, targetColumn, featureColumns)
  }

  /**
   * 3.Variant -> 6 columns (5 features, 1 target) should be use for training and predict
   * @return list of data sets, target column and feature column
   */
  def getPreparedDataSetSixColumns():(List[Map[String,Any]], String, List[String]) = {
    val cleanedData = cleanUpDataWithSixColumns()
    val featureColumns = getFeaturesColumns(cleanedData)

    val preparedData = cleanedData.map(passenger => {
      passenger.updated("age", getAgeCategory(passenger)).updated("fare", getFareCategory(passenger))
    })

    (preparedData, targetColumn, featureColumns)
  }

  /**
   * Firstly clean up data
   * remove some unused columns and just keep reasonable columns
   * Fill the missing data as well
   */
  def cleanUpDataWithEigthColumns():List[Map[String, Any]] = {
    train.map(passenger => {
      Map(
        "survived" -> passenger.getOrElse("survived", -1), //target column
        "pclass" -> passenger.getOrElse("pclass", -1),
        "sex" -> passenger.getOrElse("sex", NaN),
        "age" -> passenger.getOrElse("age", -1),
        "sibsp" -> passenger.getOrElse("sibsp", -1),
        "fare" -> passenger.getOrElse("fare", -1),
        "cabin" -> passenger.getOrElse("cabin", NaN),
        "embarked" -> passenger.getOrElse("embarked", NaN)
      )
    })
  }

  def cleanUpDataWithSevenColumns():List[Map[String, Any]] = {
    train.map(passenger => {
      Map(
        "survived" -> passenger.getOrElse("survived", -1), //target column
        "pclass" -> passenger.getOrElse("pclass", -1),
        "sex" -> passenger.getOrElse("sex", NaN),
        "age" -> passenger.getOrElse("age", -1),
        "sibsp" -> passenger.getOrElse("sibsp", -1),
        "fare" -> passenger.getOrElse("fare", -1),
        "cabin" -> passenger.getOrElse("cabin", NaN)
      )
    })
  }

  def cleanUpDataWithSixColumns():List[Map[String, Any]] = {
    train.map(passenger => {
      Map(
        "survived" -> passenger.getOrElse("survived", -1), //target column
        "pclass" -> passenger.getOrElse("pclass", -1),
        "sex" -> passenger.getOrElse("sex", NaN),
        "age" -> passenger.getOrElse("age", -1),
        "fare" -> passenger.getOrElse("fare", -1),
        "cabin" -> passenger.getOrElse("cabin", NaN)
      )
    })
  }
  /**
   *
   * @param dataSet
   * @return feature columns name
   */
  def getFeaturesColumns(dataSet:List[Map[String, Any]]):List[String] = {
    dataSet(0).filter(item => item._1 != targetColumn).map(item => item._1).toList
  }

  def createDataSet(): Map[Any, Map[String, Map[Any, Double]]] = {
    Map(0 -> Map(
      "AgeCategory" -> Map("Infant"-> (1 - pSurvivedInfant), "Child"-> (1 - pSurvivedChild),
        "Adult"-> (1 - pSurvivedAdult), "Old"-> (1 - pSurvivedOld))
    ),
      1 -> Map(
        "AgeCategory" -> Map("Infant"-> pSurvivedInfant, "Child"-> pSurvivedChild,
          "Adult"-> pSurvivedAdult, "Old"-> pSurvivedOld)
      )
    )
  }

  def getAgeCategory(passenger:Map[String, Any]):String = {
    val age = passenger.getOrElse("age", -1).toString.toFloat
    if (age == -1) NaN
    else if (age < 4) INFANT
    else if (age < 15) CHILD
    else if (age > 50) OLD
    else ADULT
  }

  def getFareCategory(passenger:Map[String, Any]):String = {
    val fare = passenger.getOrElse("fare", -1).toString.toFloat
    if (fare == -1) NaN
    else if (fare < 15) CHEAP
    else if (fare < 70) STANDARD
    else EXPENSIVE
  }

}
