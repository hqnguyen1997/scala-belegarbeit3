package titanic

import org.scalatest.FunSuite


class TitanicDataSetTest extends FunSuite {
  //////////
  test("Test get age category 1") {
    val passenger = Map("age" -> 3)
    assert(TitanicDataSet.getAgeCategory(passenger) == TitanicDataSet.INFANT)
  }

  test("Test get age category 2") {
    val passenger = Map("age" -> 4)
    assert(TitanicDataSet.getAgeCategory(passenger) == TitanicDataSet.CHILD)
  }

  test("Test get age category 3") {
    val passenger = Map("age" -> 10)
    assert(TitanicDataSet.getAgeCategory(passenger) == TitanicDataSet.CHILD)
  }

  test("Test get age category 4") {
    val passenger = Map("age" -> 12)
    assert(TitanicDataSet.getAgeCategory(passenger) == TitanicDataSet.CHILD)
  }

  test("Test get age category 5") {
    val passenger = Map("age" -> 15)
    assert(TitanicDataSet.getAgeCategory(passenger) == TitanicDataSet.ADULT)
  }

  test("Test get age category 6") {
    val passenger = Map("age" -> 50)
    assert(TitanicDataSet.getAgeCategory(passenger) == TitanicDataSet.ADULT)
  }
  test("Test get age category 7") {
    val passenger = Map("age" -> 51)
    assert(TitanicDataSet.getAgeCategory(passenger) == TitanicDataSet.OLD)
  }

  test("Test get age category 8") {
    val passenger = Map("age" -> 80)
    assert(TitanicDataSet.getAgeCategory(passenger) == TitanicDataSet.OLD)
  }
  ///////////

  test("Test get price category 1") {
    val passenger = Map("fare" -> 10.5)
    assert(TitanicDataSet.getFareCategory(passenger) == TitanicDataSet.CHEAP)
  }

  test("Test get price category 2") {
    val passenger = Map("fare" -> 50)
    assert(TitanicDataSet.getFareCategory(passenger) == TitanicDataSet.STANDARD)
  }

  test("Test get price category 3") {
    val passenger = Map("fare" -> 100)
    assert(TitanicDataSet.getFareCategory(passenger) == TitanicDataSet.EXPENSIVE)
  }

  test("Test clean up data eight columns") {
    val cleanedData = TitanicDataSet.cleanUpDataWithEigthColumns()
    cleanedData.foreach(passenger => passenger.size === 8)
  }

  test("Test get features columns") {
    val dataSet = List(Map("age" -> 10, "fare" -> 10.5, "sex" -> "male", "survived" -> 0))
    val features = TitanicDataSet.getFeaturesColumns(dataSet)
    assert(features.size === 3)
    assert(features.contains("age"))
    assert(features.contains("fare"))
    assert(features.contains("sex"))
    assert(!features.contains("survived"))
  }

  test("Test prepared data set, 7 features, 1 target column") {
    val tupel = TitanicDataSet.getPreparedDatasetEigthColumns()

    val featureColumns = tupel._3

    assert(featureColumns.size == 7)
  }

  test("Test prepared data set, 6 features, 1 target column") {
    val tupel = TitanicDataSet.getPreparedDataSetSevenColumns()

    val featureColumns = tupel._3

    assert(featureColumns.size == 6)
  }

  test("Test prepared data set, 5 features, 1 target column") {
    val tupel = TitanicDataSet.getPreparedDataSetSixColumns()

    val featureColumns = tupel._3

    assert(featureColumns.size == 5)
  }
}
