package titanic

import org.scalatest.FunSuite

class NaiveBayesClassificatorTest extends FunSuite {
  val targetColumn = "survived"
  // Reduced training data sets for test, enable counting by hand
  val train = Utils.loadDataCSV("train_reduce.csv")

  val realTrainingData = Utils.loadDataCSV("train.csv")
  // Count by hand
  val expectedTrainingTree = Map(
    0 -> Map(
      "pclass" -> Map(1 -> 0.1, 2 -> 0.0, 3 -> 0.9),
      "sex" -> Map("male" -> 0.8, "female" -> 0.2)
    ),
    1 -> Map(
      "pclass" -> Map(1 -> 0.3, 2 -> 0.3, 3 -> 0.4),
      "sex" -> Map("male" -> 0.1, "female" -> 0.9)
    )
  )

  test("Test size of the test data sets and target") {
    assert(train.size === 20)
  }

  test("Test get target classes and count") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex", "age"))
    assert(clf.getTargetClassesCount().size === 2)
    assert(clf.getTargetClassesCount().getOrElse(0, -1) === 10)
    assert(clf.getTargetClassesCount().getOrElse(1, -1) === 10)
  }
  ///////////////////////////////
  test("Test get values of feature 'sex'") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex", "age"))
    assert(clf.getFeatureValues("sex").size === 2)
  }

  test("Test get values of feature 'pclass'") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex", "age"))
    assert(clf.getFeatureValues("pclass").size === 3)
  }

  ///////////////////////////////
  test("Test Count target") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex", "age"))
    assert(clf.countTotalTargetClass(0) === 10)
    assert(clf.countTotalTargetClass(1) === 10)
  }

  ///////////////////////////////
  test("Test count feature 'sex' with target") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex", "age"))
    assert(clf.countTotalFeatureWithTargetClass("male", "sex", 0) === 8)
    assert(clf.countTotalFeatureWithTargetClass("female", "sex", 0) === 2)
    assert(clf.countTotalFeatureWithTargetClass("male", "sex", 1) === 1)
    assert(clf.countTotalFeatureWithTargetClass("female", "sex", 1) === 9)
  }

  test("Test count feature 'pclass' with target") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex", "age"))
    assert(clf.countTotalFeatureWithTargetClass(1, "pclass", 0) === 1)
    assert(clf.countTotalFeatureWithTargetClass(1, "pclass", 1) === 3)
    assert(clf.countTotalFeatureWithTargetClass(2, "pclass", 0) === 0)
    assert(clf.countTotalFeatureWithTargetClass(2, "pclass", 1) === 3)
  }

  test("Test count feature 'age' with target") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex", "age"))
    assert(clf.countTotalFeatureWithTargetClass(22, "age", 0) === 1)
    assert(clf.countTotalFeatureWithTargetClass(38, "age", 1) === 1)
  }

  ///////////////////////////////
  test("Test calculate probability of feature 'sex', value 'male' with target class 0") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex", "age"))
    val targetName = 0
    val featureColumn = "sex"
    val featureValue = "male"
    val targetCount = 10
    val result = clf.computeStats(featureValue, featureColumn, targetName, targetCount)
    assert(result == 8.toDouble/targetCount)
  }

  test("Test calculate probability of feature 'sex', value 'female' with target class 0") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex", "age"))
    val targetName = 0
    val featureColumn = "sex"
    val featureValue = "female"
    val targetCount = 10
    val result = clf.computeStats(featureValue, featureColumn, targetName, targetCount)
    assert(result == 2.toDouble/targetCount)
  }

  test("Test calculate probability of feature 'pclass', value '1' with target class 0") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex", "age"))
    val targetName = 0
    val featureColumn = "pclass"
    val featureValue = 1
    val targetCount = 10
    val result = clf.computeStats(featureValue, featureColumn, targetName, targetCount)
    assert(result == 1.toDouble/targetCount)
  }


  test("Test calculate probability of feature 'pclass', value '1' with target class 1") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex", "age"))
    val targetName = 1
    val featureColumn = "pclass"
    val featureValue = 1
    val targetCount = 10
    val result = clf.computeStats(featureValue, featureColumn, targetName, targetCount)
    assert(result == 3.toDouble/targetCount)
  }
  ///////////////////////////////

  test("Test train") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex"))
    val map = clf.train()
    assert(map == expectedTrainingTree)
  }

  ///////////////////////////////

  test("Test prediction probabilities 1") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex"))
    clf.train()
    val predict = clf.predictProba(Map("pclass" -> 3, "sex" -> "male"))
    assert(~=(predict.getOrElse(0, 0), 0.72, 0.00001))
    assert(~=(predict.getOrElse(1, 0), 0.04, 0.00001))
  }

  test("Test prediction 1") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex"))
    clf.train()
    // A woman in first class should be predicted as survived
    val predict = clf.predict(Map("pclass" -> 1, "sex" -> "female"))
    assert(predict == 1)
  }

  test("Test prediction 2") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex"))
    clf.train()
    // A man in economy class, unfortunately should have been dead.
    val predict = clf.predict(Map("pclass" -> 3, "sex" -> "male"))
    assert(predict == 0)
  }

  // Let try with more data
  test("Test prediction 3") {
    val clf = new NaiveBayesClassificator(realTrainingData, targetColumn, List("pclass", "sex"))
    clf.train()
    // A man in economy class, unfortunately should have been dead.
    val predict = clf.predict(Map("pclass" -> 3, "sex" -> "male"))
    assert(predict == 0)
  }

  test("Test prediction 4") {
    val clf = new NaiveBayesClassificator(train, targetColumn, List("pclass", "sex"))
    clf.train()
    // A woman in first class should be predicted as survived
    val predict = clf.predict(Map("pclass" -> 1, "sex" -> "female"))
    assert(predict == 1)
  }

  def ~=(x: Double, y: Double, precision: Double) = {
    if ((x - y).abs < precision) true else false
  }
}
