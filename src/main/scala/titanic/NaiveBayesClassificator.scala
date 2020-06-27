package titanic

/**
 * @param train => list of feature and value
 * @param targetColumn => target column name
 * @param featureColumns => feature column names
 */
class NaiveBayesClassificator(train:List[Map[String, Any]], targetColumn:String, featureColumns:List[String]) {

  var trainingTree:Map[Any, Map[String, Map[Any, Double]]] = null

  /**
   * Function create traning tree
   * @return training tree
   */
  def train():Map[Any, Map[String, Map[Any, Double]]] = {
    val targetCount = getTargetClassesCount()

    trainingTree = targetCount.map(item => {
      (item._1, featureColumns.map(featureColumnName => {
        (featureColumnName, getFeatureValues(featureColumnName).map(featureValue => (featureValue, computeStats(featureValue, featureColumnName, item._1, item._2))).toMap)
      }).toMap)
    })

    trainingTree
  }

  /**
   * predict and return probability corresponding each target class
   * @param data
   */
  def predictProba(data:Map[String, Any]):Map[Any, Double] = {
    trainingTree.map(item => {
      (item._1, data.map(feature => { //feature._1 -> feature name, feature._2 -> feature value
        item._2.getOrElse(feature._1, null).getOrElse(feature._2, 1.0)
      }).foldLeft(1.0)((X, Y) => X*Y))
    })
  }

  /**
   * predict and return in which class the data should likely be
   * @param data
   */
  def predict(data:Map[String, Any]):Any = {
    predictProba(data).maxBy(item => item._2)._1
  }

  /**
   * function calculate probability for each feature and target class
   * @param featureValue
   * @param featureColumn
   * @param targetName target class name
   * @param targetCount target class count
   * @return Map(feature -> x/y,...)
   *         x => frequenz of feature combine with target class
   *         y => frequenz of target class
   */
  def computeStats(featureValue:Any, featureColumn:String, targetName:Any, targetCount:Int):Double = {
    val featureCount = countTotalFeatureWithTargetClass(featureValue, featureColumn, targetName)
    featureCount.toDouble/targetCount
  }

  def countTotalTargetClass(targetClass:Any):Int = {
    train.filter(item => item.getOrElse(targetColumn, AnyRef) == targetClass).size
  }

  def countTotalFeatureWithTargetClass(featureValue:Any, featureColumn:String, targetName: Any):Int = {
    train.filter(
      item => item.getOrElse(targetColumn, AnyRef) == targetName && item.getOrElse(featureColumn, AnyRef) == featureValue
    ).size
  }

  def getTargetClassesCount():Map[Any, Int] = {
    train.map(item => item.getOrElse(targetColumn, AnyRef)).foldLeft(Map[Any, Int]())((base, element) => base.updated(element, base.getOrElse(element, 0) + 1))
  }

  def getFeatureValues(featureColumn:String):List[Any] = {
    train.map(item => item.getOrElse(featureColumn, AnyRef))
          .foldLeft(List[Any]())((base, element) => if (!base.contains(element)) element :: base else base)
  }
}
