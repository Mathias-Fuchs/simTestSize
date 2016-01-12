#' logisticLearning
#'
#' an example learning algorithm: logistic regression
#' @param trainingData the training data: a dataframe whose response column is labelled "y"
#' @return a prediction rule in the form of a function that eats testing data and gives predictions
#' @examples
#' logisticLearning(drawLogit(n=1000, effectSize=5))(drawLogit(n=1000, effectSize=5))
#' @export
                                       

logisticLearning <- function(trainingData) {
    model <- glm(data=trainingData, formula=y~., family=binomial)
    function(testData) 
        myAuc(y=testData$y, prob=predict.glm(object=model, newdata= testData[, names(testData) != "y"], type="response"))
}
