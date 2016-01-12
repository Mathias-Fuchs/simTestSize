#' svmLearning
#'
#' an example learning algorithm: logistic regression
#' @param trainingData the training data: a dataframe whose response column is labelled "y"
#' @return a prediction rule in the form of a function that eats testing data and gives predictions
#' @examples
#' svmLearning(drawLogit(n=1000, effectSize=5))(drawLogit(n=1000, effectSize=5))
#' @export
                                       

svmLearning <- function(trainingData) {
    force(trainingData)
    
    model <- e1071::svm(y~., data=trainingData)
    force(model)
    function(testData) {
        force(testData)
        myAuc(
            y=testData$y,
            prob=predict(
                object=model,
                newdata=testData[, names(testData) != "y"],
                type="response"
            )
        )
    }
}
