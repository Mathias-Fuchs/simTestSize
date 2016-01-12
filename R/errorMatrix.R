#' errorMatrix: the error matrix.
#'
#' the matrix containing the elementary error estimators. Their rows correspond to the learning sets, and columns correspond to the test chunks.
#' @param learningAlgorithm a function that takes learning data as argument and outputs a prediction rule in the form of another function that maps test data to numeric vectors. These can be errors or AUCs, one for each testing data set.
#' @param drawFunction a function that returns a dataframe with the response variable in a column names y
#' @param testChunkSize the sample size of a single test chunk
#' @param g the learning sample sizelearningAlgorithm, drawFunction, Nwithin=12, Nbetween=17)
#' @param Nwithin number of test chunks to be drawn for each training iteration. In the paper, this number was called n_test. Defaults to 110.
#' @param Nbetween number of training data sets to be drawn. In the paper, this number was calles N. Defaults to 100.
#' @param ... additional parameters to be passed to the drawing function
#' @return the error matrix
#' @examples errorMatrix(
#' learningAlgorithm=svmLearning,
#' drawFunction=drawLogit,
#' g=20,
#' testChunkSize=1
#' )
#' @export


errorMatrix <- function(learningAlgorithm, drawFunction, g, testChunkSize, Nwithin=110, Nbetween=100, ...) {


    errors <- matrix(NA, nrow=Nwithin, ncol=Nbetween)
    for (i in 1:Nwithin) {
        rule <- svmLearning(drawLogit(effectSize=10, p=2, n=100))
        for (j in 1:Nbetween) 
            errors[i, j] <- rule(drawLogit(n=10, effectSize=10))
    }



    
    ## errors <- t(
    ##     replicate(
    ##         Nbetween, 
    ##         {
    ##             learningData <- drawFunction(n=g, ...)
    ##             predictionRule <- learningAlgorithm(learningData)
    ##             replicate(Nwithin, predictionRule(drawFunction(n=testChunkSize, ...)))
    ##         }
    ##     )
    ## )
    
    errors <- errors[which(sapply(1:nrow(errors), function(i) all(!is.na(errors[i, ])))), ]
    
    errors
}
