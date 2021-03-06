#' simTestSize: main function of package simTestSize
#'
#' computes a suggested best number of test chunks, i.e., calls to the function passed to the argument drawFunction.
#' @param learningAlgorithm a function that takes learning data as argument and outputs a prediction rule in the form of another function that maps test data to numeric vectors. These can be errors or AUCs, one for each testing data set.
#' @param drawFunction a function that returns a dataframe with the response variable in a column names y
#' @param testChunkSize the sample size of a single test chunk
#' @param g the learning sample sizelearningAlgorithm, drawFunction, Nwithin=12, Nbetween=17)
#' @param Nwithin number of test chunks to be drawn for each training iteration. In the paper, this number was called n_test.
#' @param Nbetween number of training data sets to be drawn. In the paper, this number was calles N.
#' @param conf.level the confidence level; defaults to .95
#' @param ... additional parameters to be passed to the drawFunction
#' @return a list with entries lower, point, upper for the confidence interval for the ratio of the two variances and another for the optimal number of test chunks, the error matrix, the times C and B, and the variance within learning sets (vwl), and the variance between learning sets (vbl).
#' @examples
#' simTestSize(
#' learningAlgorithm=svmLearning,
#' drawFunction=drawLogit,
#' g=20,
#' testChunkSize=1
#' )
#' @export

simTestSize <- function(
    learningAlgorithm,
    drawFunction,
    g,
    conf.level=.95,
    testChunkSize,
    Nwithin=110,
    Nbetween=100,
    ...
) {

                                        # a matrix whose rows correspond to learning sets, i.e., iterations, and columns correspond to test chunks
    errors=errorMatrix(
        learningAlgorithm=learningAlgorithm,
        drawFunction=drawFunction,
        g=g,
        testChunkSize=testChunkSize,
        Nwithin=Nwithin,
        Nbetween=Nbetween,
        ...
    )

                                        # a list with entries A and B
    t <- stopwatch(
        learningAlgorithm=learningAlgorithm,
        drawFunction=drawFunction,
        g=g,
        testChunkSize=testChunkSize,
        ...
    )

    A <- t$A
    B <- t$B
    

    


    
    Nbetween <- dim(errors)[1]
    
                                        # we now view the rows of this matrix as the "subjects" and want a confidence interval for the ratio of the "between subjects" variance to the "within subjects variance"
    
    
                                        # average variance within each learning iteration
    vwl <- mean(apply(errors, 1, var))
                                        # between subjects variance
    vbl <- var(rowMeans(errors))


    alpha <- 1 - conf.level

    confForRatioOfVariances <- list(
        lower=vwl / vbl * qf(alpha/2, df1=Nbetween - 1, df2=Nbetween * (Nwithin - 1)),
        point=vwl / vbl,
        upper=vwl / vbl * qf(1 - alpha/2, df1=Nbetween - 1, df2=Nbetween * (Nwithin - 1))
    )
                                        # A is the "drawing and learning time"
    
    list(
        varRatioLower=confForRatioOfVariances$lower,
        varRatioPoint=confForRatioOfVariances$point,
        varRatioUpper=confForRatioOfVariances$upper,
        ntestOptimalLower = sqrt(confForRatioOfVariances$lower * A / B), 
        ntestOptimal = sqrt(confForRatioOfVariances$point * A / B),
        ntestOptimalUpper = sqrt(confForRatioOfVariances$upper * A / B),
        errors=errors,
        A=A,
        B=B,
        vwl=vwl,
        vbl=vbl
    )
}
