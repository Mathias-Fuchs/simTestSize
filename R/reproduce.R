#' reproduce
#'
#' function to reproduce the tables in the paper
#' @return a list with three tables. 
#' @export


reproduce <- function() {
                                        # a single error
    ## rule <- svmLearning(drawLogit(effectSize=10, p=2, n=100))
    ## rule(drawLogit(n=10, effectSize=10))

    ##                                     # should give the same distribution as the code below
    ## errorMatrix(
    ##     learningAlgorithm=svmLearning,
    ##     drawFunction=drawLogit,
    ##     g=100,
    ##     testChunkSize=10,
    ##     effectSize=10,
    ##     Nwithin=10,
    ##     Nbetween=10,
    ##     p=2
    ## )


    results <- data.frame(g=c(10, 30, 1000), es=c(1000, 10, 1), A=NA, B=NA, varRatioPoint = NA, ntestLower=NA, ntestPoint=NA, ntestUpper=NA)
    
    
    for (i in 1:3) {
        r <- simTestSize(
            learningAlgorithm=svmLearning,
            drawFunction=drawLogit,
            g=results$g[i],
            testChunkSize=10,
            effectSize=results$es[i],
            Nwithin=50,
            Nbetween=50,
            p=2
        )

#        print(r$varRatioPoint)
        results$A[i] <- r$A
        results$B[i] <- r$B
        results$varRatioPoint[i] <- r$varRatioPoint
        results$ntestLower[i] <- r$ntestOptimalLower
        results$ntestPoint[i] <- r$ntestOptimal
        results$ntestUpper[i] <- r$ntestOptimalUpper
    }
    results
}





