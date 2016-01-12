#' stopwatch
#'
#' measures computer time for learning and testing.
#' There is a difficulty with the lazy evaluation mechanism in R: since the learning funciton returns a function that acts as a closure and keeps a reference to its calling environment, and the model is not learnt before actually evaluating the function, we have to measure the times A and B indirectly.
#' Therefore, this function approximates the learning time by measuring the time for learning and testing k times, in such a way that the evaluation promise has to be fulfilled, minus the time for drawing, for various values of k, and then fitting the linear model that the total time is A + kB.
#' @param learningAlgorithm a function that takes learning data as argument and outputs a prediction rule in the form of another function that maps test data to numeric vectors. These can be errors or AUCs, one for each testing data set.
#' @param drawFunction a function that returns a dataframe with the response variable in a column names y
#' @param g the learning sample sizelearningAlgorithm, drawFunction, Nwithin=12, Nbetween=17)
#' @param testChunkSize the sample size of a single test chunk
#' @param ... additional parameters to be passed to the drawing function
#' @return a list with the times A and B in nanoseconds
#' @examples
#' stopwatch(
#' learningAlgorithm=svmLearning,
#' drawFunction=drawLogit,
#' g=20,
#' testChunkSize=1
#' )
#' @export

stopwatch <- function(learningAlgorithm, drawFunction, g, testChunkSize, ...) {
                                        # A is the learning time
                                        # measureDrawingTime
    time <- data.frame(k=c(5, 10, 15, 20), elapsed=NA)
    for (i in 1:nrow(time)) 
        time$elapsed[i] <- mean(microbenchmark::microbenchmark(
            {
                predictionRule <- learningAlgorithm(drawFunction(n=g, ...))
                error <- predictionRule(drawFunction(n=testChunkSize * time$k[i], ...))
            },
            times=4)$time)
    
    linearmodel <- lm(elapsed~k, time)
    list(
        A=as.numeric(linearmodel$coefficients[1]),
        B=as.numeric(linearmodel$coefficients[2])
    )
}
