#' drawLogit
#'
#' drawsDataFromLogitModel
#'
#' @param effectSize the mean difference in the truly predictive genes. Default is 0.7.
#' @param n sample size
#' @param p Number of features
#' @param indicesTrulyPredictive the indices of the truly predictive genes.
#' @return a dataframe with the following variables:
#' itemize{
#' \item{"x"}{the design matrix containing the values of the covariables. Independent observations are in the rows, covariables in the columns.}
#' \item{"y"}{the response vector containing information about the binary indepedent variable.}
#' }
#' @examples
#' drawLogit(n=5, p=2)
#' @export


drawLogit <- function(
    effectSize = 1,
    n = 100,
    p = 4,
    indicesTrulyPredictive = 1
) {

                                        # the coefficient vector
    beta <- rep(0, p)
    beta[indicesTrulyPredictive] <- effectSize
    
                                        # the design
    x = matrix(rnorm(n*p), nrow=n, ncol=p)

    
                                        # the logistic function applied to the linear predictors
    pi = 1 / (1 + exp(-x %*% beta))
    
                                        # the response is drawn
    y <- as.factor(sapply(pi, function(p) rbinom(n=1,size=1,prob=p)))
    designNames = paste("x", 1:p, sep="")
    
    d <- data.frame(
        x=x,
        y=y
    )
    names(d) <- c(designNames, "y")
    d
}
