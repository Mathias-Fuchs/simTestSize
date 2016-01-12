#' another implementation of the AUC in analogy to the implementation in glmnet
#' @param y a factor with levels 0 and 1
#' @param prob the predicted class probabilities
#' @return the AUC
#' @examples
#' myAuc(y=factor(c(rep(0, 5), rep(1, 5))), prob=sin(1:10)^2)
#' @export



myAuc <- function (y, prob) { 
    rprob = rank(prob)
    n1 = sum(y == 1)
    n0 = length(y) - n1
    u = sum(rprob[y == 1]) - n1 * (n1 + 1)/2
    u/(n1 * n0)
}
