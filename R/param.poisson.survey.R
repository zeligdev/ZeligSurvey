#' Param Method for the \code{poisson.survey} Zelig Model
#' @note This method is used internally by the \code{survey.zelig} package
#' @S3method param poisson.survey
#' @usage \method{param}{poisson.survey}(obj, num=1000, ...)
#' @param obj a \code{zelig} object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a \code{parameters} object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.poisson.survey <- function(obj, num=1000, ...) {
  list(
       simulations = mvrnorm(num, coef(obj), vcov(obj)),
       alpha = NULL,

       # note: assignment of link and link-inverse are
       #       implicit when the family is assigned
       fam   = poisson()
       )
}
