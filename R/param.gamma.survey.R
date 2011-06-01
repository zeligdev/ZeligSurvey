#' Param Method for the \code{gamma.survey} Zelig Model
#' @note This method is used internally by the \code{survey.zelig} package
#' @S3method param gamma.survey
#' @usage \method{param}{gamma.survey}(obj, num=1000, ...)
#' @param obj a \code{zelig} object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a \code{parameters} object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.gamma.survey <- function(obj, num=1000, ...) {
  shape <- gamma.shape(obj)

  list(
       simulations = mvrnorm(num, coef(obj), vcov(obj)),
       alpha = rnorm(num, shape$alpha, shape$SE),

       # note: assignment of link and link-inverse are
       #       implicit when the family is assigned
       fam   = Gamma()
       )
}
