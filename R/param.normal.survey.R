#' Param Method for the \code{normal.survey} Zelig Model
#' @note This method is used internally by the \code{survey.zelig} package
#' @S3method param normal.survey
#' @usage \method{param}{normal.survey}(obj, num=1000, ...)
#' @param obj a \code{zelig} object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a \code{parameters} object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.normal.survey <- function(obj, num=1000, ...) {
  df <- obj$result$df.residual
  sig2 <- summary(obj)$dispersion
  
  list(
       simulations = mvrnorm(num, coef(obj), vcov(obj)),
       alpha = sqrt(df*sig2/rchisq(num, df=df)),

       # note: assignment of link and link-inverse are
       #       implicit when the family is assigned
       fam   = gaussian()
       )
}
