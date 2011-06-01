#' Describe a \code{gamma.survey} Citation to Zelig
#' @param ... ignored parameters
#' @return a list to be processed by \code{as.description}
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
describe.gamma.survey <- function(...) {
  list(
       authors = "Nicholas Carnes",
       year = 2008,
       description = "Survey-Weighted Gamma Regression for Continuous, Positive Dependent Variables"
       )
}
