param.poisson.survey <- function(object, num=NULL, bootstrap=FALSE) {
  df <- object$result$df.residual
  sig2 <- summary(object)$dispersion
  
  list(
       simulations = mvrnorm(num, coef(object), vcov(object)),
       alpha = NULL,

       # note: assignment of link and link-inverse are
       #       implicit when the family is assigned
       fam   = object$result$family
       )
}
