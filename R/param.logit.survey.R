param.logit.survey <- function(object, num=NULL, bootstrap=FALSE) {
  list(
       simulations = mvrnorm(num, coef(object), vcov(object)),
       alpha = NULL,

       # note: assignment of link and link-inverse are
       #       implicit when the family is assigned
       fam   = binomial(link="logit")
       )
}
