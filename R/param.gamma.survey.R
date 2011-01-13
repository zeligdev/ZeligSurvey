param.gamma.survey <- function(object, num=NULL, bootstrap=FALSE) {
  shape <- gamma.shape(object)

  list(
       simulations = mvrnorm(num, coef(object), vcov(object)),
       alpha = rnorm(num, shape$alpha, shape$SE),

       # note: assignment of link and link-inverse are
       #       implicit when the family is assigned
       fam   = Gamma()
       )
}
