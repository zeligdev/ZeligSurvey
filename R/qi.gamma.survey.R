qi.gamma.survey <- function(z, x, x1=NULL, y=NULL, num=1000, param=NULL) {
  model <- GetObject(z)

  coef <- coef(param)
  alpha <- alpha(param)

  eta <- coef %*% t(x)

  link.inverse <- linkinv(param)

  theta <- matrix(link.inverse(eta), nrow=nrow(coef))

  pr <- ev <- matrix(NA, nrow=nrow(theta), ncol(theta))

  dimnames(pr) <- dimnames(ev) <- dimnames(theta)


  ev <- theta

  for (i in 1:nrow(ev)) {
    pr[i,] <- rgamma(
                     n     = length(ev[i,]),
                     shape = alpha[i],
                     scale = theta[i,]/alpha[i]
                     )
  }


  # ensure these are no-show
  pr1 <- ev1 <- fd <- NA

  
  # if x1 is available
  if (!is.null(x1)) {
    ev1 <- theta1 <- matrix(link.inverse(coef %*% t(x1)), nrow(coef))
    fd <- ev1-ev
  }


  # ensure these are no-show
  att.pr <- att.ev <- NA


  # I have no clue if this even works
  if (!is.null(y)) {

    yvar <- matrix(
                   rep(y, nrow(simpar)),
                   nrow = nrow(simpar),
                   byrow = TRUE
                   )
    
    tmp.ev <- yvar - ev
    tmp.pr <- yvar - pr

    att.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(param))
    att.pr <- matrix(apply(tmp.pr, 1, mean), nrow = nrow(param))
  }


  list(
       "Expected Values: E(Y|X)" = ev,
       "Expected Values for (X1): E(Y|X1)" = ev1,
       "Predicted Values: Y|X" = pr,
       "Predicted Values (for X1): Y|X1" = pr1,
       "First Differences E(Y|X1)-E(Y|X)" = fd,
       "Average Treatment Effect: Y-EV" = att.ev,
       "Average Treatment Effect: Y-PR" = att.pr
       )
}
