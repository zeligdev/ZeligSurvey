qi.poisson.survey <- function(z, x, x1=NULL, y=NULL, num=1000, param=NULL) {
  model <- GetObject(z)

  coef <- coef(param)
  alpha <- alpha(param)

  eta <- coef %*% t(x)

  link.inverse <- linkinv(param)

  theta <- matrix(link.inverse(eta), nrow=nrow(coef))

  pr <- ev <- matrix(NA, nrow=nrow(theta), ncol(theta))

  dimnames(pr) <- dimnames(ev) <- dimnames(theta)


  ev <- theta


  for (k in 1:nrow(ev))
    pr[k, ] <- rpois(length(ev[k, ]), lambda=ev[k, ])



  ev1 <- pr1 <- fd <- NA

  if (!is.null(x1)) {
    ev1 <- theta1 <- matrix(link.inverse(coef %*% t(x1)),
                            nrow = nrow(coef)
                            )

    fd <- ev1-ev
  }

  att.ev <- att.pr <- NA

  if (!is.null(y)) {
    yvar <- matrix(rep(y, nrow(coef)), nrow=nrow(coef), byrow=TRUE)

    tmp.ev <- yvar - ev
    tmp.pr <- yvar - pr

    att.ev <- matrix(apply(tmp.ev, 1, mean), nrow=nrow(coef))
    att.pr <- matrix(apply(tmp.pr, 1, mean), nrow=nrow(coef))
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
