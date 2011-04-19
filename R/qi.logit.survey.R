qi.logit.survey <- function(z, x, x1=NULL, y=NULL, num=1000, param=NULL) {

  model <- GetObject(z)

  coef <- coef(param)
  alpha <- alpha(param)

  eta <- coef %*% t(x)

  link.inverse <- linkinv(param)

  theta <- matrix(link.inverse(eta), nrow=nrow(coef))

  pr <- ev <- matrix(NA, nrow=nrow(theta), ncol(theta))

  dimnames(pr) <- dimnames(ev) <- dimnames(theta)



  #

  ev <- theta

  for (k in 1:ncol(theta)) {
    pr[,k] <- rbinom(length(ev[,k]), 1, ev[,k])
    pr[,k] <- as.character(pr[,k])
  }

  levels(pr) <- c("0", "1")
  
  if (!is.null(y) && NCOL(y))
    y <- y[,1]


  # invisiblify 
  pr1 <- ev1 <- fd <- rr <- NA

  
  if (!is.null(x1)) {
    ev1 <- theta1 <- matrix(link.inverse(coef %*% t(x1)),
                            nrow = nrow(coef)
                            )


    pr1 <- matrix(NA, nrow=nrow(theta), ncol(theta))

    for (k in 1:ncol(theta)) {
      pr1[,k] <- rbinom(length(ev1[,k]), 1, ev1[,k])
      pr1[,k] <- as.character(pr1[,k])
    }

    levels(pr1) <- c("0", "1")
    
    fd <- ev1-ev
    rr <- ev1/ev
  }


  att.ev <- att.pr <- NA

  if (!is.null(y)) {

    yvar <- matrix(rep(y, nrow(coef)),
                   nrow = nrow(coef)
                   )

    tmp.ev <- yvar - ev
    tmp.pr <- yvar - as.integer(pr)

    att.ev <- matrix(apply(tmp.ev, 1, mean), nrow=nrow(coef))
    att.pr <- matrix(apply(tmp.pr, 1, mean), nrow=nrow(coef))
  }

  list(
       "Expected Values: E(Y|X)" = ev,
       "Expected Values (for x1): E(Y|X1)" = ev1,
       "Predicted Values: Y|X" = pr,
       "Predicted Values (for x1): Y|X1" = pr1,
       "First Differences: E(Y|X1)-E(Y|X)" = fd,
       "Risk Ratios: P(Y=1|X1)/P(Y=0|X)" = rr,
       "Average Treatment Effect: Y - EV" = att.ev,
       "Average Treatment Effect: Y - PR" = att.pr
       )
}
