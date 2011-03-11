plot.sim.probit.survey <- function(
                                   sim.object,
                                   xlab="",
                                   user.par=FALSE,
                                   ...
                                   ) {


  # save old device (??)
  old.par <- par(no.readonly=TRUE)


  # get quantities of interest
  qi <- sim.object$qi


  # 
  cols <- ifelse(is.null(qi$ev2), 1, 2)
  rows <- ifelse(is.null(qi$att1), 3, 4)


  #
  par(mfrow=c(rows, cols))

  #
  if (!(is.null(qi$ev1) || is.na(qi$ev1)))
    truehist(
             qi$ev1,
             main = "Expected Values: E(Y|X)",
             xlab = paste("N =", length(qi$ev1)),
             ylab = "Density", col="red"
             )

  #
  if (!(is.null(qi$ev2) || is.na(qi$ev2)))
    truehist(
             qi$ev2,
             main = "Expected Values: E(Y|X1)", 
             xlab = paste("N =", length(qi$ev2)),
             ylab = "Density", col="cyan"
             )

  #
  if (!(is.null(qi$pv1) || is.na(qi$pv1))) {
    y0 <- 100 * sum(qi$pv1 == 0)/length(qi$pv1)
    y1 <- 100 * sum(qi$pv1 == 1)/length(qi$pv1)
    
    barplot(
            c(y0, y1),
            main="Predicted Values: E(Y|X)",
            xlab="",
            names.arg = c("Y = 0", "Y = 1"), horiz = TRUE,
            col="red"
            )
  }

  if (!(is.null(qi$pv2) || is.na(qi$pv2))) {
    y0 <- 100 * sum(qi$pv2 == 0)/length(qi$pv2)
    y1 <- 100 * sum(qi$pv2 == 1)/length(qi$pv2)
    
    barplot(
            c(y0, y1),
            main="Predicted Values: Y|X1",
            xlab="",
            names.arg = c("Y = 0", "Y = 1"), horiz=TRUE,
            col="cyan"
            )
  }

  #
  if (!(is.null(qi$fd) || is.na(qi$fd)))
    plot(density(qi$fd), main="First Differences: E(Y|X1)-E(Y|X)")

  #
  if (!(is.null(qi$rr) || is.na(qi$rr)))
    plot(density(qi$rr), main="Risk Ratios: P(Y=1|X1)/P(Y=0|X)")

  if (!(is.null(qi$att1) || is.na(qi$att1)))
    plot(density(qi$att1), main="Average Treatment Effect: Y - EV")

  if (!(is.null(qi$att2) || is.na(qi$att2)))
    plot(density(qi$att1), main="Average Treatment Effect: Y - PR")
  #

  #
  par(old.par)
}

plot.sim.gamma.survey <- plot.sim.probit.survey
plot.sim.logit.survey <- plot.sim.probit.survey
plot.sim.normal.survey <- plot.sim.probit.survey
plot.sim.poisson.survey <- plot.sim.probit.survey


