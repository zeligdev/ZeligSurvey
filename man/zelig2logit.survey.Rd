\name{zelig2logit.survey}
\alias{zelig2logit.survey}
\title{Interface between zelig and svyglm for the logit...}
\usage{zelig2logit.survey(formula, weights, ids, probs, strata, fpc,
    nest=FALSE, check.strata=!nest, repweights, type,
    combined.weights=FALSE, rho, bootstrap.average, scale, rscales,
    fpctype="fraction", return.replicates=FALSE, na.action="na.omit",
    start, etastart, mustart, offset, model1=TRUE, method="glm.fit",
    x=FALSE, y=TRUE, contrasts, design, data)}
\description{Interface between \code{zelig} and \code{svyglm} for the \code{logit.survey}}
\note{This manual file is largely incomplete, and needs a significant amount
of filling out. This, in itself, might be motivation to divide this
package into more models with more specific function.}
\value{a \code{list} used to construct parameters for the \code{svyglm}
function}
\arguments{\item{formula}{a \code{formula}}
\item{weights}{...}
\item{ids}{...}
\item{probs}{...}
\item{strata}{...}
\item{fpc}{...}
\item{nest}{...}
\item{check.strata}{...}
\item{repweights}{...}
\item{type}{...}
\item{combined.weights}{...}
\item{rho}{...}
\item{bootstrap.average}{...}
\item{scale}{...}
\item{rscales}{...}
\item{fpctype}{...}
\item{return.replicates}{...}
\item{na.action}{...}
\item{start}{...}
\item{etastart}{...}
\item{mustart}{...}
\item{offset}{...}
\item{model1}{...}
\item{method}{...}
\item{x}{...}
\item{y}{...}
\item{contrasts}{...}
\item{design}{...}
\item{data}{a \code{data.frame}}}
