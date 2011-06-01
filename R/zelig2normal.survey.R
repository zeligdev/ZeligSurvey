#' Interface between \code{zelig} and \code{svyglm} for the \code{normal.survey}
#' @note This manual file is largely incomplete, and needs a significant amount
#'   of filling out. This, in itself, might be motivation to divide this
#'   package into more models with more specific function.
#' @param formula a \code{formula}
#' @param weights ...
#' @param ids ...
#' @param probs ...
#' @param strata ...
#' @param fpc ...
#' @param nest ...
#' @param check.strata ...
#' @param repweights ...
#' @param type ...
#' @param combined.weights ...
#' @param rho ...
#' @param bootstrap.average ...
#' @param scale ...
#' @param rscales ...
#' @param fpctype ...
#' @param return.replicates ...
#' @param na.action ...
#' @param start ...
#' @param etastart ...
#' @param mustart ...
#' @param offset ...
#' @param model1 ...
#' @param method ...
#' @param x ...
#' @param y ...
#' @param contrasts ...
#' @param design ...
#' @param data a \code{data.frame}
#' @return a \code{list} used to construct parameters for the \code{svyglm}
#'   function
#' @export
zelig2normal.survey <- function(
                               formula,
                               weights=NULL, 
                               ids=NULL,
                               probs=NULL,
                               strata = NULL,  
                               fpc=NULL,
                               nest = FALSE,
                               check.strata = !nest,
                               repweights = NULL,
                               type,
                               combined.weights=FALSE,
                               rho = NULL,
                               bootstrap.average=NULL, 
                               scale=NULL,
                               rscales=NULL,
                               fpctype="fraction",
                               return.replicates=FALSE,
                               na.action="na.omit",
                               start=NULL,
                               etastart=NULL, 
                               mustart=NULL,
                               offset=NULL, 	      		
                               model1=TRUE,
                               method="glm.fit",
                               x=FALSE,
                               y=TRUE,
                               contrasts=NULL,
                               design=NULL,
                               data
                               ) {
  if (is.null(ids))
    ids <- ~1

  # the following lines designate the design
  # NOTE: nothing truly special goes on here;
  #       the below just makes sure the design is created correctly
  #       for whether or not the replication weights are set
  design <- if (is.null(repweights))
    svydesign(
              data=data,
              ids=ids,
              probs=probs,
              strata=strata,
              fpc=fpc,
              nest=nest,
              check.strata=check.strata,
              weights=weights
              )

  else {
    assign(".survey.prob.weights", weights, envir=.GlobalEnv)
    
    svrepdesign(
                data=data,
                repweights=repweights, 	
                type=type,
                weights=weights,
                combined.weights=combined.weights, 
                rho=rho,
                bootstrap.average=bootstrap.average,
                scale=scale,
                rscales=rscales,
                fpctype=fpctype,
                fpc=fpc
                )
  }

  
  list(
       .function = "svyglm",
       formula = formula,
       design  = design
       )
}

  
