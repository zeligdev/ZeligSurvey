#' Interface between \code{zelig} and \code{svyglm} for the \code{gamma.survey}
#' @note This manual file is largely incomplete, and needs a significant amount
#'   of filling out. This, in itself, might be motivation to divide this
#'   package into more models with more specific function.
#' @param formula a \code{formula}
#' @param weights NULL
#' @param ids NULL
#' @param probs NULL
#' @param strata NULL  
#' @param fpc NULL
#' @param nest FALSE
#' @param check.strata !nest
#' @param repweights NULL
#' @param type ... ...
#' @param combined.weights FALSE
#' @param rho   NULL
#' @param bootstrap.average NULL
#' @param scale NULL
#' @param rscales NULL
#' @param fpctype "fraction"
#' @param return.replicates FALSE
#' @param na.action "na.omit"
#' @param start NULL
#' @param etastart NULL
#' @param mustart NULL
#' @param offset NULL	      		
#' @param model1 TRUE
#' @param method "glm.fit"
#' @param x FALSE
#' @param y TRUE
#' @param contrasts NULL
#' @param design NULL
#' @param link "inverse"
#' @param data a \code{data.frame}
#' @param ... ignored parameters
#' @return a \code{list} used to construct parameters for the \code{svyglm}
#'   function
#' @export
zelig2gamma.survey <- function(
                               formula,
                               weights=NULL, 
                               ids=NULL,
                               probs=NULL,
                               strata = NULL,  
                               fpc = NULL,
                               nest = FALSE,
                               check.strata = !nest,
                               repweights = NULL,
                               type,
                               combined.weights = FALSE,
                               rho = NULL,
                               bootstrap.average = NULL, 
                               scale = NULL,
                               rscales = NULL,
                               fpctype = "fraction",
                               return.replicates=FALSE,
                               na.action = "na.omit",
                               start = NULL,
                               etastart = NULL, 
                               mustart = NULL,
                               offset = NULL, 	      		
                               model1 = TRUE,
                               method = "glm.fit",
                               x = FALSE,
                               y = TRUE,
                               contrasts = NULL,
                               design = NULL,
                               link = "inverse",
                               data,
                               ...
                               ) {
  if (is.null(ids))
    ids <- ~1

  # the following lines designate the design
  # NOTE: nothing truly special goes on here;
  #       the below just makes sure the design is created correctly
  #       for whether or not the replication weights are set
  design <- if (is.null(repweights)) {
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
  }

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

  list(.function = "svyglm",
       
       formula = formula,
       design  = design,
       family  = Gamma()
       )
}
