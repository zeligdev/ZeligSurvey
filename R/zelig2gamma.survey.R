zelig2gamma.survey <- function(
                               formula,
                               model,
                               data,
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
                               link=NULL
                               ) {


  # assigning default parameters
  family <- if (is.null(link))
    Gamma
  else
    Gamma(link=link)

  if (is.null(ids))
    ids <- ~1

  # the following lines designate the design
  # NOTE: nothing truly special goes on here;
  #       the below just makes sure the design is created correctly
  #       for whether or not the replication weights are set
  design <- if (is.null(rep.weights))
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
       "svyglm",

       # crucial parameters
       formula=formula,
       design=design,
       family=family,
       method="glm.fit",
       data=data,

       x=x,
       y=y,
       model1=model1,
       contrasts=contrasts,
       rho=rho,
       offset=offset,

       # not-so-important parameters
       na.action=na.action,
       etastart=etastart
       )
}



  
