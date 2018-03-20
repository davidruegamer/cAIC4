#' Function to stepwise select the (generalized) linear mixed model
#' fitted via (g)lmer() or (generalized) additive (mixed) model
#' fitted via gamm4() with the smallest cAIC.
#' 
#' 
#' The step function searches the space of possible models in a greedy manner,
#' where the direction of the search is specified by the argument
#' direction. If direction = "forward" / = "backward", 
#' the function adds / exludes random effects until the cAIC can't be improved.
#' In the case of forward-selection, either a new grouping structure, new
#' slopes for the random effects or new \code{s()}-terms must be supplied to the function call.
#' If direction = "both", the greedy search is alternating between forward
#' and backward steps, where the direction is changed after each step
#'
#'@param object fit by \code{[lme4]{lmer}}, \code{[lme4]{glmer}} or \code{[gamm4]{gamm4}} 
#'for which the stepwise procedure is to be computed
#'@param groupCandidates see slopeCandidates. Group nesting must be initiated manually, i.e. by 
#'listing up the string of the groups in the manner of lme4. For example \code{groupCandidates = c("a", "b", "a/b")}.   
#'@param slopeCandidates character vectors containing names of possible new random effect groups / slopes
#'@param fixEfCandidates character vector containing names of possible (non-)linear fixed effects in the GAMM; 
#'NULL for the (g)lmer-use case 
#'@param direction character vector indicating the direction in c("both","backward","forward")
#'@param numberOfPermissibleSlopes how much slopes are permissible for one group RE
#'@param trace logical; should information ne printed during the running of stepcAIC?
#'@param steps maximum number of steps to be considered
#'@param keep list($fixed,$random) of formulae; which splines / fixed (fixed) or random effects (random) to be 
#'kept during selection must be included in the original model 
#'@param numCores the number of cores to be used in calculations; this is done by using \code{parallel::mclapply}
#'@param data data.frame, from which the new REs are to be taken
#'@param returnResult logical; whether to return the result (best model and corresponding cAIC)
#'@param calcNonOptimMod logical; if FALSE, models which failed to converge are not considered for cAIC calculation
#'@param bsType type of splines to consider in forward gamm4 steps
#'@param allowUseAcross allow slopes to be used in other grouping variables
#'@param ... options for cAIC call
#'@section Details: 
#' For use with "gamm4-objects": 
#' groupCandidates are interpreted as covariables and fitted as splines.
#' If groupCandidates does include characters such as "s(..,bs='tp')" 
#' the respective spline is included in the forward stepwise procedure.
#' @return if \code{returnResult} is \code{TRUE}, a list with the best model \code{finalModel} and
#' the corresponding cAIC \code{bestCAIC} is returned. 
#' 
#' Note that if \code{trace} is set to \code{FALSE} and \code{returnResult}
#' is also \code{FALSE}, the function call may not be meaningful
#' @author David Ruegamer
#' @export
#' @import parallel
#' @importFrom stats as.formula dbinom dnorm dpois family
#' formula glm lm model.frame model.matrix
#' predict qlogis reformulate simulate terms
#' @importFrom utils combn
#' @importFrom stats4 logLik
#' @examples 
#' 
#' (fm3 <- lmer(strength ~ 1 + (1|sample) + (1|batch), Pastes))
#' 
#' fm3_step <- stepcAIC(fm3, direction = "backward", trace = TRUE, data = Pastes)
#' 
#' fm3_min <- lm(strength ~ 1, data=Pastes)
#' 
#' fm3_min_step <- stepcAIC(fm3_min, groupCandidates = c("batch", "sample"), 
#' direction="forward", data=Pastes, trace=TRUE)
#' fm3_min_step <- stepcAIC(fm3_min, groupCandidates = c("batch", "sample"), 
#' direction="both", data=Pastes, trace=TRUE)
#' # try using a nested group effect which is actually not nested -> warning
#' fm3_min_step <- stepcAIC(fm3_min, groupCandidates = c("batch", "sample", "batch/sample"), 
#'                          direction="both", data=Pastes, trace=TRUE)
#' 
#' Pastes$time <- 1:dim(Pastes)[1]
#' fm3_slope <- lmer(data=Pastes, strength ~ 1 + (1 + time | cask))
#' 
#' fm3_slope_step <- stepcAIC(fm3_slope,direction="backward", trace=TRUE, data=Pastes)
#' 
#' 
#' 
#' fm3_min <- lm(strength ~ 1, data=Pastes)
#' 
#' fm3_min_step <- stepcAIC(fm3_min,groupCandidates=c("batch","sample"),
#' direction="forward", data=Pastes,trace=TRUE)
#' 
#' 
#' 
#' fm3_inta <- lmer(strength ~ 1 + (1|sample:batch), data=Pastes)
#' 
#' fm3_inta_step <- stepcAIC(fm3_inta,groupCandidates=c("batch","sample"),
#' direction="forward", data=Pastes,trace=TRUE)
#' 
#' fm3_min_step2 <- stepcAIC(fm3_min,groupCandidates=c("cask","batch","sample"),
#' direction="forward", data=Pastes,trace=TRUE)
#' 
#' fm3_min_step3 <- stepcAIC(fm3_min,groupCandidates=c("cask","batch","sample"),
#' direction="both", data=Pastes,trace=TRUE)
#' 
#' \dontrun{
#' fm3_inta_step2 <- stepcAIC(fm3_inta,direction="backward", 
#' data=Pastes,trace=TRUE)
#' }
#' 
#' ##### create own example
#' 
#' 
#' na <- 20
#' nb <- 25
#' n <- 400
#' a <- sample(1:na,400,replace=TRUE)
#' b <- factor(sample(1:nb,400,replace=TRUE))
#' x <- runif(n)
#' y <- 2 + 3 * x + a*.02 + rnorm(n) * .4
#' a <- factor(a)
#' c <- interaction(a,b)
#' y <- y + as.numeric(as.character(c))*5
#' df <- data.frame(y=y,x=x,a=a,b=b,c=c)
#' 
#' smallMod <- lm(y ~ x)
#' 
#' \dontrun{
#' # throw error
#' stepcAIC(smallMod, groupCandidates=c("a","b","c"), data=df, trace=TRUE, returnResult=FALSE)
#' 
#' smallMod <- lm(y ~ x, data=df)
#' 
#' # throw error
#' stepcAIC(smallMod, groupCandidates=c("a","b","c"), data=df, trace=TRUE, returnResult=FALSE)
#' 
#' # get it all right
#' mod <- stepcAIC(smallMod, groupCandidates=c("a","b","c"), 
#'                 data=df, trace=TRUE, 
#'                 direction="forward", returnResult=TRUE)
#' 
#' # make some more steps...
#' stepcAIC(smallMod, groupCandidates=c("a","b","c"), data=df, trace=TRUE, 
#'          direction="both", returnResult=FALSE)
#' 
#' mod1 <- lmer(y ~ x + (1|a), data=df)
#' 
#' stepcAIC(mod1, groupCandidates=c("b","c"), data=df, trace=TRUE, direction="forward")
#' stepcAIC(mod1, groupCandidates=c("b","c"), data=df, trace=TRUE, direction="both")
#' 
#' 
#' 
#' mod2 <- lmer(y ~ x + (1|a) + (1|c), data=df)
#' 
#' stepcAIC(mod2, data=df, trace=TRUE, direction="backward")
#' 
#' mod3 <- lmer(y ~ x + (1|a) + (1|a:b), data=df)
#' 
#' stepcAIC(mod3, data=df, trace=TRUE, direction="backward")
#' 
#' }
#' 
stepcAIC <- function(object, 
                     groupCandidates=NULL,
                     slopeCandidates=NULL,
                     fixEfCandidates=NULL,
                     numberOfPermissibleSlopes=2,
                     allowUseAcross=FALSE,
                     direction = "backward",
                     trace = FALSE,
                     steps = 50, 
                     keep = NULL,
                     numCores = 1,
                     data = NULL,
                     returnResult=TRUE,
                     calcNonOptimMod=TRUE,
                     bsType="tp",
                     ...)
{

  #######################################################################
  ########################## pre-processing #############################
  #######################################################################

  
  if(!is.null(data)){
    data <- get(deparse(substitute(data)), envir = parent.frame())
  }else if(inherits(object, c("lmerMod", "glmerMod"))){
    data <- get(deparse(substitute(object@call[["data"]])), envir = parent.frame())
  }else{
    stop("argument data must be supplied!")
  }
  possible_predictors <- colnames(data)
  
  ### build nesting in groupCandidates
  nestCands <- groupCandidates[grep("/", groupCandidates)]
  nestCands <- nestCands[!nestCands %in% possible_predictors]
  for(nc in nestCands){ 
    # check if really nested
    ncc <- trimws(strsplit(nc, "/")[[1]])
    if(!isNested(data[,ncc[1]], data[,ncc[2]])){
      warning(paste0("Dropping incorrect nesting group ", nc, " from groupCandidates."))
    }else{
      groupCandidates <- unique( c(groupCandidates, allNestSubs(nc)) )
    }
    groupCandidates <- setdiff(groupCandidates, nc)
    
  }
  # intaCands <- groupCandidates[grep(":", groupCandidates)]
  # if(length(intaCands) > 0) intaCands <- intaCands[!intaCands %in% possible_predictors]
  # for(ic in intaCands){
  #   sepIc <- trimws(strsplit(ic, ":")[[1]])
  #   if(cor(sapply(data[,sepIc], as.numeric))==1)
  #     stop(paste0("Interaction of ", sepIc, " not meaningful."))
  # }
  
  
  
  existsNonS <- FALSE
  
  ### check if gamm4-call
  
  if(is.list(object) & length(object)==2 & all(c("mer","gam") %in% names(object))){

    if(allowUseAcross | !is.null(slopeCandidates))
      stop("allowUseAcross and slopeCandidates are not permissible for gamm4-objects!")
    
    ig <- mgcv::interpret.gam(object$gam$formula)
    existsNonS <- length(ig$smooth.spec)<length(ig$fake.names)
    
    if( !is.null(fixEfCandidates) ) stopifnot( fixEfCandidates %in% possible_predictors )
        
  }else{
    
    if( !is.null(groupCandidates) ) stopifnot( all ( groupCandidates %in% possible_predictors ) | 
                                                 ( unlist(strsplit(groupCandidates, ":")) %in% 
                                                     possible_predictors ) )
    if( !is.null(slopeCandidates) ) stopifnot( slopeCandidates %in% possible_predictors )
    
  }
  
  
  #######################################################################
  ##########################   entry step   #############################
  #######################################################################
  
  # -> get cAIC of input model
    
  if(inherits(object, c("lmerMod", "glmerMod")) | "mer"%in%names(object)){
      
    timeBefore <- Sys.time()
    cAICofMod <- tryCatch(cAIC(object,...)$caic, error = function(e){
      
      cat("\n\nThe cAIC of the initial model can not be calculated. Continue Anyway?")
      readline("If so, type 'y': ")
      
    })
    if(!is.numeric(cAICofMod) && cAICofMod=="y"){ 
      cAICofMod <- Inf 
      }else if(!is.numeric(cAICofMod) && cAICofMod!="y") return(NULL)
    
    timeForCalc <- Sys.time() - timeBefore

  }else if(any(class(object)%in%c("lm","glm"))){
    
      # ll <- getGLMll(object)
      # bc <- attr(logLik(object),"df")
      cAICofMod <- cAIC(object)$caic #-2*ll + 2*bc
      
      if(direction=="backward") stop("A simple (generalized) linear model can't be reduced!")

  }else{
    
    stop("Class of object is not known")
    
  }


  # check if call is inherently consistent
  
  stopifnot( 
    
    direction=="backward" | 
      
      ( direction %in% c("forward","both") & 
          ( !is.null(groupCandidates) | !is.null(slopeCandidates) | !is.null(fixEfCandidates) ) ) | 
      
      ( direction %in% c("forward","both") & 
          is.null(groupCandidates) & is.null(slopeCandidates) & is.null(fixEfCandidates) &
          ( allowUseAcross | existsNonS ) ) 
  )
  
  if( direction=="backward" & !( is.null(groupCandidates) & is.null(slopeCandidates) & is.null(fixEfCandidates) )
      ) warning("I will ignoring variables in group- / slopeCandidates or fixEfCandidates for backward selection.")
  

  #######################################################################
  ##########################      (end)     #############################
  #######################################################################
  
  #######################################################################
  ####################### iteratively fitting ###########################
  #######################################################################
  
  # indicator to break while loop
  stepsOver <- FALSE
  
  # indicator for direction=="both"
  dirWasBoth <- ifelse( direction=="both", TRUE, FALSE )
  
  # indicator for improvement in direction=="both" - step
  improvementInBoth <- FALSE
  
  # indicator for check if step procedure didnt yield better
  # results compared to the previous step
  equalToLastStep <- FALSE
  
  # change direction to either forward or backward
  direction <- ifelse( direction%in%c("both","forward"),"forward","backward" )
  
  # get the initial number of steps
  stepsInit <- steps
  
  
  
  ###################################################################
  ####################### iterative part ############################
  ###################################################################
  
  if(trace){
    
    cat("Starting stepwise procedure...")
    cat("\n_____________________________________________\n")
    cat("_____________________________________________\n")
    
  }
  
  
  # try to improve the model as long as stepsOver==FALSE
  while(!stepsOver){

    # get all components needed for stepping procedure
    comps <- getComponents(object)

    ########################### printing ##############################
    
    if(trace) {
    
      cat("\nStep ",stepsInit-steps+1," (",direction,"):  cAIC=", format(round(cAICofMod, 4)), "\n", 
            "Best model so far: ", makePrint(object), "\n",
          "New Candidates:\n\n",
          sep = "")
      utils::flush.console()
    
    }
    
    ###################################################################
    
  
    steps = steps - 1
      
    newSetup <- if(direction=="forward"){
      
                       makeForward(comps=comps,
                                   slopeCandidates=slopeCandidates,
                                   groupCandidates=groupCandidates,
                                   fixEfCandidates=fixEfCandidates,
                                   nrOfCombs=numberOfPermissibleSlopes,
                                   allowUseAcross=allowUseAcross,
                                   bsType=bsType,
                                   keep=keep)
    }else{
      
      makeBackward(comps=comps,
                   keep=keep)
      
    }
    
    newSetup <- mergeChanges(initialParts=comps, listParts=newSetup)
    
    ### ( print ) ###
    
    if(trace & !is.null(newSetup)) cat("Calculating cAIC for", 
                                       length(newSetup),
                                       "model(s) ...")
    
    #############
    
    ### calculate all other models and cAICs
    
    tempRes <- if(!is.null(newSetup)){
      
                      calculateAllCAICs(newSetup=newSetup,
                                        modelInit=object,
                                        numCores=numCores,
                                        data=data,
                                        calcNonOptimMod=calcNonOptimMod,
                                        ...)
                      
    }
     
    ##############
    
    if(is.list(tempRes) & !is.null(tempRes$message)){ # gamm4 with error
      
      warning(paste0("There are zero variance components.\n", tempRes$message))
      
      if(returnResult){
        return(list(finalModel=object,
                    bestCAIC=NA)
        )
      }else{
        return(invisible(NULL))
      }
      
    }
    
    
    ### get performance
      
    aicTab <- as.data.frame(tempRes$aicTab)
    
    ### ( print ) ###
    
    
    if (trace) {
      cat("\r\r\r\r\r\r\r\r\r\r\r\r\r")
      print(aicTab[with(aicTab,order(-caic)), ], row.names = FALSE)
      cat("\n_____________________________________________\n")
      cat("_____________________________________________\n")
      utils::flush.console()
    }
    
    bestModel <- tempRes$bestMod
    indexMinCAIC <- which.min(aicTab$caic)
    minCAIC <- ifelse(length(indexMinCAIC)==0, Inf, aicTab$caic[indexMinCAIC]) 
    keepList <- list(random=interpret.random(keep$random),gamPart=NULL)
    if(!is.null(keep$fixed)) keepList$gamPart <- mgcv::interpret.gam(keep$fixed)

    ###############################################################################
    ###############################################################################
    ############################# - decision part - ###############################
    ###############################################################################
    ###############################################################################

    if( minCAIC==Inf ){
      
      if(dirWasBoth){
        
        direction <- ifelse( direction=="forward", "backward", "forward" )
        improvementInBoth <- FALSE 
        
      }else{
        
        stepsOver <- TRUE
        bestModel <- object
        minCAIC <- cAICofMod
        
      }
      
    }else if(
      
        ( minCAIC <= cAICofMod & !dirWasBoth & direction=="backward" & any(class(bestModel)%in%c("glm","lm")) ) 
        # if backward step procedure reached (g)lm
      
      |
        
        ( minCAIC <= cAICofMod & !dirWasBoth & direction=="backward" & 
          is.logical(all.equal(newSetup[[which.min(aicTab$caic)]],keepList)) )
        # if backward step procedure reached minimal model defined by keep statement
      
      |
        
        ( minCAIC <= cAICofMod & all( is.na(newSetup) ) ) 
        # if there is a new better model, which is a (g)lm
        # stop stepping and return bestModel / bestCAIC
      
      ){
      
      stepsOver <- TRUE
      
    }else if( minCAIC <= cAICofMod & all(!is.na(newSetup) & !equalToLastStep ) ){
      
      if( minCAIC == cAICofMod ) equalToLastStep <- TRUE
      
      # if there is a new better model and the new model is not a (g)lm
      # update the best model
  
      if( steps==0 | length(newSetup)==1 ) stepsOver <- TRUE else{
        
        cAICofMod <- minCAIC
        object <- bestModel
        improvementInBoth <- TRUE # set TRUE as performance improved (only relevant for direction=="both")
        if(dirWasBoth)  direction <- ifelse( direction=="forward", "backward", "forward" )
        
      }
      
    }else if( minCAIC > cAICofMod & ( steps==0 | length(newSetup)==1 ) & !dirWasBoth ){
        
      # if there is no better model, but all the required steps were done or
      # there is no more combination of random effects to check or the 
      # "both"-stepping was not successful in the previous turn, stop
      # stepping and return the current model or previous model
      
      stepsOver <- TRUE
      minCAIC <- cAICofMod
      bestModel <- object
        
    }else if( minCAIC >= cAICofMod & dirWasBoth & improvementInBoth ){
      
      # if there is no new better model, but direction was "both" and 
      # the step before the last step was a successfully forward / backward step
          
      direction <- ifelse( direction=="forward", "backward", "forward" )
      improvementInBoth <- FALSE 
      # set to FALSE to prevent unneccessary steps if the current model is the best model
      
    }else{
      
      # in case when the procedure did all steps / no more random effects are available
      # but the last step did not yield better performance or the last step had an equal cAIC
      
      stepsOver <- TRUE
      bestModel <- object
      minCAIC <- cAICofMod
      
    }
  
  } # while end
  
  ###############################################################################
  ############################  return result ###################################  
  ###############################################################################

  if(minCAIC==Inf){
    
    cat("\nNo best model found.")
    
  }else{
    
    cat("\nBest model: ", makePrint(bestModel),
        ", cAIC:",minCAIC,"\n_____________________________________________\n")
    
  }
  
  
   
  if(returnResult){
    return(list(finalModel=bestModel,
                bestCAIC=minCAIC)
    )
  }else{
    return(invisible(NULL))
  }
  
}
