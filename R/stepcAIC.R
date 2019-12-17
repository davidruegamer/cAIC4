#' Function to stepwise select the (generalized) linear mixed model
#' fitted via (g)lmer() or (generalized) additive (mixed) model
#' fitted via gamm4() with the smallest cAIC.
#' 
#' 
#' The step function searches the space of possible models in a greedy manner,
#' where the direction of the search is specified by the argument
#' direction. If direction = "forward" / = "backward", 
#' the function adds / exludes random effects until the cAIC can't be improved further.
#' In the case of forward-selection, either a new grouping structure, new
#' slopes for the random effects or new covariates modeled nonparameterically 
#' must be supplied to the function call.
#' If direction = "both", the greedy search is alternating between forward
#' and backward steps, where the direction is changed after each step
#'
#'@param object object returned by \code{[lme4]{lmer}}, \code{[lme4]{glmer}} or 
#'\code{[gamm4]{gamm4}} 
#'@param numberOfSavedModels integer defining how many additional models to be saved
#'during the step procedure. If \code{1} (DEFAULT), only the best model is returned. 
#'Any number \code{k} greater \code{1} will return the \code{k} best models. 
#'If \code{0}, all models will be returned (not recommended for larger applications).
#'@param groupCandidates character vector containing names of possible grouping variables for 
#'new random effects. Group nesting must be specified manually, i.e. by 
#'listing up the string of the groups in the manner of lme4. For example 
#'\code{groupCandidates = c("a", "b", "a/b")}.   
#'@param slopeCandidates character vector containing names of possible new random effects
#'@param fixEfCandidates character vector containing names of possible (non-)linear fixed effects 
#'in the GAMM; NULL for the (g)lmer-use case 
#'@param direction character vector indicating the direction ("both","backward","forward")
#'@param numberOfPermissibleSlopes how much slopes are permissible for one grouping variable
#'@param trace logical; should information be printed during the execution of stepcAIC?
#'@param steps maximum number of steps to be considered
#'@param keep list($fixed,$random) of formulae; which splines / fixed (fixed) or 
#'random effects (random) to be kept during selection; specified terms must be 
#'included in the original model 
#'@param numCores the number of cores to be used in calculations; 
#'parallelization is done by using \code{parallel::mclapply}
#'@param data data.frame supplying the data used in \code{object}. \code{data} must also include 
#'variables, which are considered for forward updates.
#'@param returnResult logical; whether to return the result (best model and corresponding cAIC)
#'@param calcNonOptimMod logical; if FALSE, models which failed to converge are not considered 
#'for cAIC calculation
#'@param bsType type of splines to be used in forward gamm4 steps
#'@param allowUseAcross allow slopes to be used in other grouping variables
#'@param allowCorrelationSel logical; FALSE does not allow correlations of random effects 
#'to be (de-)selected (default)
#'@param allowNoIntercept logical; FALSE does not allow random effects without random intercept
#'@param digits number of digits used in printing the results
#'@param printValues what values of \code{c("cll", "df", "caic", "refit")} 
#'to print in the table of comparisons
#'@param ... further options for cAIC call
#'@section Details: 
#' 
#' Note that the method can not handle mixed models with uncorrelated random effects and does NOT
#' reduce models to such, i.e., the model with \code{(1 + s | g)} is either reduced to
#' \code{(1 | g)} or \code{(0 + s | g)} but not to \code{(1 + s || g)}.
#' @return if \code{returnResult} is \code{TRUE}, a list with the best model \code{finalModel},
#' \code{additionalModels} if \code{numberOfSavedModels} was specified and
#' the corresponding cAIC \code{bestCAIC} is returned. 
#' 
#' Note that if \code{trace} is set to \code{FALSE} and \code{returnResult}
#' is also \code{FALSE}, the function call may not be meaningful
#' @author David Ruegamer
#' @export
#' @import parallel
#' @importFrom stats as.formula dbinom dnorm dpois family
#' formula glm lm model.frame model.matrix
#' predict reformulate simulate terms
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
                     numberOfSavedModels = 1,
                     groupCandidates = NULL,
                     slopeCandidates = NULL,
                     fixEfCandidates = NULL,
                     numberOfPermissibleSlopes = 2,
                     allowUseAcross = FALSE,
                     allowCorrelationSel = FALSE,
                     allowNoIntercept = FALSE,
                     direction = "backward",
                     trace = FALSE,
                     steps = 50, 
                     keep = NULL,
                     numCores = 1,
                     data = NULL,
                     returnResult = TRUE,
                     calcNonOptimMod = TRUE,
                     bsType = "tp",
                     digits = 2,
                     printValues = "caic",
                     ...)
{
  
  #######################################################################
  ########################## pre-processing #############################
  #######################################################################
  
  
  if(!is.null(data)){
    data <- get(deparse(substitute(data)), envir = parent.frame())
    if(inherits(object, c("lmerMod", "glmerMod")))
      attr(data, "orgname") <- as.character(object@call[["data"]]) else
        attr(data, "orgname") <- as.character(object$call[["data"]])
  }else if(inherits(object, c("lmerMod", "glmerMod"))){
    data <- get(deparse(object@call[["data"]]), envir = parent.frame())
    attr(data, "orgname") <- as.character(object@call[["data"]])
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
    
    ### check for dot in formula
    
    if(grepl("\\s{1}\\.{1}\\s{1}", as.character(object$mer@call)[2]))
    {
      
      stop("Abbrevation of variable names via dot in formula is not supported.")
      
    }
    
  }else{ # not gamm4, but potentially a lm / glm object
    
    if( !is.null(groupCandidates) ) stopifnot( all ( groupCandidates %in% possible_predictors ) | 
                                                 ( unlist(strsplit(groupCandidates, ":")) %in% 
                                                     possible_predictors ) )
    if( !is.null(slopeCandidates) ) stopifnot( slopeCandidates %in% possible_predictors )
    
    ### check for dot in formula
    
    
    if(inherits(object, "merMod")){
      
      if(grepl("\\s{1}\\.{1}\\s{1}", as.character(object@call)[2])){
        
        
        fullform <- terms(formula(object), data=object@frame)
        fullform <- as.formula(Reduce(paste, deparse(fullform)))
        object <- update(object, formula = fullform)
        
        
      }
      
    }else if(any(class(object)%in%c("lm","glm"))){
      
      # formula(object) should already give the desired result
      
    }else{
      
      stop("Model class not supported.")
      
    }
    
  }
  
  if(!returnResult & numberOfSavedModels != 1)
    warning("No result will be returned if returnResult==FALSE.")
  
  # define everything needed to save further models
  if(numberOfSavedModels==1) additionalModels <- NULL else{
    additionalModels <- list()
    additionalCaics <- c()
  }
  if(numberOfSavedModels==0) numberOfSavedModels <- Inf
  
  if(numberOfPermissibleSlopes < 1) 
    stop("numberOfPermissibleSlopes must be greater or equal to 1")
  # redefine numberOfPermissibleSlopes as intercepts will also count as slopes
  numberOfPermissibleSlopes <- numberOfPermissibleSlopes + 1
  
  #######################################################################
  ##########################   entry step   #############################
  #######################################################################
  
  # -> get cAIC of input model
  
  if(inherits(object, c("lmerMod", "glmerMod")) | "mer"%in%names(object)){
    
    timeBefore <- Sys.time()
    cAICofMod <- tryCatch(cAIC(object,...), error = function(e){
      
      cat("\n\nThe cAIC of the initial model can not be calculated. Continue Anyway?")
      readline("If so, type 'y': ")
      
    })
    if(!is.numeric(cAICofMod$caic) && cAICofMod=="y"){ 
      cAICofMod <- Inf 
    }else if(!is.numeric(cAICofMod$caic) && cAICofMod!="y") return(NULL)
    
    refit <- cAICofMod$new
    # if(refit==1 & inherits(object, c("lmerMod", "glmerMod")))
    #   object <- cAICofMod$reducedModel
    cAICofMod <- cAICofMod$caic
    
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
  
  if(!( 
    
    direction=="backward" | 
    
    ( direction %in% c("forward","both") & 
      ( !is.null(groupCandidates) | !is.null(slopeCandidates) | !is.null(fixEfCandidates) ) 
    ) | 
    
    ( direction %in% c("forward","both") & 
      is.null(groupCandidates) & is.null(slopeCandidates) & is.null(fixEfCandidates) &
      ( allowUseAcross | existsNonS ) ) 
  ))
  stop("Can not make forward steps without knowledge of additional random effect covariates.")
  
  if( direction=="backward" & !( is.null(groupCandidates) & is.null(slopeCandidates) & 
                                 is.null(fixEfCandidates) )
  ) 
    warning("Ignoring variables in group-/slopeCandidates or fixEfCandidates for backward steps.")
  
  
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
    
    newSetup <- if(direction=="forward"){
      
      makeForward(comps=comps,
                  slopeCandidates=slopeCandidates,
                  groupCandidates=groupCandidates,
                  fixEfCandidates=fixEfCandidates,
                  nrOfCombs=numberOfPermissibleSlopes,
                  allowUseAcross=allowUseAcross,
                  allowCorrelationSel=allowCorrelationSel,
                  bsType=bsType,
                  keep=keep)
    }else{
      
      makeBackward(comps=comps,
                   keep=keep,
                   allowCorrelationSel=allowCorrelationSel,
                   allowNoIntercept=allowNoIntercept)
      
    }
    
    if(all(sapply(newSetup, is.null)) & direction=="forward")
    {
      
      if(trace){
        
        cat("\nBest model: ", makePrint(object), "\ncAIC:", 
            cAICofMod, "\n_____________________________________________\n")
        # cat("\nModel can not be further extended.")
        
        if(refit==1) cat("\nBest model should be refitted due to zero variance components.\n")
        
      }
      
      return(list(finalModel=object,
                  additionalModels=NULL,
                  bestCAIC=cAICofMod)
      )
      
    }
    
    ########################### printing ##############################
    
    if(trace) {
      
      cat("\nStep ",stepsInit-steps+1," (",direction,"):  cAIC=", 
          format(round(cAICofMod, 4)), "\n", 
          "Best model so far:\n", makePrint(object), "\n", sep = "")
      utils::flush.console()
      
    }
    
    ###################################################################
    
    steps = steps - 1
    
    if(trace) cat("New Candidates:\n\n")
    
    newSetup <- mergeChanges(initialParts=comps, listParts=newSetup)
    
    ### ( print ) ###
    
    if(trace & !is.null(newSetup)) cat("Calculating cAIC for", 
                                       length(newSetup),
                                       "model(s) ...\n")
    
    #############
    
    ### calculate all other models and cAICs
    
    tempRes <- if(!is.null(newSetup)){
      
      calculateAllCAICs(newSetup=newSetup,
                        modelInit=object,
                        numCores=numCores,
                        data=data,
                        calcNonOptimMod=calcNonOptimMod,
                        nrmods=numberOfSavedModels,
                        ...)
      
    }
    
    ##############
    
    if(is.list(tempRes) & !is.null(tempRes$message)){ # gamm4 with error
      
      warning(paste0("There are zero variance components.\n", tempRes$message))
      
      if(returnResult){
        return(list(finalModel=object,
                    additionalModels=additionalModels,
                    bestCAIC=cAICofMod)
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
      print(aicTab[with(aicTab,order(-caic)), c("models",printValues)], 
            row.names = FALSE, digits = digits)
      cat("\n_____________________________________________\n")
      cat("_____________________________________________\n")
      utils::flush.console()
    }
    
    caicsres <- attr(tempRes$bestMod, "caic")
    bestModel <- tempRes$bestMod[[which.min(caicsres)]]
    
    if(numberOfSavedModels > 1 & length(tempRes$bestMod) > 0){ 
      
      additionalModels <- c(additionalModels, tempRes$bestMod)
      
      # check for duplicates among models
      duplicates <- duplicatedMers(additionalModels)
      
      # remove duplicates
      additionalModels <- additionalModels[!duplicates]
      additionalCaics <- c(additionalCaics, caicsres)[!duplicates]
      
      bestCaics <- order(additionalCaics, decreasing = FALSE)[
        1:min(numberOfSavedModels, length(additionalCaics))
        ]
      
      additionalModels <- additionalModels[bestCaics]
      additionalCaics <- additionalCaics[bestCaics]
      
    }
    indexMinCAIC <- which.min(aicTab$caic)
    minCAIC <- ifelse(length(indexMinCAIC)==0, Inf, aicTab$caic[indexMinCAIC]) 
    if(minCAIC < cAICofMod) refit <- tempRes$aicTab[indexMinCAIC,"refit"]
    
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
      
      ( minCAIC <= cAICofMod & !dirWasBoth & direction=="backward" & 
        any(class(bestModel)%in%c("glm","lm")) ) 
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
        improvementInBoth <- TRUE # set TRUE as performance improved 
        # (only relevant for direction=="both")
        if(dirWasBoth)  direction <- ifelse( direction=="forward", "backward", "forward" )
        
      }
      
    }else if( minCAIC <= cAICofMod & equalToLastStep & improvementInBoth ){
      
      # there is another best model
      cAICofMod <- minCAIC
      object <- bestModel
      improvementInBoth <- FALSE
      if(dirWasBoth)  direction <- ifelse( direction=="forward", "backward", "forward" )
      
      
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
      if(refit==0) bestModel <- object
      minCAIC <- cAICofMod
      
    }
    
  } # while end
  
  ###############################################################################
  ############################  return result ###################################  
  ###############################################################################
  
  if(minCAIC==Inf){
    
    if(trace) cat("\nNo best model found.")
    
  }else{
    
    if(trace) cat("\nBest model:\n", makePrint(bestModel),",\n",
                  "cAIC:", minCAIC, "\n_____________________________________________\n")
    
    #if(refit==1) cat("\nBest model should be refitted due to zero variance components.\n")
    
  }
  
  
  
  if(returnResult){
    if(!is.null(additionalModels)){ 
      additionalModels <- additionalModels[-1]
      attr(additionalModels, "cAICs") <- additionalCaics[-1]
    }
    return(list(finalModel=bestModel,
                additionalModels=additionalModels,
                bestCAIC=minCAIC)
    )
  }else{
    return(invisible(NULL))
  }
  
}
