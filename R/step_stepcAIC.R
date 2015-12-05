stepcAIC <- function(object, 
                     groupCandidates=NULL,
                     slopeCandidates=NULL,
                     fixEf=NULL,
                     numberOfPermissibleSlopes=2,
                     allowSwop=FALSE,
                     direction = "backward",
                     trace = FALSE,
                     steps = 50, 
                     keep = NULL,
                     numbCores = 1,
                     data = NULL,
                     nestingDepth = 1,
                     returnResult=TRUE,
                     calcNonOptimMod=TRUE,
                     bsType="tp",
                     ...)
{
 
  # Function to stepwise select the best (generalized) linear mixed model
  # fitted via (g)lmer or the best (generalized) additive (mixed) model
  # fitted via gamm4.
  # The step-function searches the space of possible models in a greedy manner,
  # where the direction of the search is specified by the argument
  # direction. If direction = "forward" / = "backward", 
  # the function adds / exludes random effects until the cAIC can't be improved.
  # In the case of forward-selection, either a new grouping structure, new
  # slopes for the random effects or new s()-terms must be supplied to the function call.
  # If direction = "both", the greedy search is alternating between forward
  # and backward steps, where the direction is changed after each step
  
  
  
  #### TODOS:
  
  # - throw error if null space is obtained
  # - test gamm4 
  
  library(cAIC4)
  library(parallel)

  ########################################
  
  
  
  # Args:
  #
  #   object          fit by (g)lmer for which the stepwise procedure is to
  #                   be computed
  #   groupCandidates,  
  #   slopeCandidates character vectors containing names of possible
  #                   new random effect groups / slopes
  #   fixEf           character vector containing names of possible
  #                   (non-)linear fixed effects in the GAMM;
  #                   NULL for the (g)lme-use case 
  #
  #   direction       character vector indicating the direction
  #                   %in%c("both","backward","forward")
  #
  #   numberOfPermissibleSlopes
  #                   how much slopes are permissible for one group RE
  #
  #   trace           logical; should information ne printed during the running of 
  #                   stepcAIC?
  #
  #   steps           maximum number of steps to be considered
  #
  #   keep            list($fixed,$random) of formulae; 
  #                   which splines / fixed (fixed) or random effects (random) 
  #                   to be kept during selection must be included in the original model 
  #   data            data.frame, from which the new REs are to be taken
  #   nestingDepth    numeric; if 1, no nested effects are evaluated, else
  #                   interactions of nestingDepth are included in the procedure
  #   returnResult    logical; whether to return the result (best model and corresponding cAIC)
  #   calcNonOptimMod logical; if FALSE, models which failed to converge are not 
  #                   considered for cAIC calculation
  #   bsType          type of splines to consider in forward gamm4 steps
  #   ...             options for cAIC call
  
  
  # for use with "gamm4-objects": 
  # ------------------------------------------------------
  # groupCandidates are interpreted as covariables and fitted as splines
  # if groupCandidates does include characters such as "s(..,bs='cr')" the respective spline is
  # included in the forward stepwise procedure
  
  
  
  
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
  
  existsNonS <- FALSE
  
  ### check if gamm4-call
  
  if(is.list(object) & length(object)==2 & all(c("mer","gam") %in% names(object))){
   
    library(mgcv)
    library(gamm4)
    
    if(nestingDepth>1 | allowSwop | !is.null(slopeCandidates)){
      
      stop("Using step-function for gamm4-object:\nNeither nesting / swopping nor slope candidates are permissible!")
      
    }
    
    ig <- interpret.gam(object$gam$formula)
    existsNonS <- length(ig$smooth.spec)<length(ig$fake.names)
    
    if( !is.null(fixEf) ) stopifnot( fixEf %in% possible_predictors )
        
  }else{
    
    if( !is.null(groupCandidates) ) stopifnot( groupCandidates %in% possible_predictors )
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
    if(!is.numeric(cAICofMod) && cAICofMod=="y") cAICofMod <- Inf
    timeForCalc <- Sys.time() - timeBefore

  }else if(any(class(object)%in%c("lm","glm"))){
    
      ll <- getGLMll(object)
      bc <- attr(stats4:::logLik(object),"df")
      cAICofMod <- -2*ll + 2*bc
      
      if(direction=="backward") stop("A simple (generalized) linear model can't be reduced!")

  }else{
    
    stop("Class of object is not known")
    
  }


  # check if call is inherently consistent
  
  stopifnot( direction=="backward" | ( direction %in% c("forward","both") & 
                   ( !is.null(groupCandidates) | !is.null(slopeCandidates) | !is.null(fixEf) ) ) | 
                   ( direction %in% c("forward","both") & 
                       is.null(groupCandidates) & is.null(slopeCandidates) & is.null(fixEf) &
                       ( allowSwop | existsNonS ) ) 
  )
  
  if( direction=="backward" & !( is.null(groupCandidates) & is.null(slopeCandidates) & is.null(fixEf) )
      ) warning("I will ignoring variables in group- / slopeCandidates or fixEf for backward selection.")
  
    
  ### more to be added ...

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
  
  
  
  
  # try to improve the model as long as stepsOver==FALSE
  while(!stepsOver){

    # get all components needed for stepping procedure
    comps <- getComponents(object)

    ########################### printing ##############################
    
    if(trace) {
    
      cat("\nStep ",stepsInit-steps+1," (",direction,"):  cAIC=", format(round(cAICofMod, 4)), "\n", 
            "Best model so far: ", makePrint(object), "\n\n",
          "_____________________________________________\nNew Candidates:\n\n",
          sep = "")
      utils::flush.console()
    
    }
    
    ###################################################################
    
  
    steps = steps - 1
      
    newSetup <- if(direction=="forward"){
      
                       makeForward(comps=comps,
                                   slopeCandidates=slopeCandidates,
                                   groupCandidates=groupCandidates,
                                   fixEf=fixEf,
                                   nrOfCombs=numberOfPermissibleSlopes,
                                   allowSwop=allowSwop,
                                   intDep=nestingDepth,
                                   bsType=bsType,
                                   keep=keep,
                                   ...)
    }else{
      
      makeBackward(comps=comps,
                   keep=keep)
      
    }
    
    newSetup <- mergeChanges(initialParts=comps, listParts=newSetup)
    
    ### ( print ) ###
    
    if(trace & !is.null(newSetup)) cat("Calculating cAIC for", 
                                       length(newSetup),
                                       "possible model(s) ...")
    
    #############
    
    ### calculate all other models and cAICs
    
    tempRes <- if(!is.null(newSetup)){
      
                      calculateAllCAICs(newSetup=newSetup,
                                        # gamPos=!is.null(fixEf),
                                        # comps=comps,
                                        modelInit=object,
                                        numbCores=numbCores,
                                        data=data,
                                        calcNonOptimMod=calcNonOptimMod,
                                        ...)
                      
    }else{
      
                      calcSimple(object)
                      
    }
     
    ##############
    
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
    if(!is.null(keep$fixed)) keepList$gamPart <- interpret.gam(keep$fixed)

    ###############################################################################
    ###############################################################################
    ############################# - decision part - ###############################
    ###############################################################################
    ###############################################################################
    
  #   lenRanefATM <- sum(sapply(object@cnms,length))
    
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
  
      cAICofMod <- minCAIC
      object <- bestModel
      improvementInBoth <- TRUE # set TRUE as performance improved (only relevant for direction=="both")
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
      # but the last step got better performance or the last step had an equal cAIC
      
      stepsOver <- TRUE
      bestModel <- object
      minCAIC <- cAICofMod
      
    }
  
  } # while end
  
  ###############################################################################
  ############################  return result ###################################  
  ###############################################################################

  cat("\nBest model: ", makePrint(bestModel),
      ", cAIC:",minCAIC,"\n")
   
  if(returnResult){
    return(list(finalModel=bestModel,
                bestCAIC=minCAIC)
    )
  }else{
    return(invisible(NULL))
  }
  
}