#######################################################################################
### allCombn functions
### purpose: 
allCombn <- function(x,simplify=F)
{
  
  m <- length(x)-1
  unlist(lapply(X=1:m,function(i)combn(x,i,simplify=simplify)),recursive=F)
  
}

allCombn2 <- function(x,range,simplify=F)
{
  
  unlist(lapply(X=2:range,function(i)combn(x,i,simplify=simplify)),recursive=F)
  
}


#######################################################################################
### backwardGam function
### purpose: reduce complexity of gamm4 model

backwardGam <- function(intGam, keep)
{
  
  # intGam    result of interpret.gam - call / $gamPart of getComponents-result
  
  vars <- intGam$fake.names
  sTerm <- vars%in%sapply(intGam$smooth.spec,function(x)x$term)
  nonS <- maybeNonS <- vars[!sTerm]
  addNonS <- addSlabs <- NULL
  haveS <- replaceHaveS <- vars[sTerm]  # should be at least of length = 1 , 
  # else a (g)lmer should be fitted
  sLabs <- maybeSlabs <- makeS(intGam)
  
  # handle keep
  
  if(!is.null(keep)){
    
    keep <- mgcv::interpret.gam(keep)
    keepVars <- keep$fake.names
    keepSterm <- keepVars%in%sapply(keep$smooth.spec,function(x)x$term)
    keepNonS <- keepVars[!keepSterm]
    keepHaveS <- keepVars[keepSterm]
    
    # prevent keepNonS variables to be excluded
    
    keepTermNonS <- nonS%in%keepNonS
    maybeNonS <- nonS[!keepTermNonS]
    addNonS <- nonS[keepTermNonS]
    
    # prevent keepHaveS variables to be excluded
    
    keepTermHaveS <- haveS%in%keepHaveS
    maybeSlabs <- sLabs[!keepTermHaveS]
    addSlabs <- sLabs[keepTermHaveS]
    replaceHaveS <- haveS[!keepTermHaveS]
    
  }
  
  
  returnList <- NULL
  
  # create list for dropping linear term
  
  if(length(maybeNonS)>0)
    returnList <- lapply(combn(maybeNonS,length(maybeNonS)-1,simplify=F),
                         function(x)append(x,c(sLabs,addNonS)))
  
  if(length(maybeSlabs)>0){
    
    for(i in 1:length(maybeSlabs)){
      
      temp <- maybeSlabs
      temp[i] <- replaceHaveS[i]
      returnList <- append(returnList,list(c(temp,addSlabs,nonS)))
      
    }
    
  }
  
  returnList
  
}


#######################################################################################
### backwardStep function
### purpose: reduce complexity of model


backwardStep <- function(cnms, keep, allowCorrelationSel, allowNoIntercept)
{
  
  if( (sum(sapply(cnms,length))==1# & !isGam
  ) ){
    if(!is.null(keep)){
      retList <- vector("list",1)
      retList[[1]] <- cnms
      return(retList)
    }else{
      return(NA)
    }
  } 
  
  cnms2 <- rep(cnms,sapply(cnms,length))
  listCnms <- split(cnms2,names(cnms2))
  
  if(!is.null(keep)){
    
    keep <- interpret.random(keep)
    
    for(i in 1:length(listCnms)){
      
      if(names(listCnms)[i]%in%names(keep)){
        
        temp <- listCnms[[names(listCnms)[i]]] 
        indRem <- unlist(temp)!=unlist(keep)
        listCnms[[names(listCnms)[i]]] <- temp[indRem]
        
      }
    }
  } 
  
  newCnms <- lapply(listCnms,function(d){
    
    if(length(d)<=1){
      
      #       if(names(d)%in%names(keep)){
      #         keep[names(d)]
      #       }else{
      list(NA)
      #       }
      
    }else{
      
      for(i in 1:length(d)){
        
        d[[i]] <- d[[i]][-i]  
        
      }
      #       if(names(d)%in%names(keep)){
      #         append(d,keep[names(d)])
      #       }else{
      d
      #       }
      
    }
    
  })
  
  if(!is.null(keep)){
    
    for(n in names(keep)){
      
      if(is.na(newCnms[[n]])){
        newCnms[[n]] <- keep[n]
      }else{
        newCnms[[n]] <- append(newCnms[[n]],keep[n])
      }
    }
    
  }
  
  newCnms <- unlist(newCnms,recursive=FALSE)
  
  # problematic: variables with dots in name...
  names(newCnms) <- gsub("\\..*","",names(newCnms)) 
  
  listOfAllCombs <- vector("list",length(newCnms))
  
  for(i in 1:length(newCnms)){
    
    accessREi <- names(newCnms)[i]
    listOfAllCombs[[i]] <- append(cnms[names(cnms)!=accessREi],newCnms[i])
    
  }
  
  notempty <- sapply(listOfAllCombs, function(x) length(x[[1]])>0)
  listOfAllCombs <- listOfAllCombs[notempty]
  newCnms <- newCnms[notempty]
  listOfAllCombs <- suppressWarnings(split(unlist(listOfAllCombs,recursive=FALSE),
                                           rep(1:length(newCnms),each=length(cnms))))
  listOfAllCombs <- lapply(listOfAllCombs,checkREs)
  
  listOfAllCombs <- listOfAllCombs[sapply(listOfAllCombs,function(r)!is.null(r))]
  listOfAllCombs <- lapply(listOfAllCombs,function(t)t[order(names(t))])
  listOfAllCombs <- listOfAllCombs[!(duplicated(listOfAllCombs) & duplicated(lapply(listOfAllCombs,names)))]
  
  #   listOfAllCombs <- listOfAllCombs[
  #     which(sapply(listOfAllCombs,function(l)
  #     !is.logical(all.equal(l[order(names(l))],
  #                           cnms[order(names(cnms))]))
  #   ))
  #   ]
  if(!allowCorrelationSel) listOfAllCombs <- removeUncor(listOfAllCombs)
  if(!allowNoIntercept) listOfAllCombs <- removeNoInt(listOfAllCombs)
  
  return(listOfAllCombs)
}


#######################################################################################
### calculateAllCAICs function
### purpose: computes cAIC for all given models

calculateAllCAICs <- function(newSetup,
                              # gamPos,
                              modelInit, 
                              numCores, 
                              data, 
                              calcNonOptimMod, 
                              nrmods,
                              ...)
{
  
  #   if(is.null(newSetup$sPart)) isGam <- FALSE  
  
  formulaList <- lapply(newSetup,function(x)makeFormula(x,modelInit))
  
  ### create all possible models ###
  
  listOfModels <- mclapply(formulaList, function(f)
    makeUpdate(modelInit=modelInit, setup=f, data=data),
    mc.cores=numCores)
  
  
  #######################################################################
  ################### calculate alle the cAICs ##########################
  #######################################################################
  
  listOfCAICs <- mclapply(listOfModels,function(m){
    
    # if(any(class(m)%in%c("glm","lm"))){
    #   
    #   ll <- getGLMll(m)
    #   bc <- attr(logLik(m),"df")
    #   caic <- -2*ll + 2*bc
    #   c(ll,bc,caic)
    #   
    # }else{
    
    if(length(class(m))==1 && class(m)=="list"){ # m is a gamm4 object
      
      tryCatch(cAIC(m,...)[c("loglikelihood","df","caic", "new")], 
               error = function(e){ 
                 
                 ret <- c(NA,NA,NA,NA)
                 attr(ret, "message") <- e
                 return(ret)
                 
               })
      
    }else{
      
      if(!calcNonOptimMod){
        
        errCode <- m@optinfo$conv$lme4$code
        if(!is.null(errCode)) return(c(NA,NA,NA))
        
      }
      
      tryCatch(cAIC(m,...)[c("loglikelihood","df","caic","new")], 
               error = function(e){ 
                 
                 ret <- c(NA,NA,NA)
                 attr(ret, "message") <- e
                 return(ret)
                 
               })
      
    }
    # }
  }, mc.cores=numCores)
  
  if(all(sapply(listOfCAICs, function(x) is.na(sum(unlist(x))))))
  {
    listOfCAICs$message <- attr(listOfCAICs[[1]],"message")
    return(listOfCAICs)
  }
  # if(all(sapply(listOfCAICs, is.list))
  #    all(sapply(listOfCAICs, function(x) !is.null(x$message))))
  #   return(listOfCAICs) else 
  listOfCAICs <- lapply(listOfCAICs,unlist)
  
  #######################################################################
  ################ list all the cAICs and models ########################
  #######################################################################
  
  
  aicTab <- as.data.frame(as.matrix(do.call("rbind",listOfCAICs)))
  colnames(aicTab) <- c("cll","df","caic","refit")
  aicTab$models <- sapply(formulaList, makePrint, initial=FALSE)
  
  aicTab <- as.data.frame(aicTab[,c("models","cll","df","caic","refit")])
  
  minInd <- order(aicTab$caic, decreasing = FALSE)
  bestMod <- NA
  if(length(minInd)!=0){ 
    bestMod <- listOfModels[minInd[1:(min(nrmods,length(minInd)))]]
    attr(bestMod, "caic") <- sort(aicTab$caic, decreasing = FALSE)[
      1:(min(nrmods,length(minInd)))]
  }
  
  return(list(aicTab=aicTab,
              bestMod=bestMod)
  )
  
}


#######################################################################################
### checkHierarchicalOrder function
### purpose: checks the correct hierarchical order

checkHierarchicalOrder <- function(listIn)
{
  
  listIn <- listIn[order(sapply(listIn,length),decreasing=T)]
  
  notAllowed <- list()
  lenMax <- length(listIn[[1]])
  lenMin <- length(listIn[[length(listIn)]])
  
  i = 1
  
  while(i < length(listIn)){
    
    lenI <- length(listIn[[i]])
    
    if( lenMax>1 & lenI>lenMin ){ 
      
      notAllowed <- allCombn(listIn[[i]])
      listIn <- listIn[!listIn%in%notAllowed]
      
    }
    
    i = i + 1
    
  }
  
  return(listIn)  
  
}


#######################################################################################
### checkREs  function
### purpose:  checks for NULL- or NA-REs, then for duplicates and calls the check
###           for hierarchical order


checkREs <- function(reList)
{
  
  # first check: NULL-REs / NA-REs
  
  reList <- reList[sapply(reList,function(x)!is.null(x))]
  reList <- reList[sapply(reList,function(x)any(!is.na(x)))]
  
  nam <- unique(names(reList))
  
  checkedList <- list()
  
  for(i in 1:length(nam)){
    
    namL <- reList[names(reList)==nam[i]]
    namL <- lapply(namL,sort)    
    
    # second check: duplicated REs    
    namL <- namL[!duplicated(namL)]
    
    # third check: hierarchical order
    if(length(namL)>1) namL <- checkHierarchicalOrder(namL)
    
    checkedList <- append(checkedList,namL)
    
  }
  
  
  return(checkedList)  
  
}


#######################################################################################
### nester function
### purpose:  does the nesting of random effects
# 
# 
# nester <- function(namesGroups, intDep)
# {
#   
#   countColons <- function(strings) sapply(regmatches(strings, gregexpr(":", strings)), length)
#   
#   # namesGroups <- unique(unlist(sapply(listOfRanefCombs,names)))
#   nrOfColons <- countColons(namesGroups)
#   newGroups <- namesGroups[nrOfColons<intDep-1]
#   
#   if(length(namesGroups)>1){
#     
#     combsGroups <- sapply(allCombn2(newGroups,intDep),paste0,collapse=":")
#     combsGroups <- combsGroups[countColons(combsGroups)<=intDep-1]
#     namesGroups <- append(namesGroups,combsGroups)
#     
#   }
#   
#   return(namesGroups)
#   
# }

#######################################################################################
### allNestSubs function
### purpose:  create all grouping variables from nested expression

allNestSubs <- function(x)
{
  unlist(sapply( findbars( as.formula( paste0("~ (1 | ", x, ")"))), 
                 function(y) deparse(y[[3]])
  )
  )
}

#######################################################################################
### cnmsConverter function
### purpose:  converts cnms to formula-like string

cnmsConverter <- function(cnms)
{
  
  charForm <- character(length(cnms))
  
  if(all(sapply(cnms,function(x) all(is.na(x)))) | all(sapply(cnms,is.null))) return(NULL)
  
  for(i in 1:length(cnms)){
    
    if ("(Intercept)"%in%cnms[[i]]) {
      cnms[[i]][which(cnms[[i]]=="(Intercept)")] <- "1"
    }else{
      cnms[[i]] <- append(cnms[[i]],"0")
    }
    
    
    
    charForm[i] <- paste("(", paste(cnms[[i]], 
                                    collapse = " + "), 
                         " | ", names(cnms)[i], ")", 
                         sep = "")
  }
  
  charForm
  
}




#######################################################################################
### forwardGam function
### purpose:  does the forward step for gamm4 models


forwardGam <- function(intGam, fixEfCandidates=NULL, bsType="ps", keep)
{
  
  vars <- intGam$fake.names
  sTerm <- vars%in%sapply(intGam$smooth.spec,function(x)x$term)
  nonS <- vars[!sTerm]
  haveS <- vars[sTerm] # should be at least of length 1 , else a (g)lmer should be fitted
  sLabs <- makeS(intGam)
  keepNonS <- vars[!(vars %in% fixEfCandidates) & !(vars %in% haveS)]
  
  newX <- fixEfCandidates[which(!fixEfCandidates %in% vars)]
  
  if(!is.null(keep)){
    
    keep <- mgcv::interpret.gam(keep)
    keepVars <- keep$fake.names
    keepSterm <- keepVars%in%sapply(keep$smooth.spec,function(x)x$term)
    keepNonS <- keepVars[!keepSterm]
    
  }
  
  nonS <- nonS[!nonS%in%keepNonS] # drop the keepNonS from nonS
  # to prevent s-making
  if(length(nonS)==0) nonS <- NULL
  
  returnListS <- vector("list",length=length(nonS)+length(newX)) 
  
  for(i in 1:length(returnListS)){
    
    if(i <= length(newX)){
      
      returnListS[[i]] <- c(keepNonS,sLabs,nonS,newX[i])
      
    }else{
      
      returnListS[[i]] <- c(keepNonS,sLabs,nonS[-(i-length(newX))],
                            paste0("s(",nonS[i-length(newX)],
                                   ",bs=",deparse(bsType),")"))
      
    }
    
  }
  
  returnListS
  
}


#######################################################################################
### forwardStep function
### purpose:  does the forward step

forwardStep <- function(cnms,
                        slopeCandidates,
                        groupCandidates,
                        nrOfCombs,
                        allowUseAcross,
                        allowCorrelationSel
)
{
  
  if(allowUseAcross)
    allSlopes <- unique(c(unlist(cnms), slopeCandidates), "(Intercept)") else 
      allSlopes <- c(slopeCandidates,"(Intercept)")
    
    allGroups <- unique( c(names(cnms), groupCandidates) )
    
    allSlopeCombs <- list()
    
    for(i in 1:nrOfCombs){
      
      if(i<=length(allSlopes)){
        
        allSlopeCombs <- append(allSlopeCombs,combn(allSlopes,m=i,simplify=FALSE))
        
      }
      
    }
    
    allSlopeCombs <- allSlopeCombs[sapply(allSlopeCombs,function(x)!any(duplicated(x)))]
    
    reList <- rep(allSlopeCombs,each=length(allGroups))
    names(reList) <- rep(allGroups,length(allSlopeCombs))
    
    allCombs <- lapply(X=1:length(reList),function(i)append(cnms,reList[i]))
    allCombs <- lapply(allCombs,checkREs)
    if(!allowCorrelationSel) allCombs <- allCombs[
      sapply(allCombs,function(x) 
        length(unique(names(x))) == length(x))]
    allCombs <- allCombs[!duplicated(allCombs)]
    allCombs <- allCombs[!(sapply(allCombs,function(x)all.equal(x,lapply(cnms,sort)))=="TRUE")]
    
    if(length(allCombs)==0) return(NULL)
    
    allCombs <- allCombs[sapply(allCombs,function(r)!is.null(r))]
    allCombs <- lapply(allCombs,function(t)t[order(names(t))])
    allCombs <- allCombs[!(duplicated(allCombs) & duplicated(lapply(allCombs,names)))]
    
    # also allow for correlation parameter to be selected?
    if(!allowCorrelationSel) allCombs <- removeUncor(allCombs)
    
    # only allow for REs, which are one variable larger than the current one
    allCombs <- allCombs[!sapply(allCombs, function(r){
      
      any(sapply(1:length(r), function(i){
        
        if(!names(r)[i] %in% names(cnms)){
          
          length(r[[i]]) > 1
          
        }else{
          
          length(r[[i]]) > length(cnms[[names(r)[i]]]) + 1
          
        }
        
      }))
      
    })]
    
    if(length(allCombs)==0) return(NULL)
    
    return(#list(randomPart=
      allCombs#, sPart=...)
    )
    
}

#######################################################################################
### removeUncor function
### purpose:  removes random effects with uncorrelated intercept and slope

removeUncor <- function(res)
{
  
  # keep <- sapply(res, function(re){
  #   
  #   length(re) == 1 | 
  #     (all(unlist(sapply(re, function(x) 
  #       any(grepl("(Intercept)", x, fixed=T))))))
  #   
  # })
  
  drop <- sapply(res, function(re){
    
    reL <- split(re, names(re))
    dropPerName <- sapply(reL, function(rel)
    {
      if(length(rel) > 1){ 
        ints <- sapply(rel, function(x) any(grepl("(Intercept)", x, fixed=T)))
        noints <- sapply(rel, function(x) any(!grepl("(Intercept)", x, fixed=T)))
        return(ints & noints)
      }else return(FALSE)
    })
    any(dropPerName)
    
  })
  
  # res <- res[keep]
  # check for several random intercepts with different slopes
  # drop <- sapply(res, function(re){
  #   
  #   reL <- split(re, names(re))
  #   dropPerName <- sapply(reL, function(rel)
  #   {
  #   if(length(rel) > 1){ 
  #     ints <- sapply(rel, function(x) any(grepl("(Intercept)", x, fixed=T)))
  #   }else FALSE
  #   })
  #   any(dropPerName)
  #   
  # })
  return(res[!drop])
  
}

### removeNoInt function
### purpose:  removes random effects with no random intercept

removeNoInt <- function(res)
{
  
  hasInt = function(x) grepl("(Intercept)",x,fixed=TRUE)
  
  for(i in 1:length(res)){
    
    namresi = names(res[[i]])
    
    for(j in 1:length(namresi)){
      
      resForThisGroup <- unlist(res[[i]][namresi[j]])
      # remove RE without intercept
      if(!hasInt(resForThisGroup)) res[[i]] <- res[[i]][-j]
      
    }
    
  }
  
  res <- res[sapply(res, length)>0]
  
  return(res)
  
}


#######################################################################################
### getComponents function
### purpose:  extracts important components of [g]lmerMod and 'gamm4' objects

getComponents <- function(object)
{
  
  isGam <- is.list(object) & length(object)==2 & 
    all(c("mer","gam") %in% names(object))
  gamPart <- NULL
  random <- NULL
  
  if(isGam){
    
    nrOfSmooths <- length(object$gam$smooth)
    # cutp <- length(object$mer@cnms)-nrOfSmooths
    random <- object$mer@cnms[!object$mer@cnms%in%sapply(object$gam$smooth,function(x)x$label)] # ,cutp)
    if(length(random)==0) random=NULL
    
    gamPart <- mgcv::interpret.gam(object$gam$formula)
    
  }else if(inherits(object, c("lmerMod", "glmerMod"))){
    
    random <- object@cnms
    
  }#else if(any(class(object)%in%c("lm","glm"))){
  
  #}  
  
  return(list(random=random,
              gamPart=gamPart
  )
  )
  
}



#######################################################################################
### getGLMll function
### purpose:  



# getGLMll <- function(object)
# {
#   
#   y <- object$y
#   if(is.null(y)) y <- eval(object$call$data, environment(formula(object)))[all.vars(formula(object))[1]][[1]]
#   if(is.null(y)) stop("Please specify the data argument in the initial model call!")
#   
#   mu <- predict(object,type="response")
#   sigma <- ifelse("glm"%in%class(object),sqrt(summary(object)$dispersion),summary(object)$sigma)  
#   
#   switch(family(object)$family, binomial = {
#     cll <- sum(dbinom(x = y, size = length(unique(y)), prob = mu, log = TRUE))
#   }, poisson = {
#     cll <- sum(dpois(x = y, lambda = mu, log = TRUE))
#   }, gaussian = {
#     cll <- sum(dnorm(x = y, mean = mu, sd = sigma, log = TRUE))
#   }, {
#     cat("For this family no bias correction is currently available \n")
#     cll <- NA
#   })
#   return(cll)
#   
#   
# }




#######################################################################################
### keeps functions
### purpose:  


# sepKeeps <- function(comps, keep = keep)
# {
#   
#   keepRE <- keep$random
#   keepS <- keep$fixed
#   
#   if(!is.null(keepS)) keepS <- mgcv::interpret.gam(keepS)
#   if(!is.null(keepRE)) keepRE <- interpret.random(keepRE)
#   
#   randomNK <- excludeRE(comps, keepRE)
#   gamPartNK <- excludeS(comps, keepS)   
#   
#   return(list(random = random,
#               gamPart = gamPart))
#    
# }
# 
# addKeeps <- function(keep, newComps)
# {
#   
#   random <- lapply(newComps$random, function(x) append(x, keep$random))
#   gamPart <- lapply(newComps$gamPart, function(x) append(x, keep$gamPart))
#   
#   return(list(random=random,
#               gamPart=gamPart))
#   
# }


#######################################################################################
### interpret_random function
### purpose:  

interpret.random <- function(frla)
{
  
  bars <- findbars(frla[[length(frla)]])
  names(bars) <-  unlist(lapply(bars, function(x) deparse(x[[3]])))
  
  lapply(bars, function(b){
    
    hasInt <- attr(terms(as.formula(paste0("~",deparse(subbars(b))))),"intercept")==1
    
    v <- ifelse(hasInt, "(Intercept)", "0")
    v <- append(v,all.vars(as.formula(paste0("~",deparse(b[[2]])))))
    
    
    return(v)
    
  })
  
}


#######################################################################################
### makeBackward function
### purpose:  

makeBackward <- function(comps, keep, allowCorrelationSel, allowNoIntercept)
{
  
  # comps   list created by getComponents
  
  returnListRE <- returnListS <- NULL
  
  returnListRE <- if(!is.null(comps$random)) 
    backwardStep(comps$random, keep=keep$random, 
                 allowCorrelationSel=allowCorrelationSel,
                 allowNoIntercept=allowNoIntercept)
  
  returnListS <- if(!is.null(comps$gamPart) && comps$gamPart$fake.formula[[3]]!=1) 
    backwardGam(comps$gamPart, keep=keep$fixed)
  
  return(list(gamPart=returnListS, random=returnListRE))
  
}



#######################################################################################
### makeFormula function
### purpose:  


makeFormula <- function(setup, modelInit)
{
  
  # setup         list ($random,$gamPart) created by makeBackward / makeForward
  # modelInit     initial model
  
  # get config
  
  isGam <- !is.null(setup$gamPart) & length(setup$gamPart)>0
  wasGam <- is.list(modelInit) & length(modelInit)==2 & all(c("mer","gam") %in% names(modelInit))
  
  random <- gamPart <- reFormula <- rhs <-  NULL
  
  ### create random part
  
  if(!is.null(setup$random) && all(!is.na(setup$random))){
    
    charForm <- cnmsConverter(setup$random)
    
    reFormula <- paste(charForm, collapse = " + ")
    
  }
  
  ### create gamPart / lhs / rhs
  
  if(isGam){
    
    rhs <- paste(setup$gamPart, collapse = " + ")    
    
  }else{
    
    if(wasGam){
      
      rhs <- "1"
      
    }else{ # (g)lmer / (g)lm
      
      if(nobars(formula(modelInit)) == formula(modelInit)[[2]]){
        
        nobarsF <- NULL
        
      }else{
        
        if(any(class(modelInit)%in%c("lm","glm"))){
          
          nobarsF <- labels(terms(formula(modelInit)))
          
        }else{
          
          nobarsF <- attr(terms.formula(nobars(formula(modelInit)),
                                        data = modelInit@frame), "term.labels")
          
        }
        
      }
      
      rhs <- c(nobarsF, reFormula)
      
    }
    
  }
  
  # if there are no covariates, set rhs to "1"
  
  if(is.null(rhs) | length(rhs)==0) rhs <- "1"
  
  # extract response
  
  lhs <- ifelse(wasGam, formula(modelInit$gam)[[2]], formula(modelInit)[[2]])
  
  # specify the parts random and gamPart
  
  if(isGam | wasGam){
    
    random <- reFormula
    gamPart <- reformulate(rhs, lhs)
    
  }else{
    
    random <- reformulate(rhs, lhs)
    
  }
  
  return(list(random=random,
              gamPart=gamPart)
  )
  
  
}



#######################################################################################
### makeForward function
### purpose:  



makeForward <- function(comps, 
                        slopeCandidates,
                        groupCandidates,
                        nrOfCombs,
                        allowUseAcross,
                        fixEfCandidates, 
                        bsType,
                        keep,
                        allowCorrelationSel)
{
  
  returnListRE <- returnListS <- NULL
  # ellipsis <- as.list(substitute(list(...)))
  
  if(is.null(comps$random) & is.null(comps$gamPart)){ 
    
    gr <- rep("(Intercept)",length(groupCandidates))
    names(gr) <- groupCandidates
    
    returnListRE <- if(!is.null(groupCandidates)) lapply(split(gr, 1:length(gr)),as.list)
    
    returnListS <- if(!is.null(fixEfCandidates)) lapply(as.list(fixEfCandidates),as.list)
    
  }else{
    
    returnListS <- if(!is.null(comps$gamPart) | !is.null(fixEfCandidates)) 
      forwardGam(comps$gamPart, fixEfCandidates=fixEfCandidates, bsType=bsType, keep=keep$fixed)
    
    returnListRE <- if(!is.null(slopeCandidates) | !is.null(groupCandidates) | 
                       length(comps$random)>1 | allowUseAcross) 
      forwardStep(cnms=comps$random, slopeCandidates, groupCandidates, 
                  nrOfCombs, allowUseAcross, allowCorrelationSel)
    
  }
  
  
  
  return(list(gamPart=returnListS, random=returnListRE))
  
}



#######################################################################################
### makePrint function
### purpose:  



makePrint <- function(comps, initial=TRUE)
{
  
  if(initial){
    
    isLMER <- FALSE
    
    if(inherits(comps, c("lmerMod", "glmerMod"))){
      
      f <- nobars(formula(comps))
      f <- if(is.name(f)){
        1 
      }else{
        f[[length(f)]]
      }
      
      isLMER <- TRUE
      
    }
    
    if("lm" %in% class(comps)){
      
      pr <- paste0("~ ", #attr(comps$terms, "intercept"), " + ",
                   paste(attr(comps$terms, "term.labels"),
                         collapse = " + "))
      
    }else{
      
      comps <- mergeChanges(getComponents(comps), NULL)
      
      if(isLMER) comps$gamPart <- all.vars(f)    
      
      gp <- c(
        comps$gamPart,
        cnmsConverter(comps$random)
      )
      
      if(is.null(gp) | length(gp)==0) gp <- "1"
      
      pr <- paste0("~ ",
                   paste(gp,
                         collapse = " + ")
      )
      
    }    
  }else{
    
    gp <- NULL
    gp <- if(!is.null(comps$gamPart)) as.character(Reduce(paste,deparse(comps$gamPart)))
    parts <- c(gp,comps$random)
    # print(parts)
    pr <- paste(parts,collapse=" + ")    
    pr <- as.character(Reduce(paste,deparse(as.formula(pr)[-2]))) # too complicated
    
  }
  
  return(pr)
  
}


#######################################################################################
### makeS function
### purpose:  




makeS <- function(intGam)
{
  
  sapply(intGam$smooth.spec, 
         function(x) paste0("s(",
                            x$term,
                            ",bs=",
                            deparse(paste0(substring(attr(x,"class"), 1, 2))),
                            ")")
  )
  
}


#######################################################################################
### makeUpdate function
### purpose:  

makeUpdate <- function(modelInit, 
                       setup, 
                       data)
{
  
  willBeGam <- !is.null(setup$gamPart) & 
    grepl("s\\(",Reduce(paste,deparse(setup$gamPart))) # probably not the best way to check...
  
  hasBars <- ifelse(!is.character(setup$random),
                    !is.null(findbars(setup$random)),
                    !is.null(findbars(as.formula(paste0("~",setup$random))))
  )
  
  isGlm <- any(class(modelInit)%in%c("glm","lm"))
  
  isGam <- is.list(modelInit) & length(modelInit)==2 & 
    all(c("mer","gam") %in% names(modelInit))
  
  # make a decision which method should be used for fitting
  
  if(!willBeGam & isGlm & hasBars){
    
    fm <- ifelse(isGam,
                 family(modelInit$mer)$family,
                 family(modelInit)$family)
    
    mod <- if(fm=="gaussian"){
      
      lmer(setup$random, data = data)
      
    }else{
      
      glmer(setup$random, data = data, 
            family = fm)
      
    }
    
  }else if(!willBeGam & !isGlm & hasBars & !isGam){
    
    mod <- update(modelInit,
                  formula = setup$random)    
    
  }else if(!willBeGam & !hasBars & !isGam){
    
    fm <- ifelse(isGam,
                 family(modelInit$mer)$family,
                 family(modelInit)$family)
    
    mod <- eval(parse(text=paste0("glm(",paste(format(setup$random), 
                                               collapse=""),
                                  ", family = ", fm, ", data = ",
                                  attr(data, "orgname"),")")), 
                envir = environment(modelInit))
    
  }else if(!willBeGam & !hasBars & isGam){
    
    fm <- ifelse(isGam,
                 family(modelInit$mer)$family,
                 family(modelInit)$family)
    
    mod <- eval(parse(text=paste0("glm(",paste(format(setup$gamPart), 
                                               collapse=""),
                                  ", family = ", fm, ", data = ",
                                  attr(data, "orgname"),")")))   
    
    
  }else{ # willBeGam
    
    r <- if(!is.null(setup$random)){
      as.formula(paste("~",setup$random))
    }else{
      NULL
    }
    
    mod <- gamm4::gamm4(setup$gamPart, 
                        data = data, 
                        family = family(modelInit$mer), 
                        random = r)
    
  }
  
  return(mod)
  
}

#######################################################################################
### mergeChanges function
### purpose:  

mergeChanges <- function(initialParts, listParts)
{
  
  ### initial part
  
  orgS <- orgRE <- NULL
  
  if(!is.null(initialParts$gamPart)){
    
    vars <- initialParts$gamPart$fake.names
    sTerm <- vars%in%sapply(initialParts$gamPart$smooth.spec,function(x)x$term)
    nonS <- vars[!sTerm]
    haveS <- vars[sTerm] # should be at least of lenght = 1 , else a (g)lmer should be fitted
    sLabs <- makeS(initialParts$gamPart)
    
    orgS <- append(sLabs,nonS)
    
  }
  
  if(!is.null(initialParts$random)) orgRE <- initialParts$random
  
  if(is.null(listParts$gamPart) & is.null(listParts$random)) 
    return(list(random=orgRE,gamPart=orgS))  
  
  newRE <- lapply(listParts$random,function(r)list(random=r,gamPart=orgS))
  newS <- lapply(listParts$gamPart,function(s)list(random=orgRE,gamPart=s))
  
  resList <- append(newRE,newS)
  
  ### drop those models with the exact same configuration as the initial model
  
  resList <- resList[!sapply(resList,function(r)
    is.logical(all.equal(r, list(random=orgRE,gamPart=orgS))))]
  
  return(resList)
  
}


#######################################################################################
### duplicatedMers function
### purpose: get the duplicated merMod models in a list of models
duplicatedMers <- function(listModels)
{
  
  duplicated(sapply(listModels, function(x){
    if(class(x)[1]=="list") 
      as.character(Reduce(paste,deparse(formula(x$mer)))) else 
        as.character(Reduce(paste,deparse(formula(x))))
  }))
  
  
}
