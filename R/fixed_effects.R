renderThetaName <- function(namesTheta){
  #namesTheta <- trimws(names(getThetaNames(model)))
  r <- vector(mode = "list", length = length(namesTheta))
  for (i in seq_along(namesTheta)) {
    r[[i]] <- tagList(
      # div(style = "padding:8px;"),
      textInput(paste0("nameTheta", i), label = NULL, value = namesTheta[[i]]) %>% shinyjs::disabled(),
      # div(style = "padding:7px;")
    )
  }
  r
}

# getThetaNames without parsing
# getThetaNames <- function(stparms, covarlist){
#   fixeff_st <- vector(mode = "character", length = length(stparms))
#   
#   for(st in seq_along(stparms)){
#     fixeff_st[[st]] <- stparms[[st]]@fixedEffName
#   }
#   
#   if(length(covarlist) > 0){
#     fixeff_covar <- vector(mode = "list", length = length(covarlist))
#     for(cov_st in seq_along(covarlist)){
#       fixeff_covar[[cov_st]] <- paste0("d",names(covarlist[[cov_st]]@covarEffList), "d", covarlist[[cov_st]]@name)
#     }
#     fixeff_covar <- unlist(fixeff_covar)
#   } else {
#     fixeff_covar <- NULL
#   }
#   
#   fixeff <- c(fixeff_st, fixeff_covar)
#   
#   return(fixeff)
#   
# }
getThetaNames <- function(.Object){
            statements=attr(.Object,"statements")
            thetas=list()
            lineIndexes=grep("fixef\\(",.Object@statements)
            for ( indx in lineIndexes ) {
              line=.Object@statements[[indx]]
              tokens=unlist(strsplit(line,"fixef(",fixed=TRUE))
              name=unlist(strsplit(tokens[[2]],"=",fixed=TRUE))[[1]]
              name=unlist(strsplit(name,"(",fixed=TRUE))[[1]]
              nam=trimws(name,"both")
              val=unlist(strsplit(tokens[[2]],",",fixed=TRUE))[[2]]
              thetas[[name]]=val
            }

            return(thetas)
}


renderMetaFixedEff <- function(modelUser, input, reactiveFrozen, thetanames){
  posFixEffSelect <- grep(pattern = "nameTheta", names(input))
  for(i in posFixEffSelect){
    n <- names(input)[[i]]
    if(!is.null(input[[n]])){
      fe_parmname <- input[[n]]
      if(fe_parmname %notin% thetanames) next
      panel <- substr(n, 10,12)
      feInitial <- as.numeric(input[[paste0("feInitialNumeric", panel)]])
      feFreeze <- input[[paste0("feFreeze", panel)]]
      
      if(reactiveFrozen$pk && fe_parmname %in% paste0("tv",pk_stparms)){
        feFreeze <- TRUE
        }
      if(reactiveFrozen$emax && fe_parmname %in% paste0("tv",emax_stparms)){
          feFreeze <- TRUE
      }
      if(reactiveFrozen$indirect && fe_parmname %in% paste0("tv",indirect_stparms)){
          feFreeze <- TRUE
        }
      if(reactiveFrozen$linear && fe_parmname %in% paste0("tv",linear_stparms)){
          feFreeze <- TRUE
      }
          if(input[[paste0("feLowerNumeric", panel)]] == "" || feFreeze == TRUE){
            feLower <- NULL
          } else {
            feLower <- as.numeric(input[[paste0("feLowerNumeric", panel)]])
          }
          if(input[[paste0("feUpperNumeric", panel)]] == "" || feFreeze == TRUE){
            feUpper <- NULL
          } else {
            feUpper <- as.numeric(input[[paste0("feUpperNumeric", panel)]])
          }
          if(input[[paste0("feUnits", panel)]] == ""){
            feUnits <- NULL
          } else {
            feUnits <- input[[paste0("feUnits", panel)]]
          }
      
      tryCatch({
        modelUser <- 
          metaExpr({
            ..(modelUser) %>%
              fixedEffect(effect = ..(fe_parmname), value = ..(feInitial), lowerBound = ..(feLower), upperBound = ..(feUpper), isFrozen = ..(feFreeze), unit = ..(feUnits))
          })
      }, error = function(e){
        return(modelUser)
      })
      }
  }
  return(modelUser)
}

`%notin%` <- Negate(`%in%`)
