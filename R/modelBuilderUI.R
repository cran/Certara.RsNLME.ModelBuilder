.create_pml_wordlist <- function() {
  pml_statements <-
    "bolus|call|cfMacro|cfMacro1|cfMicro|count|covariate|delayInfCpt|deriv|dosepoint|dosepoint1|dosepoint2|error|event|fixef|group|include|infuse|interpolate|join|LL|LL|multi|observe|ordinal|override|peakreset|proc|ranef|secondary|sequence|sleep|sproc|stparm|struct|transit|urine|urinecpt"
  pml_controls <-
    "continue|do|else|for|if|return|switch|while|double|real"
  pml_functions <-
    "abs|acos|acosh|asin|asinh|atan|atan2|atanh|CalcTMax|cbrt|ceil|cos|cosh|delay|erf|erfc|erfunc|exp|exp2|expm1|fabs|factorial|floor|fmax|fmin|fmod|gammaDelay|hypot|icloglog|ilogb|ilogit|iloglog|iprobit|lambertw|ldexp|lgamm|lgamma|llrint|llround|ln|lnegbin|lnegbin_rp|lnorm|log|log|log10|log10|log1p|log2|logb|lphi|lpois|lrint|lround|max|min|nearbyint|peak|phi|pnegbin|pow|ppois|probit|remainder|rint|rnegbin|rnegbin_rp|round|rpois|scalbln|scalbn|sin|sinh|sqrt|tan|tanh|test|tgamma|trunc|unifToPoisson"
  pml_wordlist <-
    strsplit(
      paste(pml_statements, pml_controls, pml_functions, sep = "|"),
      split = "|",
      fixed = TRUE
    )
  names(pml_wordlist)[1] <- "pml"
  pml_wordlist
}

.check_syntax <- function(pmlfile_out, TDLLocation) {
  if (!is.na(file.info(pmlfile_out)$size) &&
      file.info(pmlfile_out)$size > 2) {
    # suppressing warnings since in case of TDL errors it is expected
    tdlargs <- paste("-i", pmlfile_out, dirname(pmlfile_out))
    tdllog <- suppressWarnings(system2(TDLLocation, stderr = TRUE, args = tdlargs))
    
    # if status exists in tdllog, we've found some error
    if (length(tdllog > 0)) {
      #& length(attr(tdllog, "status")) != 0) {
      # find out the latest row with the license info
      licenseinforow <- grep("Using\\W+license", tdllog)
      len_licenseinforow <- length(licenseinforow)
      if (len_licenseinforow > 0 &
          len_licenseinforow < length(tdllog)) {
        # get rid of license info if any
        tdllog <- tdllog[-(1:licenseinforow)]
      }
      
      tdllog <- paste0(tdllog, collapse = "\n")
    } else {
      tdllog <- ""
    }
    
    tdl_warnings_file <- "TDL5Warnings"
    if (file.exists(tdl_warnings_file)) {
      tdl_warnings_text <- readLines(tdl_warnings_file)
      tdllog <- paste0(tdllog, tdl_warnings_text, collapse = "\n")
    }
  } else if (is.na(file.info(pmlfile_out)$size)) {
    warning("pml file", pmlfile_out, "not found.")
    tdllog <-
      "Cannot find the file with statements used for syntax check"
  } else {
    tdlargs <- paste("-license_check")
    tdllog <- suppressWarnings(system2(TDLLocation, stderr = TRUE, args = tdlargs))
    nonetLineNo <- grep("Servername : no-net", tdllog)
    if (length(nonetLineNo > 0)) {
      tdllog <- tdllog[-nonetLineNo]
    }
  }
  
  tdllog
}


#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom fs path_home
#' @rawNamespace import(shiny, except = c(runExample, dataTableOutput, renderDataTable))
#' @importFrom shinyWidgets actionBttn
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom htmltools span tags HTML div h3 h4 h5 h6
#' @import shinyAce
#' @import shinymeta
#' @import Certara.RsNLME
#' @import shinyjs 
#' @rawNamespace import(bslib, except = c(bootstrap))
#' @importFrom magrittr %>%

.run_shiny_model_builder <- function(pmlfile_out, modelName, data, baseModel = NULL, dataName) {
  
  # Setup ----
  if (.Platform$OS.type == "windows") {
    installDir <-
      gsub("\\", "/", utils::shortPathName(Sys.getenv("INSTALLDIR")), fixed = TRUE)
    TDLLocation <- file.path(installDir, "TDL5.exe")
  } else {
    installDir <- Sys.getenv("INSTALLDIR")
    TDLLocation <-
      file.path(installDir, Sys.getenv("PML_BIN_DIR"), "TDL5")
  }

  syntaxcheck <- FALSE # Not necessary for built in model builder, slows performance
  if (!file.exists(TDLLocation)) {
    warning("\nTDL5 executable not found, syntax won't be checked.\n")
  }
  
  themes <- getAceThemes()
  pml_wordlist <- .create_pml_wordlist()
  
  appenv <- environment()
  
  server <- function(input, output, session) {

    # Setup reactiveVal ----
    # active_tab_param <- reactiveVal(FALSE)
    error_msg <- reactiveVal(NULL)
    
    # Error msg ----
    output$error_msg <- renderText({
      paste0(error_msg(), collapse = "\n")
    })
    outputOptions(output, "error_msg", suspendWhenHidden = FALSE)
    
    # pkModelInputs ----
    pkModelInputs <- reactiveValues(
      values = list(
        isPopulation = TRUE,
        parameterization = "Clearance",
        absorption = "Intravenous",
        numCompartments = 1,
        isClosedForm = TRUE,
        isTlag = FALSE,
        hasEliminationComp = FALSE,
        isFractionExcreted = FALSE,
        isSaturating = FALSE,
        infusionAllowed = FALSE,
        isDuration = FALSE,
        isStdevFrozen = FALSE)
    )
    
    # pk disable/enable ----
    observe(priority = 1, {
      if(input$pk_parameterization != "Clearance"){
        updateSelectInput(session, inputId = "pk_elimination", selected = "Linear", choices = c("Linear", "Michaelis-Menten"))
        shinyjs::disable(id = "pk_elimination")
      } else {
        shinyjs::enable(id = "pk_elimination")
      }
      if(input$pk_closed_form == TRUE){
        updateCheckboxInput(session, inputId = "pk_eliminationcomp", value = FALSE)
        shinyjs::disable(id = "pk_eliminationcomp")
      } else {
        shinyjs::enable(id = "pk_eliminationcomp")
      }
      if(input$pk_eliminationcomp == TRUE){
        updateCheckboxInput(session, inputId = "pk_closed_form", value = FALSE)
        shinyjs::disable(id = "pk_closed_form")
      } else {
        shinyjs::enable(id = "pk_closed_form")
      }
      if(input$pk_elimination == "Michaelis-Menten"){
        updateCheckboxInput(session, inputId = "pk_closed_form", value = FALSE)
        shinyjs::disable(id = "pk_closed_form")
      }
    })
    
    # emax enable/disable ----
    observe({
    if(input$pd_checkBaseline == TRUE){
      shinyjs::enable(id = "pd_checkFractional")
    } else {
      updateCheckboxInput(session, inputId = "pd_checkFractional", value = FALSE)
      shinyjs::disable(id = "pd_checkFractional")
    }
    })
    
    # Distributed Delay ----
    observeEvent(input$pk_parameterization,{
      if(input$pk_parameterization %in% c("Macro", "Macro1")){
        updateSelectInput(session, inputId = "pk_administration", choices = c("Intravenous", "First-Order"), selected = pkModelInputs$values$absorption)
      } else {
        updateSelectInput(session, inputId = "pk_administration", choices = c("Intravenous", "First-Order", "Gamma", "InverseGaussian", "Weibull"), selected = pkModelInputs$values$absorption)
    }
    })
    
    observe({
      if(input$pk_administration %in% c("Gamma", "InverseGaussian", "Weibull")){
        updateSelectInput(session, inputId = "pk_parameterization", choices = c("Clearance", "Micro"), selected = pkModelInputs$values$parameterization)
        updateCheckboxInput(session, inputId = "pk_closed_form", value = FALSE)
      } else {
        updateSelectInput(session, inputId = "pk_parameterization", choices = c("Clearance", "Micro", "Macro", "Macro1"), selected = pkModelInputs$values$parameterization)
      }
    })
    
    
    # Observer for pkmodel inputs ----
    observe(priority = 1, {
      
      # Check if any covType value is "Occasion" if switched to individual
      if (!input$pk_switch_population) {
        # Check instantiated covType inputs
        covType_index <-
          grep("^panel_\\d+-covType$", names(input), value = TRUE)
        # Extract the values of matched inputs
        covType_values <-
          lapply(covType_index, function(x)
            input[[x]])
        if (any(covType_values == "Occasion")) {
          showModal(modalDialog(
            size = "m",
            title = "Error",
            easyClose = TRUE,
            
            div(
              p(
                "Remove any occasion covariates before changing to individual model.",
                style = "color:red;"
              )
            ),
            
            footer = NULL
          ))
          updateCheckboxInput(session, inputId = "pk_switch_population", value = TRUE)
          return()
        }
      }
    
      
      pkModelInputs$values$isPopulation = input$pk_switch_population
      pkModelInputs$values$parameterization = input$pk_parameterization
      pkModelInputs$values$absorption = input$pk_administration
      pkModelInputs$values$numCompartments = as.numeric(input$pk_num_comp)
      
      if(input$pk_parameterization %in% c("Macro","Macro1")){
        pkModelInputs$values$isClosedForm <- TRUE
      } else if(input$pk_administration %in% c("Gamma", "InverseGaussian", "Weibull")){
        pkModelInputs$values$isClosedForm <- FALSE
      } else {
        pkModelInputs$values$isClosedForm <- input$pk_closed_form
      }
      pkModelInputs$values$isTlag = input$pk_time_lag
      pkModelInputs$values$hasEliminationComp = ifelse(input$pk_closed_form == TRUE || input$pk_parameterization == "Macro" || input$pk_parameterization == "Macro1" , FALSE, input$pk_eliminationcomp)
      pkModelInputs$values$isFractionExcreted = ifelse(pkModelInputs$values$hasEliminationComp == TRUE, input$pk_fe, FALSE)
      pkModelInputs$values$isSaturating = ifelse(input$pk_elimination == "Michaelis-Menten" && input$pk_parameterization == "Clearance", TRUE, FALSE)
      pkModelInputs$values$infusionAllowed = input$pk_infusion
      pkModelInputs$values$isDuration = ifelse(input$pk_infusion == TRUE, input$pk_duration, FALSE)
      pkModelInputs$values$isStdevFrozen = FALSE
    })
    
    
    pkModelReactives <- reactiveValues(
      values = list(
        namesStParm = NULL,
        namesTheta = NULL
      )
    )
    
    observe({
      req(model())
      pkModelReactives$values$namesStParm = structuralParameterNames(model())
    })
    
    observe({
      req(pkModelReactives$values$namesStParm)

      cov_sel_inputs <- grep("covEffects", names(input), value = TRUE)
      for (i in cov_sel_inputs) {
        selection <- input[[i]]
        updateSelectInput(session, i, selected = selection, choices = pkModelReactives$values$namesStParm)
      }
    })
    
    
    # 'Model' Object ----
    model <- metaReactive2(varname = "model",{
      # req(tabInfo()) 
        if(input$modeltype == "PK"){
          modelUser <- metaExpr({
            pkmodel(
              isPopulation = ..(pkModelInputs$values$isPopulation),
              parameterization = ..(pkModelInputs$values$parameterization),
              absorption = ..(ifelse(pkModelInputs$values$absorption == "First-Order", "Extravascular", pkModelInputs$values$absorption)),
              numCompartments = ..(pkModelInputs$values$numCompartments),
              isClosedForm = ..(pkModelInputs$values$isClosedForm), 
              isTlag = ..(pkModelInputs$values$isTlag),
              hasEliminationComp = ..(pkModelInputs$values$hasEliminationComp),
              isFractionExcreted = ..(pkModelInputs$values$isFractionExcreted),
              isSaturating = ..(pkModelInputs$values$isSaturating),
              infusionAllowed = ..(pkModelInputs$values$infusionAllowed),
              isDuration = ..(pkModelInputs$values$isDuration),
              columnMap = FALSE,
              modelName = ..(modelName),
              workingDir = ..(dirname(pmlfile_out))
            )
          })
        }
        if(input$modeltype == "Emax"){
          modelUser <- metaExpr({
            emaxmodel(
              isPopulation = ..(pkModelInputs$values$isPopulation),
              checkBaseline = ..(input$pd_checkBaseline),
              checkFractional = ..(input$pd_checkFractional),
              checkInhibitory = ..(input$pd_checkInhibitory),
              checkSigmoid = ..(input$pd_checkSigmoid),
              columnMap = FALSE,
              modelName = ..(modelName),
              workingDir = ..(dirname(pmlfile_out))
            )
          })
        }
        if(input$modeltype == "PK/Emax"){
          modelUser <- metaExpr({
            pkemaxmodel(
              isPopulation = ..(pkModelInputs$values$isPopulation),
              parameterization = ..(pkModelInputs$values$parameterization),
              absorption = ..(ifelse(pkModelInputs$values$absorption == "First-Order", "Extravascular", pkModelInputs$values$absorption)),
              numCompartments = ..(pkModelInputs$values$numCompartments),
              isClosedForm = ..(pkModelInputs$values$isClosedForm), 
              isTlag = ..(pkModelInputs$values$isTlag),
              hasEliminationComp = ..(pkModelInputs$values$hasEliminationComp),
              isFractionExcreted = ..(pkModelInputs$values$isFractionExcreted),
              isSaturating = ..(pkModelInputs$values$isSaturating),
              infusionAllowed = ..(pkModelInputs$values$infusionAllowed),
              isDuration = ..(pkModelInputs$values$isDuration),
              isPkFrozen = ..(input$pd_isPkFrozen),
              hasEffectsCompartment = ..(input$pd_hasEffectsCompartment),
              checkBaseline = ..(input$pd_checkBaseline),
              checkFractional = ..(input$pd_checkFractional),
              checkInhibitory = ..(input$pd_checkInhibitory),
              checkSigmoid = ..(input$pd_checkSigmoid),
              isEmaxFrozen = ..(input$pd_isEmaxFrozen),
              columnMap = FALSE,
              modelName = ..(modelName),
              workingDir = ..(dirname(pmlfile_out))
            )
          })
        }
        if(input$modeltype == "PK/Indirect"){
          modelUser <- metaExpr({
            pkindirectmodel(
              isPopulation = ..(pkModelInputs$values$isPopulation),
              parameterization = ..(pkModelInputs$values$parameterization),
              absorption = ..(ifelse(pkModelInputs$values$absorption == "First-Order", "Extravascular", pkModelInputs$values$absorption)),
              numCompartments = ..(pkModelInputs$values$numCompartments),
              isClosedForm = ..(pkModelInputs$values$isClosedForm), 
              isTlag = ..(pkModelInputs$values$isTlag),
              hasEliminationComp = ..(pkModelInputs$values$hasEliminationComp),
              isFractionExcreted = ..(pkModelInputs$values$isFractionExcreted),
              isSaturating = ..(pkModelInputs$values$isSaturating),
              infusionAllowed = ..(pkModelInputs$values$infusionAllowed),
              isDuration = ..(pkModelInputs$values$isDuration),
              isPkFrozen = ..(input$pki_isPkFrozen),
              hasEffectsCompartment = ..(input$pki_hasEffectsCompartment),
              indirectType = ..(gsub(" ", "", input$pki_indirectType, fixed = TRUE)),
              isBuildup = ..(input$pki_isBuildup),
              isExponent = ..(input$pki_isExponent),
              indirectFrozen = ..(input$pki_indirectFrozen),                
              columnMap = FALSE,
              modelName = ..(modelName),
              workingDir = ..(dirname(pmlfile_out))
            )
          })
        }
        
        if(input$modeltype == "Linear"){
          modelUser <- metaExpr({
            linearmodel(
              isPopulation = ..(pkModelInputs$values$isPopulation),
              type = ..(input$l_type),
              columnMap = FALSE,
              modelName = ..(modelName),
              workingDir = ..(dirname(pmlfile_out))
            )
          })
        }
        
        if(input$modeltype == "PK/Linear"){
          modelUser <- metaExpr({
            pklinearmodel(
              isPopulation = ..(pkModelInputs$values$isPopulation),
              parameterization = ..(pkModelInputs$values$parameterization),
              absorption = ..(ifelse(pkModelInputs$values$absorption == "First-Order", "Extravascular", pkModelInputs$values$absorption)),
              numCompartments = ..(pkModelInputs$values$numCompartments),
              isClosedForm = ..(pkModelInputs$values$isClosedForm), 
              isTlag = ..(pkModelInputs$values$isTlag),
              hasEliminationComp = ..(pkModelInputs$values$hasEliminationComp),
              isFractionExcreted = ..(pkModelInputs$values$isFractionExcreted),
              isSaturating = ..(pkModelInputs$values$isSaturating),
              infusionAllowed = ..(pkModelInputs$values$infusionAllowed),
              isDuration = ..(pkModelInputs$values$isDuration),
              isPkFrozen = ..(input$pkl_isPkFrozen),
              hasEffectsCompartment = ..(input$pkl_hasEffectsCompartment),
              linearType = ..(input$pkl_linearType),
              isLinearFrozen = ..(input$pkl_isLinearFrozen),
              columnMap = FALSE,
              modelName = ..(modelName),
              workingDir = ..(dirname(pmlfile_out))
            )
          })
        }
        
      ############-Residual Error Term 1-###################
      if(!is.null(errorNamesReactive$values$predNames[[1]])){
        
        if(!is.null(input$errorType1_1)){
          
          req(pkResErrorInputs$values$errorType1)
          
          if(is.na(pkResErrorInputs$values$staticLLOQ1)){
            
            if(# !is.null(pkResErrorInputs$values$errorType1) && pkResErrorInputs$values$errorType1 == "Power") {
              pkResErrorInputs$values$errorType1 == "Power"){
              
              tryCatch({
                modelUser <- metaExpr({
                  ..(modelUser) %>%
                    residualError(
                      predName = ..(errorNamesReactive$values$predNames[[1]]),
                      errorType = ..(pkResErrorInputs$values$errorType1),
                      SD = ..(as.numeric(pkResErrorInputs$values$SD1)),
                      isFrozen = ..(pkResErrorInputs$values$isFrozen1),
                      isBQL = ..(pkResErrorInputs$values$isBQL1),
                      exponent = ..(as.numeric(pkResErrorInputs$values$exponent1))
                    )
                })
              }, error = function(e){
                return(modelUser)
              })
            } else {
              
              tryCatch({
                modelUser <- metaExpr({
                  ..(modelUser) %>%
                    residualError(
                      predName = ..(errorNamesReactive$values$predNames[[1]]),
                      errorType = ..(pkResErrorInputs$values$errorType1),
                      SD = ..(as.numeric(pkResErrorInputs$values$SD1)),
                      isFrozen = ..(pkResErrorInputs$values$isFrozen1),
                      isBQL = ..(pkResErrorInputs$values$isBQL1)
                    )
                })
              }, error = function(e){
                return(modelUser)
              })
            }
          } else {
            
            if(pkResErrorInputs$values$errorType1 == "Power"){
              
              tryCatch({
                modelUser <- metaExpr({
                  ..(modelUser) %>%
                    residualError(
                      predName = ..(errorNamesReactive$values$predNames[[1]]),
                      errorType = ..(pkResErrorInputs$values$errorType1),
                      SD = ..(as.numeric(pkResErrorInputs$values$SD1)),
                      isFrozen = ..(pkResErrorInputs$values$isFrozen1),
                      isBQL = ..(pkResErrorInputs$values$isBQL1),
                      staticLLOQ = ..(pkResErrorInputs$values$staticLLOQ1),
                      exponent = ..(as.numeric(pkResErrorInputs$values$exponent1))
                    )
                })
              }, error = function(e){
                return(modelUser)
              })
            } else {
              
              tryCatch({
                modelUser <- metaExpr({
                  ..(modelUser) %>%
                    residualError(
                      predName = ..(errorNamesReactive$values$predNames[[1]]),
                      errorType = ..(pkResErrorInputs$values$errorType1),
                      SD = ..(as.numeric(pkResErrorInputs$values$SD1)),
                      isFrozen = ..(pkResErrorInputs$values$isFrozen1),
                      isBQL = ..(pkResErrorInputs$values$isBQL1),
                      staticLLOQ = ..(pkResErrorInputs$values$staticLLOQ1)
                    )
                })
              }, error = function(e){
                return(modelUser)
              })
            }
          }
        }
      
        
        ############-Residual Error Term 2-###################
        if(!is.null(input$errorType2_1)){
          if(is.na(pkResErrorInputs$values$staticLLOQ2)){
            if(pkResErrorInputs$values$errorType2 == "Power"){
              tryCatch({
                modelUser <- metaExpr({
                  ..(modelUser) %>%
                    residualError(
                      predName = ..(errorNamesReactive$values$predNames[[2]]),
                      errorType = ..(pkResErrorInputs$values$errorType2),
                      SD = ..(as.numeric(pkResErrorInputs$values$SD2)),
                      isFrozen = ..(pkResErrorInputs$values$isFrozen2),
                      isBQL = ..(pkResErrorInputs$values$isBQL2),
                      exponent = ..(as.numeric(pkResErrorInputs$values$exponent2))
                    )
                })
              }, error = function(e){
                return(modelUser)
              })
            } else {
              tryCatch({
                modelUser <- metaExpr({
                  ..(modelUser) %>%
                    residualError(
                      predName = ..(errorNamesReactive$values$predNames[[2]]),
                      errorType = ..(pkResErrorInputs$values$errorType2),
                      SD = ..(as.numeric(pkResErrorInputs$values$SD2)),
                      isFrozen = ..(pkResErrorInputs$values$isFrozen2),
                      isBQL = ..(pkResErrorInputs$values$isBQL2)
                    )
                })
              }, error = function(e){
                return(modelUser)
              })
            }
          } else {
            if(pkResErrorInputs$values$errorType2 == "Power"){
              tryCatch({
                modelUser <- metaExpr({
                  ..(modelUser) %>%
                    residualError(
                      predName = ..(errorNamesReactive$values$predNames[[2]]),
                      errorType = ..(pkResErrorInputs$values$errorType2),
                      SD = ..(as.numeric(pkResErrorInputs$values$SD2)),
                      isFrozen = ..(pkResErrorInputs$values$isFrozen2),
                      isBQL = ..(pkResErrorInputs$values$isBQL2),
                      staticLLOQ = ..(pkResErrorInputs$values$staticLLOQ2),
                      exponent = ..(as.numeric(pkResErrorInputs$values$exponent2))
                    )
                })
              }, error = function(e){
                return(modelUser)
              })
            } else {
              tryCatch({
                modelUser <- metaExpr({
                  ..(modelUser) %>%
                    residualError(
                      predName = ..(errorNamesReactive$values$predNames[[2]]),
                      errorType = ..(pkResErrorInputs$values$errorType2),
                      SD = ..(as.numeric(pkResErrorInputs$values$SD2)),
                      isFrozen = ..(pkResErrorInputs$values$isFrozen2),
                      isBQL = ..(pkResErrorInputs$values$isBQL2),
                      staticLLOQ = ..(pkResErrorInputs$values$staticLLOQ2)
                    )
                })
              }, error = function(e){
                return(modelUser)
              })
            }
          }
        }
      
        
        ############-Residual Error Term 3-###################
        if(!is.null(input$errorType3_1)){
          if(is.na(pkResErrorInputs$values$staticLLOQ3)){
            if(pkResErrorInputs$values$errorType3 == "Power"){
              tryCatch({
                modelUser <- metaExpr({
                  ..(modelUser) %>%
                    residualError(
                      predName = ..(errorNamesReactive$values$predNames[[3]]),
                      errorType = ..(pkResErrorInputs$values$errorType3),
                      SD = ..(as.numeric(pkResErrorInputs$values$SD3)),
                      isFrozen = ..(pkResErrorInputs$values$isFrozen3),
                      isBQL = ..(pkResErrorInputs$values$isBQL3),
                      exponent = ..(as.numeric(pkResErrorInputs$values$exponent3))
                    )
                })
              }, error = function(e){
                return(modelUser)
              })
            } else {
              tryCatch({
                modelUser <- metaExpr({
                  ..(modelUser) %>%
                    residualError(
                      predName = ..(errorNamesReactive$values$predNames[[3]]),
                      errorType = ..(pkResErrorInputs$values$errorType3),
                      SD = ..(as.numeric(pkResErrorInputs$values$SD3)),
                      isFrozen = ..(pkResErrorInputs$values$isFrozen3),
                      isBQL = ..(pkResErrorInputs$values$isBQL3)
                    )
                })
              }, error = function(e){
                return(modelUser)
              })
            }
          } else {
            if(pkResErrorInputs$values$errorType3 == "Power"){
              tryCatch({
                modelUser <- metaExpr({
                  ..(modelUser) %>%
                    residualError(
                      predName = ..(errorNamesReactive$values$predNames[[3]]),
                      errorType = ..(pkResErrorInputs$values$errorType3),
                      SD = ..(as.numeric(pkResErrorInputs$values$SD3)),
                      isFrozen = ..(pkResErrorInputs$values$isFrozen3),
                      isBQL = ..(pkResErrorInputs$values$isBQL3),
                      staticLLOQ = ..(pkResErrorInputs$values$staticLLOQ3),
                      exponent = ..(as.numeric(pkResErrorInputs$values$exponent3))
                    )
                })
              }, error = function(e){
                return(modelUser)
              })
            } else {
              tryCatch({
                modelUser <- metaExpr({
                  ..(modelUser) %>%
                    residualError(
                      predName = ..(errorNamesReactive$values$predNames[[3]]),
                      errorType = ..(pkResErrorInputs$values$errorType3),
                      SD = ..(as.numeric(pkResErrorInputs$values$SD3)),
                      isFrozen = ..(pkResErrorInputs$values$isFrozen3),
                      isBQL = ..(pkResErrorInputs$values$isBQL3),
                      staticLLOQ = ..(pkResErrorInputs$values$staticLLOQ3)
                    )
                })
              }, error = function(e){
                return(modelUser)
              })
            }
          }
        }
      }
        
      ###############-Structural Parameters-######################
      suppressWarnings(
      tryCatch({
        modelUser <- renderMetaStParm(modelUser, input, pkModelReactives$values$namesStParm)
      }, error = function(e){
        return(modelUser)
      })
      )
        

      ###############-Covariates-######################
      tryCatch({
        suppressWarnings(
      modelUser <- renderMetaCov(modelUser, input)
        )
      }, error = function(e){
        return(modelUser)
      })
      
      
      ###############-Fixed Effects-####################
      tryCatch({
      modelUser <- renderMetaFixedEff(modelUser, input, reactiveFrozen$values, reactiveThetaNames$values)
      }, error = function(e){
       return(modelUser)
      }, warning = function(w){
       return(modelUser)
      })
      
      
      ###############-Random Effects-####################
      if(pkModelInputs$values$isPopulation){
        if(!is.null(input$ranEffSelections1)){
          if(input$diagonalRanEff1 == TRUE){
            vals1 <- getDiagonalRanEff1(input)
            ranEff1 <- c(input$ranEffSelections1)
            if(length(ranEff1) != length(vals1)){
              vals1 <- vals1[1:length(ranEff1)]
              if(any(is.na(vals1))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff1, vals1, input$diagonalRanEff1, input$freezeRanEff1)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff1, vals1, input$diagonalRanEff1, input$freezeRanEff1)
            }
          } else {
            vals1 <- getBlockRanEff1(input)
            ranEff1 <- c(input$ranEffSelections1)
            if(reqBlockNum(length(ranEff1)) != length(vals1)){
              vals1 <- vals1[1:reqBlockNum(length(ranEff1))]
              if(any(is.na(vals1))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff1, vals1, input$diagonalRanEff1, input$freezeRanEff1)
              return(modelUser)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff1, vals1, input$diagonalRanEff1, input$freezeRanEff1)
            }

          }
        }

        if(!is.null(input$ranEffSelections2)){
          if(input$diagonalRanEff2 == TRUE){
            vals2 <- getDiagonalRanEff2(input)
            ranEff2 <- c(input$ranEffSelections2)
            if(length(ranEff2) != length(vals2)){
              vals2 <- vals2[1:length(ranEff2)]
              if(any(is.na(vals2)) || any(vals2 == "")) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff2, vals2, input$diagonalRanEff2, input$freezeRanEff2)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff2, vals2, input$diagonalRanEff2, input$freezeRanEff2)
            } 
          } else {
            vals2 <- getBlockRanEff2(input)
            ranEff2 <- c(input$ranEffSelections2)
            if(reqBlockNum(length(ranEff2)) != length(vals2)){
              vals2 <- vals2[1:reqBlockNum(length(ranEff2))]
              if(any(is.na(vals2))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff2, vals2, input$diagonalRanEff2, input$freezeRanEff2)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff2, vals2, input$diagonalRanEff2, input$freezeRanEff2)
            }

          }
        }

        if(!is.null(input$ranEffSelections3)){
          if(input$diagonalRanEff3 == TRUE){
            vals3 <- getDiagonalRanEff3(input)
            ranEff3 <- c(input$ranEffSelections3)
            if(length(ranEff3) != length(vals3)){
              vals3 <- vals3[1:length(ranEff3)]
              if(any(is.na(vals3))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff3, vals3, input$diagonalRanEff3, input$freezeRanEff3)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff3, vals3, input$diagonalRanEff3, input$freezeRanEff3)
            }
          } else {
            vals3 <- getBlockRanEff3(input)
            ranEff3 <- c(input$ranEffSelections3)
            if(reqBlockNum(length(ranEff3)) != length(vals3)){
              vals3 <- vals3[1:reqBlockNum(length(ranEff3))]
              if(any(is.na(vals3))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff3, vals3, input$diagonalRanEff3, input$freezeRanEff3)
              return(modelUser)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff3, vals3, input$diagonalRanEff3, input$freezeRanEff3)
            }

          }
        }
        
        if(!is.null(input$ranEffSelections4)){
          if(input$diagonalRanEff4 == TRUE){
            vals4 <- getDiagonalRanEff4(input)
            ranEff4 <- c(input$ranEffSelections4)
            if(length(ranEff4) != length(vals4)){
              vals4 <- vals4[1:length(ranEff4)]
              if(any(is.na(vals4))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff4, vals4, input$diagonalRanEff4, input$freezeRanEff4)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff4, vals4, input$diagonalRanEff4, input$freezeRanEff4)
            }
          } else {
            vals4 <- getBlockRanEff4(input)
            ranEff4 <- c(input$ranEffSelections4)
            if(reqBlockNum(length(ranEff4)) != length(vals4)){
              vals4 <- vals4[1:reqBlockNum(length(ranEff4))]
              if(any(is.na(vals4))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff4, vals4, input$diagonalRanEff4, input$freezeRanEff4)
              return(modelUser)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff4, vals4, input$diagonalRanEff4, input$freezeRanEff4)
            }
            
          }
        }
        
        if(!is.null(input$ranEffSelections5)){
          if(input$diagonalRanEff5 == TRUE){
            vals5 <- getDiagonalRanEff5(input)
            ranEff5 <- c(input$ranEffSelections5)
            if(length(ranEff5) != length(vals5)){
              vals5 <- vals5[1:length(ranEff5)]
              if(any(is.na(vals5))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff5, vals5, input$diagonalRanEff5, input$freezeRanEff5)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff5, vals5, input$diagonalRanEff5, input$freezeRanEff5)
            }
          } else {
            vals5 <- getBlockRanEff5(input)
            ranEff5 <- c(input$ranEffSelections5)
            if(reqBlockNum(length(ranEff5)) != length(vals5)){
              vals5 <- vals5[1:reqBlockNum(length(ranEff5))]
              if(any(is.na(vals5))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff5, vals5, input$diagonalRanEff5, input$freezeRanEff5)
              return(modelUser)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff5, vals5, input$diagonalRanEff5, input$freezeRanEff5)
            }
            
          }
        }
        if(!is.null(input$ranEffSelections6)){
          if(input$diagonalRanEff6 == TRUE){
            vals6 <- getDiagonalRanEff6(input)
            ranEff6 <- c(input$ranEffSelections6)
            if(length(ranEff6) != length(vals6)){
              vals6 <- vals6[1:length(ranEff6)]
              if(any(is.na(vals6))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff6, vals6, input$diagonalRanEff6, input$freezeRanEff6)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff6, vals6, input$diagonalRanEff6, input$freezeRanEff6)
            }
          } else {
            vals6 <- getBlockRanEff6(input)
            ranEff6 <- c(input$ranEffSelections6)
            if(reqBlockNum(length(ranEff6)) != length(vals6)){
              vals6 <- vals6[1:reqBlockNum(length(ranEff6))]
              if(any(is.na(vals6))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff6, vals6, input$diagonalRanEff6, input$freezeRanEff6)
              return(modelUser)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff6, vals6, input$diagonalRanEff6, input$freezeRanEff6)
            }
            
          }
        }
        
        if(!is.null(input$ranEffSelections7)){
          if(input$diagonalRanEff7 == TRUE){
            vals7 <- getDiagonalRanEff7(input)
            ranEff7 <- c(input$ranEffSelections7)
            if(length(ranEff7) != length(vals7)){
              vals7 <- vals7[1:length(ranEff7)]
              if(any(is.na(vals7))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff7, vals7, input$diagonalRanEff7, input$freezeRanEff7)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff7, vals7, input$diagonalRanEff7, input$freezeRanEff7)
            }
          } else {
            vals7 <- getBlockRanEff7(input)
            ranEff7 <- c(input$ranEffSelections7)
            if(reqBlockNum(length(ranEff7)) != length(vals7)){
              vals7 <- vals7[1:reqBlockNum(length(ranEff7))]
              if(any(is.na(vals7))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff7, vals7, input$diagonalRanEff7, input$freezeRanEff7)
              return(modelUser)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff7, vals7, input$diagonalRanEff7, input$freezeRanEff7)
            }
            
          }
        }
        
        if(!is.null(input$ranEffSelections8)){
          if(input$diagonalRanEff8 == TRUE){
            vals8 <- getDiagonalRanEff8(input)
            ranEff8 <- c(input$ranEffSelections8)
            if(length(ranEff8) != length(vals8)){
              vals8 <- vals8[1:length(ranEff8)]
              if(any(is.na(vals8))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff8, vals8, input$diagonalRanEff8, input$freezeRanEff8)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff8, vals8, input$diagonalRanEff8, input$freezeRanEff8)
            }
          } else {
            vals8 <- getBlockRanEff8(input)
            ranEff8 <- c(input$ranEffSelections8)
            if(reqBlockNum(length(ranEff8)) != length(vals8)){
              vals8 <- vals8[1:reqBlockNum(length(ranEff8))]
              if(any(is.na(vals8))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff8, vals8, input$diagonalRanEff8, input$freezeRanEff8)
              return(modelUser)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff8, vals8, input$diagonalRanEff8, input$freezeRanEff8)
            }
            
          }
        }
        
        if(!is.null(input$ranEffSelections9)){
          if(input$diagonalRanEff9 == TRUE){
            vals9 <- getDiagonalRanEff9(input)
            ranEff9 <- c(input$ranEffSelections9)
            if(length(ranEff9) != length(vals9)){
              vals9 <- vals9[1:length(ranEff9)]
              if(any(is.na(vals9))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff9, vals9, input$diagonalRanEff9, input$freezeRanEff9)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff9, vals9, input$diagonalRanEff9, input$freezeRanEff9)
            }
          } else {
            vals9 <- getBlockRanEff9(input)
            ranEff9 <- c(input$ranEffSelections9)
            if(reqBlockNum(length(ranEff9)) != length(vals9)){
              vals9 <- vals9[1:reqBlockNum(length(ranEff9))]
              if(any(is.na(vals9))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff9, vals9, input$diagonalRanEff9, input$freezeRanEff9)
              return(modelUser)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff9, vals9, input$diagonalRanEff9, input$freezeRanEff9)
            }
            
          }
        }
        
        if(!is.null(input$ranEffSelections10)){
          if(input$diagonalRanEff10 == TRUE){
            vals10 <- getDiagonalRanEff10(input)
            ranEff10 <- c(input$ranEffSelections10)
            if(length(ranEff10) != length(vals10)){
              vals10 <- vals10[1:length(ranEff10)]
              if(any(is.na(vals10))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff10, vals10, input$diagonalRanEff10, input$freezeRanEff10)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff10, vals10, input$diagonalRanEff10, input$freezeRanEff10)
            }
          } else {
            vals10 <- getBlockRanEff10(input)
            ranEff10 <- c(input$ranEffSelections10)
            if(reqBlockNum(length(ranEff10)) != length(vals10)){
              vals10 <- vals10[1:reqBlockNum(length(ranEff10))]
              if(any(is.na(vals10))) return(modelUser)
              modelUser <- renderMetaRanEff(modelUser, ranEff10, vals10, input$diagonalRanEff10, input$freezeRanEff10)
              return(modelUser)
            } else {
              modelUser <- renderMetaRanEff(modelUser, ranEff10, vals10, input$diagonalRanEff10, input$freezeRanEff10)
            }
            
          }
        }
        
      }
        
        
        

      ###############-Dose Cycle-SS-######################
      if(input$isSSDoseCycle == TRUE){
      tryCatch({
        modelUser <- renderMetaDoseSS(modelUser, input)
      }, error = function(e){
        return(modelUser)
      })
      }

      ################-Dose Cycle-ADDL-######################
      if(input$isADDLDoseCycle == TRUE){
      tryCatch({
        modelUser <- renderMetaDoseADDL(modelUser, input)
      }, error = function(e){
        return(modelUser)
      })
      }
        
      ########## User defined Extra Column Definitions- ######
      tryCatch({
        modelUser <- renderMetaExtraDef(modelUser, input)
      }, error = function(e){
        return(modelUser)
      })
        
      ###############-Data-Mapping-######################
      suppressWarnings(
      modelUser <- metaExpr({
        ..(modelUser) %>%
          dataMapping(data)
      })
      )
      
      
      ###############Column-Mapping-######################
      if(!is.null(reactiveModelVarNames$values)){
        columnnames <- c(paste0(input$col1_1, collapse = ","), input$col2_1, input$col3_1, input$col4_1, input$col5_1, input$col6_1, input$col7_1, input$col8_1,  input$col9_1,  input$col10_1,
                                input$col11_1, input$col12_1, input$col13_1,  input$col14_1,  input$col15_1, input$col16_1, input$col17_1, input$col18_1, input$col19_1, input$col20_1, 
                                input$col21_1, input$col22_1, input$col23_1,  input$col24_1,  input$col25_1, input$col26_1, input$col27_1, input$col28_1, input$col29_1, input$col30_1
                         )
        columnnames <- columnnames[1:length(reactiveModelVarNames$values)]
        columnnames <- replace(columnnames, columnnames == "", "?")
        columnnames <- replace(columnnames, is.na(columnnames), "?")
        
        
          tryCatch({
            suppressWarnings(
            modelUser <- metaExpr({
              ..(modelUser) %>%
                colMapping(..(setNames(columnnames, reactiveModelVarNames$values)[!grepl(" ",setNames(columnnames, reactiveModelVarNames$values))]))
            })
            )
          }, error = function(e){
            return(modelUser)
          })
      }
        
        
      ###############-addReset-######################
      if (input$reset == TRUE) {
        req(input$resetLow, input$resetHi)
        if (is.null(input$resetSelectDose) || input$resetSelectDose == " ") {
          reset_col <- "?"
        } else {
          reset_col <- input$resetSelectDose
        }
        tryCatch({
          modelUser <- metaExpr({
            ..(modelUser) %>%
              addReset(
                low = ..(input$resetLow),
                hi = ..(input$resetHi),
                Reset = ..(reset_col)
              )
          })
        }, error = function(e) {
          return(modelUser)
        })
      }
        
      ###############-addMDV-######################
      if(input$mdv == TRUE){
        if (is.null(input$mdvSelectDose) || input$mdvSelectDose == " ") {
          mdv_col <- "?"
        } else {
          mdv_col <- input$mdvSelectDose
        }
        tryCatch({
          modelUser <- metaExpr({
            ..(modelUser) %>%
              addMDV(MDV = ..(mdv_col))
          })
        }, error = function(e){
          return(modelUser)
        })
      }
        
      ###############-addSteadyState (Column Def)-######################
      if(input$isSS == TRUE){
        if (is.null(input$ssSelectDose) || input$ssSelectDose == " ") {
          ss_col <- "?"
        } else {
          ss_col <- input$ssSelectDose
        }
        if (is.null(input$iiSelectDose) || input$iiSelectDose == " ") {
          ii_col <- "?"
        } else {
          ii_col <- input$iiSelectDose 
        }
        if(input$isSSOffset == TRUE){
          if (is.null(input$ssOffsetSelectDose) || input$ssOffsetSelectDose == " ") {
            ss_offset__col <- "?"
          } else {
            ss_offset__col <- input$ssOffsetSelectDose 
          }
          
          tryCatch({
            modelUser <- metaExpr({
              ..(modelUser) %>%
                addSteadyState(SS = ..(ss_col), II = ..(ii_col), SSOffset = ..(ss_offset__col))
            })
          }, error = function(e){
            return(modelUser)
          })
        } else {
          tryCatch({
            modelUser <- metaExpr({
              ..(modelUser) %>%
                addSteadyState(SS = ..(ss_col), II = ..(ii_col))
            })
          }, error = function(e){
            return(modelUser)
          })
        }
      }
      ###############-addADDL (Column Def)-######################
      if(input$isADDL == TRUE){
        if (is.null(input$addlSelectDose) || input$addlSelectDose == " ") {
          addl_col <- "?"
        } else {
          addl_col <- input$addlSelectDose
        }
        if (is.null(input$iiSelectDose) || input$iiSelectDose == " ") {
          ii_col <- "?"
        } else {
          ii_col <- input$iiSelectDose
        }
        tryCatch({
          modelUser <- metaExpr({
            ..(modelUser) %>%
              addADDL(ADDL = ..(addl_col), II = ..(ii_col))
          })
        }, error = function(e){
          return(modelUser)
        })
      }
      
      modelUser
    })
    

    #-------------Structural Model > Residual Model ----------------------------------------------------------------
    
    errorNamesReactive <- reactiveValues(
      values = list(
        predNames = NULL
      )
    )
    
    observe({
      req(model())
      errorNamesReactive$values$predNames = residualEffectNames(model())
    })
    
    pkResErrorInputs <- reactiveValues(
      values = list(
        errorType1 = NULL,
        SD1 = NULL,
        isFrozen1 = FALSE,
        isBQL1 = FALSE,
        staticLLOQ1 = NULL,
        exponent1 = NULL,
        errorType2 = NULL,
        SD2 = NULL,
        isFrozen2 = FALSE,
        isBQL2 = FALSE,
        staticLLOQ2 = NULL,
        exponent2 = NULL,
        errorType3 = NULL,
        SD3 = NULL,
        isFrozen3 = FALSE,
        isBQL3 = FALSE,
        staticLLOQ3 = NULL,
        exponent3 = NULL)
    )
    
    
    observe({
      pkResErrorInputs$values$errorType1 = input$errorType1_1
      pkResErrorInputs$values$SD1 = input$errorStDev1_1
      pkResErrorInputs$values$isFrozen1 = input$errorFreeze1_1
      pkResErrorInputs$values$isBQL1 = input$errorBQL1_1
      pkResErrorInputs$values$staticLLOQ1 = ifelse(pkResErrorInputs$values$isBQL1 == TRUE && input$errorStaticLLOQ1_1 == TRUE, input$errorStaticVal1_1, NA)
      pkResErrorInputs$values$exponent1 = input$errorPower1_1
      pkResErrorInputs$values$errorType2 = input$errorType2_1
      pkResErrorInputs$values$SD2 = input$errorStDev2_1
      pkResErrorInputs$values$isFrozen2 = input$errorFreeze2_1
      pkResErrorInputs$values$isBQL2 = input$errorBQL2_1
      pkResErrorInputs$values$staticLLOQ2 = ifelse(pkResErrorInputs$values$isBQL2 == TRUE && input$errorStaticLLOQ2_1 == TRUE, input$errorStaticVal2_1, NA)
      pkResErrorInputs$values$exponent2 = input$errorPower2_1
      pkResErrorInputs$values$errorType3 = input$errorType3_1
      pkResErrorInputs$values$SD3 = input$errorStDev3_1
      pkResErrorInputs$values$isFrozen3 = input$errorFreeze3_1
      pkResErrorInputs$values$isBQL3 = input$errorBQL3_1
      pkResErrorInputs$values$staticLLOQ3 = ifelse(pkResErrorInputs$values$isBQL3 == TRUE && input$errorStaticLLOQ3_1 == TRUE, input$errorStaticVal3_1, NA)
      pkResErrorInputs$values$exponent3 = input$errorPower3_1
    })
    
    observe({
      req(input$errorType1_1)
      if(input$errorType1_1 %in% c("Additive", "AdditiveMultiplicative", "MixRatio")){
        updateNumericInput(session, inputId = "errorStDev1_1", value = 1)
      } else {
        updateNumericInput(session, inputId = "errorStDev1_1", value = .1)
      }
    })
    
    observe({
      req(input$errorType2_1)
      if(input$errorType2_1 %in% c("Additive", "AdditiveMultiplicative", "MixRatio")){
        updateNumericInput(session, inputId = "errorStDev2_1", value = 1)
      } else {
        updateNumericInput(session, inputId = "errorStDev2_1", value = .1)
      }
    })
    
    observe({
      req(input$errorType3_1)
      if(input$errorType3_1 %in% c("Additive", "AdditiveMultiplicative", "MixRatio")){
        updateNumericInput(session, inputId = "errorStDev3_1", value = 1)
      } else {
        updateNumericInput(session, inputId = "errorStDev3_1", value = .1)
      }
    })
    
    output$residEffect1 <- renderUI({
      if(is.null(errorNamesReactive$values$predNames)){
        return()
      } else {
          renderResidEffect(1, errorNamesReactive$values$predNames)
      }
    })
    
    #-------------Parameters > Structural Parameters ----------------------------------------------------------------
    
    output$structuralParamsNames <- renderUI({
      tagList(
        renderStParmName(model())
      )
    })
    
    
    output$structuralParamsStyle <- renderUI({
      choices <- c(`Product * exp(Eta)` = "LogNormal", `Sum * exp(Eta)` =  "LogNormal1", `exp(Sum + Eta)` = "LogNormal2", `ilogit(Sum + Eta)` = "LogitNormal", `Sum + Eta` = "Normal")
      st <- lapply(1:length(pkModelReactives$values$namesStParm), function(i) {
        tagList(
          # div(style = "padding: 4px;"),
          selectInput(inputId = paste0("strStyle", i), label = NULL, choices = names(choices)),
          # div(style = "padding: 11px;")
        )
      })
        st
    })
    
    output$structuralParamsFixEffName <- renderUI({
      st <- lapply(1:length(pkModelReactives$values$namesStParm), function(i) {
        tagList(
          # div(style = "padding:4px;"),
          fluidRow(
          textInput(inputId = paste0("strFixEffName", i), label = NULL, value = paste0("tv",pkModelReactives$values$namesStParm[[i]])) %>% shinyjs::disabled(),
          )
          # div(style = "padding:11px;")
        )
      })
        st
    })
    
    output$structuralParamsRanEff <- renderUI({
      if(pkModelInputs$values$isPopulation){
        initRanEffFalse <- c("CMixRatio", "CMultStdev", "C1MixRatio", "C1MultStdev", "A0MixRatio", "A0MultStdev", "EMultStdev", "EMixRatio")
        st <- lapply(1:length(pkModelReactives$values$namesStParm), function(i) {
          tagList(
            # div(style = "padding:7px;"),
            fluidRow(
              column(width = 12, 
                     checkboxInput(inputId = paste0("strRanEff", i),
                                   label = NULL,
                                   value = ifelse(pkModelReactives$values$namesStParm[[i]] %in% initRanEffFalse, FALSE, TRUE)))
            ),
            
            div(style = "padding:6.25px;")
            
          )
        })
          st
      } else {
        return()
      }
    })
    
    output$structuralParamsRanEffName <- renderUI({
      if(pkModelInputs$values$isPopulation){
        st <- lapply(1:length(pkModelReactives$values$namesStParm), function(i) {
          tagList(
            # div(style = "padding:4px;"),
            textInput(inputId = paste0("strRanEffName", i), 
                      label = NULL, 
                      value = paste0("n",pkModelReactives$values$namesStParm[[i]])) %>% shinyjs::disabled(),
            # div(style = "padding:11px;")
          )
        })
          st
      } else {
        return()
      }
    })
    
    observe({
      for(i in 1:15){
        if(is.null(input[[paste0("stParmName", i)]])){
          return()
        } else if(model()@pkModelAttrs@isPkFrozen && input[[paste0("stParmName", i)]] %in% pk_stparms){
          shinyjs::disable(id = paste0("strRanEff", i))
        } else if(model()@emaxModelAttrs@frozen && input[[paste0("stParmName", i)]] %in% emax_stparms){
          shinyjs::disable(id = paste0("strRanEff", i))
        } else if(model()@indirectModelAttrs@frozen && input[[paste0("stParmName", i)]] %in% indirect_stparms){
          shinyjs::disable(id = paste0("strRanEff", i))
        } else if(model()@isLinearFrozen && input[[paste0("stParmName", i)]] %in% linear_stparms){
          shinyjs::disable(id = paste0("strRanEff", i))
        } else {
          shinyjs::enable(id = paste0("strRanEff", i))
        }
      }
    })
    

    #----------------------------------------------------------------------------------------------------------------
    #------------ Parameters > Covariates ---------------------------------------------------------------------------
    
    ## Logic to initialize covariates from basemodel below: Do Not Remove
    # panels_on_startup <- 2
    # 
    # startupSelectedCov <- list(
    #   covSelect = c("BodyWeight", "Gender"),
    #   covType = c("Continuous", "Occasion"),
    #   covCenter = c("Mean", NULL),
    #   covCenterVal = c(),
    #   covIsPositive = c(TRUE, NULL),
    #   covLevels = c(NA, "0,1"),
    #   covLabels = c(NA, "Male, Female"),
    #   covDir = c("Forward", "Backward"),
    #   covEffects = c("V", "Cl"),
    #   occRanEffDiag = c(NULL, TRUE),
    #   valsOcc = list(cov1 = NULL, cov2 = c(3,2))
    # )
    # 
    # # add counters on startup
    # lapply(seq_len(panels_on_startup), function(i) {
    #   current_id <- paste0("panel_", counter_panels)
    #   cov_panel(current_id,  choicesAvail = names(data), effectsAvail = c("V", "Cl", "V2"),
    #             covSelect = startupSelectedCov$covSelect[i],
    #             covType = startupSelectedCov$covType[i],
    #             covCenter = startupSelectedCov$covCenter[i],
    #             covCenterVal = startupSelectedCov$covCenterVal[i],
    #             covIsPositive = startupSelectedCov$covIsPositive[i],
    #             covLevels = startupSelectedCov$covLevels[i],
    #             covHasLabels = startupSelectedCov$covHasLabels[i],
    #             covLabels = startupSelectedCov$covLabels[i],
    #             covDir = startupSelectedCov$covDir[i],
    #             covEffects = startupSelectedCov$covEffects[i],
    #             occRanEffDiag = startupSelectedCov$occRanEffDiag[i],
    #             valsOcc = startupSelectedCov$valsOcc[[i]]
    #   )
    #   insertUI(selector = "#add_panels_here",
    #             where = "afterBegin",
    #            ui = cov_panel_ui(current_id))
    # 
    #   # update counter
    #   counter_panels <<- counter_panels + 1
    # })
    ########################################################################
    
    
    reactiveCovChoices <- reactiveValues(values = list())

    #Filter choices of previously selected covariates in newly added covariates
    # observe({
    #   reactiveCovChoices$values$availChoices = setdiff(names(data), covpanelselect(input))
    # })
    
    assign("counter_panels", value = 1, envir = model_builder_env) 
           
    observeEvent(input$addCovPkBtn, {
      count <- get("counter_panels", envir = model_builder_env)
      
      current_id <- paste0("panel_", count)
      cov_panel(current_id,  effectsAvail = pkModelReactives$values$namesStParm, covSelect = input[[paste0(current_id, "-covSelect")]], isPopulation = pkModelInputs$values$isPopulation, session = session)
      insertUI(selector = "#add_panels_here",
               where = "afterBegin",
               ui = cov_panel_ui(current_id))
      
      # update counter
      assign("counter_panels", value = count + 1, envir = model_builder_env)
    })
    
    
    #Update direction choices for type = categorical/occasion
    observeEvent(c(input[["panel_1-covType"]], input[["panel_2-covType"]], input[["panel_3-covType"]], input[["panel_4-covType"]],input[["panel_5-covType"]], input[["panel_6-covType"]], input[["panel_7-covType"]], input[["panel_8-covType"]], input[["panel_9-covType"]], input[["panel_10-covType"]]),{
      for(i in 1:10){
        if(is.null(input[[paste0("panel_", i, "-covType")]]) || input[[paste0("panel_", i, "-covType")]] == ""){
          return()
        } else {
          if(input[[paste0("panel_", i, "-covType")]] %in% c("Categorical", "Occasion")){
            updateSelectInput(
              session, 
              inputId = paste0("panel_", i, "-covDir"),
              selected = "Forward",
              choices = c("Forward", "Backward"))
          } else {
            updateSelectInput(
              session, 
              inputId = paste0("panel_", i, "-covDir"),
              selected = input[[paste0("panel_", i, "-covDir")]],
              choices = c("Forward", "Backward", "Interpolate")
            )
          }
        }
      }
    })
    
    # Retain Below Code:  However, validation is now done inside covariate module
    # Show/Hide Labels according to data type for occasion/categorical covariate:
    # observe({
    #   for(i in 1:10){
    #     if(is.null(input[[paste0("panel_", i, "-covSelect")]]) || input[[paste0("panel_", i, "-covSelect")]] == "")
    #       next
    #     if(input[[paste0("panel_", i, "-covType")]] %in% c("Categorical", "Occasion")){
    #       if(class(data[, input[[paste0("panel_", i, "-covSelect")]]]) == "character"){
    #         show(id = paste0("panel_", i, "-covLabels"))
    #       } else {
    #         hide(id = paste0("panel_", i, "-covLabels"))
    #       }
    #     } else {
    #       hide(id = paste0("panel_", i, "-covLabels"))
    #     }
    #   }
    # })
    
   
    
    #----------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------
    #--------------------------------- Parameters > Fixed Effects ---------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------
    
    #issues: need to parse theta names from structural params and covariate eff list
    reactiveThetaNames <- reactiveValues(values = NULL)
    observe({
      reactiveThetaNames$values <- trimws(names(getThetaNames(model())))
    })
    
    output$fixedEffectNames <- renderUI({
      tagList(
        thetaNames <- renderThetaName(reactiveThetaNames$values)
      )
        thetaNames
    })

    # outputOptions(output, "fixedEffectNames", suspendWhenHidden = FALSE)
    # outputOptions(output, "fixedEffectInitialNumeric", suspendWhenHidden = FALSE)
    # outputOptions(output, "fixedEffectLowerNumeric", suspendWhenHidden = FALSE)
    # outputOptions(output, "fixedEffectUpperNumeric", suspendWhenHidden = FALSE)
    # outputOptions(output, "fixedEffectFreeze", suspendWhenHidden = FALSE)
    # outputOptions(output, "fixedEffectUnits", suspendWhenHidden = FALSE)
    
    output$fixedEffectInitialNumeric <- renderUI({
      fenumeric <- lapply(1:length(reactiveThetaNames$values), function(i) {
        
        tagList(
          textInput(
            paste0("feInitialNumeric", i), 
            value = ifelse(grepl("^d", reactiveThetaNames$values[[i]]), 0, 1), 
            label = NULL),
        )
      })
        fenumeric
    })
    
     observe({
      for(i in 1:15){
        if(is.null(input[[paste0("feFreeze", i)]])){
          return()
        } else if(input[[paste0("feFreeze", i)]] == TRUE){
          shinyjs::disable(id = paste0("feUpperNumeric", i))
          shinyjs::disable(id = paste0("feLowerNumeric", i))
        } else {
          shinyjs::enable(id = paste0("feUpperNumeric", i))
          shinyjs::enable(id = paste0("feLowerNumeric", i))
        }
      }
    })
    
    
    output$fixedEffectLowerNumeric <- renderUI({
      fenumericlower <- lapply(1:length(reactiveThetaNames$values), function(i) {
        tagList(
          textInput(paste0("feLowerNumeric", i), value = "", label = NULL)
        )
      })
        fenumericlower
    })
    
    output$fixedEffectUpperNumeric <- renderUI({
      fenumericupper <- lapply(1:length(reactiveThetaNames$values), function(i) {
        tagList(
          textInput(paste0("feUpperNumeric", i), value = "", label = NULL)
        )
      })
        fenumericupper
    })
    
    output$fixedEffectFreeze <- renderUI({
      fefreeze <- lapply(1:length(reactiveThetaNames$values), function(i) {
        tagList(
          checkboxInput(inputId = paste0("feFreeze", i), value = FALSE, label = NULL),
          div(style = "padding:6.25px;"),
        )
      })
        fefreeze
    })
    
    output$fixedEffectUnits <- renderUI({
      feunits <- lapply(1:length(reactiveThetaNames$values), function(i) {
        tagList(
          textInput(paste0("feUnits", i), value = NULL, label = NULL)
        )
      })
        feunits
    })
    
    #Frozen observer
    reactiveFrozen <- reactiveValues(values = 
                                       list(
                                         pk = FALSE,
                                         emax = FALSE,
                                         indirect = FALSE,
                                         linear = FALSE)
    )
    
    observe({
      req(model())
      if(model()@pkModelAttrs@isPkFrozen){
        reactiveFrozen$values$pk = TRUE
      } else {
        reactiveFrozen$values$pk = FALSE
      }
      if(model()@emaxModelAttrs@frozen){
        reactiveFrozen$values$emax = TRUE
      } else {
        reactiveFrozen$values$emax = FALSE
      }
      if(model()@indirectModelAttrs@frozen){
        reactiveFrozen$values$indirect = TRUE
      } else {
        reactiveFrozen$values$indirect = FALSE
      }
      if(model()@isLinearFrozen){
        reactiveFrozen$values$linear = TRUE
      } else {
        reactiveFrozen$values$linear = FALSE
      }
    })
    
    observe({
      for(i in 1:15){
        if(is.null(input[[paste0("nameTheta", i)]])){
          return()
        } else if(model()@pkModelAttrs@isPkFrozen && input[[paste0("nameTheta", i)]] %in% paste0("tv",pk_stparms)){
          shinyjs::disable(id = paste0("feFreeze", i))
        } else if(model()@emaxModelAttrs@frozen && input[[paste0("nameTheta", i)]] %in% paste0("tv",emax_stparms)){
          shinyjs::disable(id = paste0("feFreeze", i))
        } else if(model()@indirectModelAttrs@frozen && input[[paste0("nameTheta", i)]] %in% paste0("tv",indirect_stparms)){
          shinyjs::disable(id = paste0("feFreeze", i))
        } else if(model()@isLinearFrozen && input[[paste0("nameTheta", i)]] %in% paste0("tv",linear_stparms)){
          shinyjs::disable(id = paste0("feFreeze", i))
        } else {
          shinyjs::enable(id = paste0("feFreeze", i))
        }
      }
    })
    
    #Fixed Effects range validation
    fe_range_validation <- reactive({
      req(input$feInitialNumeric1)
      thetaInitial <- vector(mode = "numeric", length = length(reactiveThetaNames$values))
      for(i in seq_along(reactiveThetaNames$values)){
        thetaInitial[[i]] <- as.numeric(input[[paste0("feInitialNumeric", i)]])
      }
      
      thetaLower <- vector(mode = "numeric", length = length(reactiveThetaNames$values))
      for(i in seq_along(reactiveThetaNames$values)){
        thetaLower[[i]] <- as.numeric(input[[paste0("feLowerNumeric", i)]])
      }
      
      thetaLower[is.na(thetaLower)] <- -Inf

      thetaUpper <- vector(mode = "numeric", length = length(reactiveThetaNames$values))
      for(i in seq_along(reactiveThetaNames$values)){
        thetaUpper[[i]] <- as.numeric(input[[paste0("feUpperNumeric", i)]])
      }
      
      thetaUpper[is.na(thetaUpper)] <- Inf

      validate(
        need(all(thetaInitial > thetaLower) && all(thetaInitial < thetaUpper), "Error: Initial value must be within the specified lower/upper range")
      )
    })
    
    output$feRangeValidation <- renderPrint({
      fe_range_validation()
    })
    
    
    #----------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------
    #-------------------------------- Parameters > Random Effects ---------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------
    
    reactiveRanEffNames <- reactiveValues(values = NULL)
    
    observe({
      req(model())
      reactiveRanEffNames$values <- getRandomEffectNames(model())
    }, priority = 0)
    
    output$ranEffSelections1 <- renderUI({
      runjs(paste0("Shiny.onInputChange('", "ranEffSelections2', null)"))
      
      renderStrRanEffSelections(id = 1, effects = reactiveRanEffNames$values)
    })
    
    output$ranEffMatrix1 <- renderUI({
      req(input$ranEffSelections1)
      if(is.null(input$ranEffSelections1)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        tagList(
          bslib::card(class = "matrix-card",
            bslib::card_title(paste0("Covariance Matrix: ", paste0(input$ranEffSelections1, collapse = ", "))),
            bslib::card_body(class = "ran-eff-matrix-card-body",
                             renderStrRanEffMatrix(id = 1, effects = input$ranEffSelections1, isDiagonal = input$diagonalRanEff1))
          ),
          div(style = "padding: 20px;")
        )
      }
    })
    
    output$ranEffSelections2 <- renderUI({
      req(input$ranEffSelections1)
      runjs(paste0("Shiny.onInputChange('", "ranEffSelections3', null)"))
      
      if(is.null(input$ranEffSelections1)) return(NULL)
      if(pkModelInputs$values$isPopulation){
      if(length(reactiveRanEffNames$values) == length(input$ranEffSelections1)){
        return(NULL)
      }
        
        if(!(any(input$ranEffSelections1 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
        
        renderStrRanEffSelections(id = 2, effects = setdiff(reactiveRanEffNames$values,input$ranEffSelections1))
      }
    })
    
    output$ranEffMatrix2 <- renderUI({
      req(input$ranEffSelections2)
      if(is.null(input$ranEffSelections2)) return(NULL)
      

      if(pkModelInputs$values$isPopulation){
      if(length(reactiveRanEffNames$values) == length(input$ranEffSelections1)){
        return(NULL)
      }
        if(!(any(input$ranEffSelections2 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
        tagList(
          bslib::card(
            bslib::card_title(paste0("Covariance Matrix: ", paste0(input$ranEffSelections2, collapse = ", "))),
            bslib::card_body(class = "ran-eff-matrix-card-body",
                             renderStrRanEffMatrix(id = 2, effects = input$ranEffSelections2, isDiagonal = input$diagonalRanEff2))
          ),
          div(style = "padding: 20px;")
        )
      }
    })
    
    output$ranEffSelections3 <- renderUI({
      req(input$ranEffSelections2)
      runjs(paste0("Shiny.onInputChange('", "ranEffSelections4', null)"))
      
      if(is.null(input$ranEffSelections2)) return(NULL)
      

      if(pkModelInputs$values$isPopulation){
      if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2))){
        return(NULL)
      }
        if(!(any(input$ranEffSelections2 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
        renderStrRanEffSelections(id = 3, effects = setdiff(reactiveRanEffNames$values,c(input$ranEffSelections1, input$ranEffSelections2)))
      }
    })
    
    output$ranEffMatrix3 <- renderUI({
      req(input$ranEffSelections3)
      
      if(is.null(input$ranEffSelections3)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections3 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
        tagList(
          bslib::card(
            bslib::card_title(paste0("Covariance Matrix: ", paste0(input$ranEffSelections3, collapse = ", "))),
            bslib::card_body(class = "ran-eff-matrix-card-body",
                             renderStrRanEffMatrix(id = 3, effects = input$ranEffSelections3, isDiagonal = input$diagonalRanEff3))
          ),
          div(style = "padding: 20px;")
        )
      }
    })
    
    output$ranEffSelections4 <- renderUI({
      req(input$ranEffSelections3)
      runjs(paste0("Shiny.onInputChange('", "ranEffSelections5', null)"))
      
      if(is.null(input$ranEffSelections3)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections3 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
          renderStrRanEffSelections(id = 4, effects = setdiff(reactiveRanEffNames$values,c(input$ranEffSelections1, input$ranEffSelections2, input$ranEffSelections3)))
      }
    })
    
    output$ranEffMatrix4 <- renderUI({
      req(input$ranEffSelections4)
      
      if(is.null(input$ranEffSelections4)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections4 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
          tagList(
            bslib::card(
              bslib::card_title(paste0("Covariance Matrix: ", paste0(input$ranEffSelections4, collapse = ", "))),
              bslib::card_body(class = "ran-eff-matrix-card-body",
                               renderStrRanEffMatrix(id = 4, effects = input$ranEffSelections4, isDiagonal = input$diagonalRanEff4))
            ),
            div(style = "padding: 20px;")
          )
      }
    })
    
    output$ranEffSelections5 <- renderUI({
      req(input$ranEffSelections4)
      runjs(paste0("Shiny.onInputChange('", "ranEffSelections6', null)"))
      
      if(is.null(input$ranEffSelections4)){
        return(NULL)
      } 
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections4 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
        
          renderStrRanEffSelections(id = 5, effects = setdiff(reactiveRanEffNames$values,c(input$ranEffSelections1, input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4)))
      }
    })
    
    output$ranEffMatrix5 <- renderUI({
      req(input$ranEffSelections5)
      if(is.null(input$ranEffSelections4)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections5 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
          tagList(
            bslib::card(
              bslib::card_title(paste0("Covariance Matrix: ", paste0(input$ranEffSelections5, collapse = ", "))),
              bslib::card_body(class = "ran-eff-matrix-card-body",
                               renderStrRanEffMatrix(id = 5, effects = input$ranEffSelections5, isDiagonal = input$diagonalRanEff5))
            ),
            div(style = "padding: 20px;")
          )
      }
    })
    
    output$ranEffSelections6 <- renderUI({
      req(input$ranEffSelections5)
      runjs(paste0("Shiny.onInputChange('", "ranEffSelections7', null)"))
      
      if(is.null(input$ranEffSelections5)){
        return(NULL)
      } 
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections5 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
          renderStrRanEffSelections(id = 6, effects = setdiff(reactiveRanEffNames$values,c(input$ranEffSelections1, input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5)))
      }
    })
    
    output$ranEffMatrix6 <- renderUI({
      req(input$ranEffSelections6)
      if(is.null(input$ranEffSelections6)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections6 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
          tagList(
            bslib::card(
              bslib::card_title(paste0("Covariance Matrix: ", paste0(input$ranEffSelections6, collapse = ", "))),
              bslib::card_body(class = "ran-eff-matrix-card-body",
                               renderStrRanEffMatrix(id = 6, effects = input$ranEffSelections6, isDiagonal = input$diagonalRanEff6))
            ),
            div(style = "padding: 20px;")
          )
      }
    })
    
    output$ranEffSelections7 <- renderUI({
      req(input$ranEffSelections6)
      runjs(paste0("Shiny.onInputChange('", "ranEffSelections8', null)"))
      
      if(is.null(input$ranEffSelections6)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5, input$ranEffSelections6))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections6 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
          renderStrRanEffSelections(id = 7, effects = setdiff(reactiveRanEffNames$values,c(input$ranEffSelections1, input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5, input$ranEffSelections6)))
      }
    })
    
    output$ranEffMatrix7 <- renderUI({
      req(input$ranEffSelections7)
      if(is.null(input$ranEffSelections7)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5, input$ranEffSelections6))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections7 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
          tagList(
            bslib::card(
              bslib::card_title(paste0("Covariance Matrix: ", paste0(input$ranEffSelections7, collapse = ", "))),
              bslib::card_body(class = "ran-eff-matrix-card-body",
                               renderStrRanEffMatrix(id = 7, effects = input$ranEffSelections7, isDiagonal = input$diagonalRanEff7))
            ),
            div(style = "padding: 20px;")
          )
      }
    })
    
    output$ranEffSelections8 <- renderUI({
      req(input$ranEffSelections7)
      runjs(paste0("Shiny.onInputChange('", "ranEffSelections9', null)"))
      
      if(is.null(input$ranEffSelections7)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5, input$ranEffSelections6, input$ranEffSelections7))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections7 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
          renderStrRanEffSelections(id = 8, effects = setdiff(reactiveRanEffNames$values,c(input$ranEffSelections1, input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5, input$ranEffSelections6, input$ranEffSelections7)))
      }
    })
    
    output$ranEffMatrix8 <- renderUI({
      req(input$ranEffSelections8)
      if(is.null(input$ranEffSelections7)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5, input$ranEffSelections6, input$ranEffSelections7))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections8 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
          tagList(
            bslib::card(
              bslib::card_title(paste0("Covariance Matrix: ", paste0(input$ranEffSelections8, collapse = ", "))),
              bslib::card_body(class = "ran-eff-matrix-card-body",
                               renderStrRanEffMatrix(id = 8, effects = input$ranEffSelections8, isDiagonal = input$diagonalRanEff8))
            ),
            div(style = "padding: 20px;")
          )
      }
    })
    
    output$ranEffSelections9 <- renderUI({
      req(input$ranEffSelections8)
      runjs(paste0("Shiny.onInputChange('", "ranEffSelections10', null)"))
      
      if(is.null(input$ranEffSelections8)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5, input$ranEffSelections6, input$ranEffSelections7, input$ranEffSelections8))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections8 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
          renderStrRanEffSelections(id = 9, effects = setdiff(reactiveRanEffNames$values,c(input$ranEffSelections1, input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5, input$ranEffSelections6, input$ranEffSelections7, input$ranEffSelections8)))
      }
    })
    
    output$ranEffMatrix9 <- renderUI({
      req(input$ranEffSelections9)
      if(is.null(input$ranEffSelections8)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5, input$ranEffSelections6, input$ranEffSelections7, input$ranEffSelections8))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections9 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
          tagList(
            bslib::card(
              bslib::card_title(paste0("Covariance Matrix: ", paste0(input$ranEffSelections9, collapse = ", "))),
              bslib::card_body(class = "ran-eff-matrix-card-body",
                               renderStrRanEffMatrix(id = 9, effects = input$ranEffSelections9, isDiagonal = input$diagonalRanEff9))
            ),
            div(style = "padding: 20px;")
          )
      }
    })
    
    output$ranEffSelections10 <- renderUI({
      req(input$ranEffSelections9)
      if(is.null(input$ranEffSelections9)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5, input$ranEffSelections6, input$ranEffSelections7, input$ranEffSelections8, input$ranEffSelections9))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections9 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
          renderStrRanEffSelections(id = 10, effects = setdiff(reactiveRanEffNames$values,c(input$ranEffSelections1, input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5, input$ranEffSelections6, input$ranEffSelections7, input$ranEffSelections8, input$ranEffSelections9)))
      }
    })
    
    output$ranEffMatrix10 <- renderUI({
      req(input$ranEffSelections10)
      if(is.null(input$ranEffSelections10)) return(NULL)
      
      if(pkModelInputs$values$isPopulation){
        if(length(reactiveRanEffNames$values) == length(c(input$ranEffSelections1,input$ranEffSelections2, input$ranEffSelections3,  input$ranEffSelections4, input$ranEffSelections5, input$ranEffSelections6, input$ranEffSelections7, input$ranEffSelections8, input$ranEffSelections9))){
          return(NULL)
        }
        if(!(any(input$ranEffSelections10 %in% reactiveRanEffNames$values))){
          return(NULL)
        }
          tagList(
            bslib::card(
              bslib::card_title(paste0("Covariance Matrix: ", paste0(input$ranEffSelections10, collapse = ", "))),
              bslib::card_body(class = "ran-eff-ran-eff-matrix-card-body",
                               renderStrRanEffMatrix(id = 10, effects = input$ranEffSelections10, isDiagonal = input$diagonalRanEff))
            ),
            div(style = "padding: 20px;")
          )
      }
    })
    
    # outputOptions(output, "ranEffSelections1", priority = 1, suspendWhenHidden = FALSE)
    # outputOptions(output, "ranEffSelections2", priority = 2, suspendWhenHidden = FALSE)
    # outputOptions(output, "ranEffSelections3", priority = 3, suspendWhenHidden = FALSE)
    # outputOptions(output, "ranEffSelections4", priority = 4, suspendWhenHidden = FALSE)
    # outputOptions(output, "ranEffSelections5", priority = 5, suspendWhenHidden = FALSE)
    # outputOptions(output, "ranEffSelections6", priority = 6, suspendWhenHidden = FALSE)
    # outputOptions(output, "ranEffSelections7", priority = 7, suspendWhenHidden = FALSE)
    # outputOptions(output, "ranEffSelections8", priority = 8, suspendWhenHidden = FALSE)
    # outputOptions(output, "ranEffSelections9", priority = 9, suspendWhenHidden = FALSE)
    # outputOptions(output, "ranEffSelections10", priority = 10, suspendWhenHidden = FALSE)
    
    
    
    
    
    #----------------------------------------------------------------------------------------------------------------
    # Input Options ----
    #----------------------------------------------------------------------------------------------------------------
    #SS Col Def and SS Dose Cycle Observer
    observe({
    if(input$isSS == TRUE){
      shinyjs::disable(id = "isSSDoseCycle")
    } else {
      shinyjs::enable(id = "isSSDoseCycle")
    }

    if(input$isSSDoseCycle == TRUE){
      shinyjs::disable(id = "isSS")
    } else {
      shinyjs::enable(id = "isSS")
    }
    })

    #ADDL Col Def and ADDL Dose Cycle Observer
    observe({
      if(input$isADDL == TRUE){
        shinyjs::disable(id = "isADDLDoseCycle")
      } else {
        shinyjs::enable(id = "isADDLDoseCycle")
      }

      if(input$isADDLDoseCycle == TRUE){
        shinyjs::disable(id = "isADDL")
      } else {
        shinyjs::enable(id = "isADDL")
      }
    })
    # #----------------------------------------------------------------------------------------------------------------
    # # Steady State ----
    # #----------------------------------------------------------------------------------------------------------------
    # 
    reactiveDoseNames <- reactiveValues(value = c())

    observe({
      reactiveDoseNames$value <- doseNames(model())
    })

    assign("counter_panels_ss", value = 1, envir = model_builder_env)

    observeEvent(input$addSSBtn, {
      count_ss <- get("counter_panels_ss", envir = model_builder_env)
      
      current_id_ss <- paste0("ss_panel_", count_ss)
      ss_panel(current_id_ss, doseName = reactive(reactiveDoseNames$value), data = data)
      insertUI(selector = "#add_SS_here",
               where = "beforeBegin",
               ui = ss_panel_ui(current_id_ss))

      # update counter
      assign("counter_panels_ss", value = count_ss + 1, envir = model_builder_env)
    })

    #Total Cycle Time Output - SS

    output$timeTotalSS <- renderPrint({
        posSSIISelect <- grep(pattern = "ssIIConstant", names(input))
        
       ii_num_panel_inputs <- c()
        for(i in posSSIISelect){
          inputname <- names(input)[[i]]
          panel <- sub("-.*", "", inputname)
          if(input[[paste0(panel, "-ssIIType")]] == "Constant"){
          ii_num_panel_inputs <- c(ii_num_panel_inputs, inputname)
          } else {
            next
          }
        }
       
       ii_num_panel_inputs <- sort(ii_num_panel_inputs)
       
       numeric_sum <- c()
        for(i in seq_along(ii_num_panel_inputs)){
          n <- ii_num_panel_inputs[[i]]
          if(!is.null(input[[n]])){
            numeric_sum <- c(numeric_sum, input[[n]])
          }
        }
       
       numeric_sum <- sum(numeric_sum)
       
       if(numeric_sum == 0){
         numeric_sum <- NULL
       }
       
       posSSIIColSelect <- grep(pattern = "ssIIColumn", names(input))
       
       ii_col_panel_inputs <- c()
       for(i in posSSIIColSelect){
         inputname <- names(input)[[i]]
         panel <- sub("-.*", "", inputname)
         if(input[[paste0(panel, "-ssIIType")]] == "Column"){
         ii_col_panel_inputs <- c(ii_col_panel_inputs, inputname)
         } else {
           next
         }
       }
       
       ii_col_panel_inputs <- sort(ii_col_panel_inputs)
       
       
       col_sum <- c()
       for(i in seq_along(ii_col_panel_inputs)){
         n <- ii_col_panel_inputs[[i]]
         if(!is.null(input[[n]])){
           col_sum <- c(col_sum, input[[n]])
         }
       }
       
       if(length(numeric_sum) == 0 && length(col_sum) == 0){
         text_out <- NULL
       } else if(length(numeric_sum) > 0 && length(col_sum) == 0) {
         text_out <- paste0("Cycle Time = ", numeric_sum)
       } else if(length(numeric_sum) == 0 && length(col_sum) > 0){
         text_out <- paste0("Cycle Time = ",  paste0(col_sum, collapse = " + "))
       } else {
         text_out <- paste0("Cycle Time = ", numeric_sum, " + ", paste0(col_sum, collapse = " + "))
       }
       cat(text_out)
    })

    # #----------------------------------------------------------------------------------------------------------------
    # # ADDL ----
    # #----------------------------------------------------------------------------------------------------------------
    
    assign("counter_panels_addl", value = 1, envir = model_builder_env)

    observeEvent(input$addADDLBtn, {
      count_addl <- get("counter_panels_addl", envir = model_builder_env)
      
      current_id_addl <- paste0("addl_panel_", count_addl)
      addl_panel(current_id_addl, doseName = reactive(reactiveDoseNames$value), data = data)
      insertUI(selector = "#add_ADDL_here",
               where = "beforeBegin",
               ui = addl_panel_ui(current_id_addl))

      # update counter
      assign("counter_panels_addl", value = count_addl + 1, envir = model_builder_env)
    })

    #Total Cycle Time Output - SS

    output$timeTotalADDL <- renderPrint({

      posADDLIISelect <- grep(pattern = "addlIIConstant", names(input))
      
      ii_num_panel_inputs <- c()
      for(i in posADDLIISelect){
        inputname <- names(input)[[i]]
        panel <- sub("-.*", "", inputname)
        if(input[[paste0(panel, "-addlIIType")]] == "Constant"){
          ii_num_panel_inputs <- c(ii_num_panel_inputs, inputname)
        } else {
          next
        }
      }
      ii_num_panel_inputs <- sort(ii_num_panel_inputs)
      
      numeric_sum <- c()
      for(i in seq_along(ii_num_panel_inputs)){
        n <- ii_num_panel_inputs[[i]]
        if(!is.null(input[[n]])){
          numeric_sum <- c(numeric_sum, input[[n]])
        }
      }
      
      numeric_sum <- sum(numeric_sum)
      
      if(numeric_sum == 0){
        numeric_sum <- NULL
      }
      
      posADDLIIColSelect <- grep(pattern = "addlIIColumn", names(input))
      
      ii_col_panel_inputs <- c()
      for(i in posADDLIIColSelect){
        inputname <- names(input)[[i]]
        panel <- sub("-.*", "", inputname)
        if(input[[paste0(panel, "-addlIIType")]] == "Column"){
          ii_col_panel_inputs <- c(ii_col_panel_inputs, inputname)
        } else {
          next
        }
      }
      ii_col_panel_inputs <- sort(ii_col_panel_inputs)
      
      col_sum <- c()
      for(i in seq_along(ii_col_panel_inputs)){
        n <- ii_col_panel_inputs[[i]]
        if(!is.null(input[[n]])){
          col_sum <- c(col_sum, input[[n]])
        }
      }
      
      if(length(numeric_sum) == 0 && length(col_sum) == 0){
        text_out <- NULL
      } else if(length(numeric_sum) > 0 && length(col_sum) == 0) {
        text_out <- paste0("Cycle Time = ", numeric_sum)
      } else if(length(numeric_sum) == 0 && length(col_sum) > 0){
        text_out <- paste0("Cycle Time = ",  paste0(col_sum, collapse = " + "))
    } else {
      text_out <- paste0("Cycle Time = ", numeric_sum, " + ", paste0(col_sum, collapse = " + "))
    }
      cat(text_out)
    })
    #----------------------------------------------------------------------------------------------------------------
    # Column Mapping ----
    #----------------------------------------------------------------------------------------------------------------
    # 
    #----------------------------------------------------------------------------------------------------------------
    # ADDL and SS (Column Definitions) ----
    #----------------------------------------------------------------------------------------------------------------
    output$columnDoseMapping <- renderUI({
      
      if(input$reset == TRUE){
        uiDoseMappingReset <- tagList(
          selectInput(inputId = "resetSelectDose", label = "Reset *", choices = c(" ", names(data)), width = "100%"),
          div(style = "padding: 3px;")
        )
      } else {
        uiDoseMappingReset <- NULL
      }
      if(input$mdv == TRUE){
        uiDoseMappingMDV <- tagList(
          selectInput(inputId = "mdvSelectDose", label = "MDV *", choices = c(" ", names(data)), width = "100%"),
          div(style = "padding: 3px;")
          )
      } else {
        uiDoseMappingMDV <- NULL
      }
        
     if(input$isSS == TRUE){
       if(input$isSSOffset == TRUE){
        uiDoseMappingSS <- tagList(
          selectInput(inputId = "ssSelectDose", label = "SteadyState *", choices = c(" ", names(data)), width = "100%"),
          div(style = "padding: 3px;"),
          selectInput(inputId = "ssOffsetSelectDose", label = "SSOffset *", choices = c(" ", names(data)), width = "100%"),
          div(style = "padding: 3px;")
         )
       } else {
         uiDoseMappingSS <- tagList(
           selectInput(inputId = "ssSelectDose", label = "SteadyState *", choices = c(" ", names(data)), width = "100%"),
           div(style = "padding: 3px;")
         )
       }
     } else {
       uiDoseMappingSS <- NULL
     }
      
      if(input$isADDL == TRUE){
        uiDoseMappingADDL <- tagList(
          selectInput(inputId = "addlSelectDose", label = "ADDL *", choices = c(" ", names(data)), width = "100%"),
          div(style = "padding: 3px;")
        )
      } else {
        uiDoseMappingADDL <- NULL
      }
      
      if(input$isADDL || input$isSS){
      uiDoseMappingII <- tagList(
        selectInput(inputId = "iiSelectDose", label = "II *", choices = c(" ", names(data)), width = "100%")
      )
      } else {
        uiDoseMappingII <- NULL
      }
      
      tagList(c(uiDoseMappingReset, uiDoseMappingMDV, uiDoseMappingSS, uiDoseMappingADDL, uiDoseMappingII))
    })
    
    reactiveModelVarNames <- reactiveValues(values = NULL)
    
    observe({
      if(length(model()@columnMapping@mapping) > 0){
      reactiveModelVarNames$values <- names(model()@columnMapping@mapping)
      }
    })
    
    output$columnMap <- renderUI({
      if(any(grepl('\\BQL$', reactiveModelVarNames$values))){
        noreqcols <- c("A1Strip")
      } else {
        noreqcols <- c("A1Strip", "CObs", "C1Obs", "A0Obs", "EObs")
      }
      uiColMapDropdowns <- vector(mode = "list", length = length(reactiveModelVarNames$values))
      for(i in seq_along(uiColMapDropdowns)){
        uiColMapDropdowns[[i]] <- tagList(
          selectInput(
            inputId = paste0("col", i, "_1"),
            label =  ifelse(
              reactiveModelVarNames$values[[i]] %in% noreqcols,
              reactiveModelVarNames$values[[i]],
              paste0(reactiveModelVarNames$values[[i]], " *")
            ),
            selected = " ",
            width = "100%",
            choices =
              if (reactiveModelVarNames$values[[i]] == "id") {
                names(data)
              } else {
                c(" ", names(data))
              },
            multiple = if (reactiveModelVarNames$values[[i]] == "id")
              TRUE
            else
              FALSE
            
          ), 
          div(style = "padding: 3px;")
        )
      }
      uiColMapDropdowns
    })
    
    
    col_id_length_validation <- reactive({
      validate(
        need(length(input$col1_1) < 6, "Error: Cannot specify more than 5 column mappings for 'id'")
      )
    })
    
    output$colValidationIDLength <- renderPrint({
      col_id_length_validation()
    })
    
    reactive_col_validation_dup <- reactive({
      posCol <- grep(pattern = "^col", names(input))
      cols <- vector(mode = "list", length = length(posCol))
      
      colnames <- model()@columnMapping@mapping
      for(i in seq_along(colnames)){
        cols[[i]] <- input[[paste0("col", i, "_1")]]
      }
      
      cols <- unlist(cols)
      cols <- cols[cols != " "]
      
      if(input$reset){
        colReset <- input$resetSelectDose
      } else {
        colReset <- NULL
      }
      
      if(input$mdv){
        colMDV <- input$mdvSelectDose
      } else {
        colMDV <- NULL
      }
      
      if(input$isADDL){
        colADDL <- input$addlSelectDose
      } else {
        colADDL <- NULL
      }
      
      if(input$isSS){
        colSS <- input$ssSelectDose
        if(input$isSSOffset){
          colSSOffset <- input$ssOffsetSelectDose
        } else {
          colSSOffset <- NULL
        }
      } else {
        colSS <- NULL
        colSSOffset <- NULL
      }
      
      if(input$isSS || input$isADDL){
        colII <- input$iiSelectDose
      } else {
        colII <- NULL
      }
      
      extradefs <- c(colII, colSS, colSSOffset, colADDL, colMDV, colReset)

      extradefs <- extradefs[extradefs != " "]
      
      validate(
        need(!any(duplicated(c(cols, extradefs))), "Error: Duplicated column selections")
      )
      
    })
    
    output$colValidationDup <- renderPrint({
        reactive_col_validation_dup()
    })

    observeEvent(input$updateColsOut,{
      shinyAce::updateAceEditor(
        session,
        "colsout",
        theme = input$theme,
        mode = "r",
        tabSize = 4,
        useSoftTabs = FALSE,
        showInvisibles = FALSE,
        showLineNumbers = TRUE,
        value = paste0(c(unlist(writeColsOut(model())), ""), collapse = "\n"))
    })
    
    assign("counter_panels_extradef", value = 1, envir = model_builder_env)
    
    observeEvent(input$addExtraDefBtn, {
      count_extradef <- get("counter_panels_extradef", envir = model_builder_env)
      
      current_id_extradef <- paste0("extradef_panel_", count_extradef)
      extradef_panel(current_id_extradef)
      insertUI(selector = "#add_extradef_here",
               where = "afterBegin",
               ui = extradef_panel_ui(current_id_extradef))
      
      # update counter
      assign("counter_panels_extradef", value = count_extradef + 1, envir = model_builder_env)
    })
    

    
    #----------------------------------------------------------------------------------------------------------------
    # User Data ----
    #----------------------------------------------------------------------------------------------------------------
    
    output$userData <- DT::renderDataTable(options = list(scrollX = TRUE),{
      data
    })
    
    
    #----------------------------------------------------------------------------------------------------------------
    # R Code Generation ----
    #----------------------------------------------------------------------------------------------------------------
    
    observeEvent(input$generate_code, {
      
      code <-  expandChain(
        model()
      )
      
      shinyAce::updateAceEditor(
        session,
        "rsnlme",
        theme = input$theme,
        mode = "r",
        tabSize = 4,
        useSoftTabs = FALSE,
        showInvisibles = FALSE,
        showLineNumbers = TRUE,
        value = paste0(c(
          "library(Certara.RsNLME)",
          unlist(remove_working_dir(replace_data_arg(
            formatCode(code), dataName
          ))),
          ""
        ), collapse = "\n")
      )

    }, ignoreInit = TRUE)
    
    
    
    # updating ace with options
    observe({
      shinyAce::updateAceEditor(
        session,
        "ace",
        theme = input$theme,
        mode = "pml",
        tabSize = 4,
        useSoftTabs = FALSE,
        showInvisibles = FALSE,
        showLineNumbers = TRUE,
        value = paste0(c(unlist(model()@statements), ""), collapse = "\n")
      )
    })
    
    # observeEvent(input$ace, {
    #   writeLines(input$ace, con = pmlfile_out)
    #   if (syntaxcheck) {
    #     tdllog <- .check_syntax(pmlfile_out, TDLLocation)
    #     
    #     if (all(nchar(tdllog) == 0)) {
    #       tdllog <- "No warnings or errors"
    #       warningcolor <- "#235C30"
    #     } else {
    #       warningcolor <- "#FF0000"
    #     }
    #     
    #     output$tdllog <-
    #       renderUI(HTML(paste0('<pre><font color=\"', warningcolor, '\">', tdllog, "</font></pre>")))
    #   }
    # })
    
    #----------------------------------------------------------------------------------------------------------------
    # Exit Behavior ----
    #----------------------------------------------------------------------------------------------------------------
    observeEvent(input$exitShiny, {
      showModal(
        modalDialog(
          size = "m",
          title = "Exit Model Builder",
          easyClose = TRUE,
          
          div(
            div(style = "display: inline-block;",
                actionButton("exitConfirm", "Save & Exit")),
            div(style = "display: inline-block;",
                actionButton("exitCancel", "Exit"))
          ),
          
          footer = NULL
        )
      )
    })
    
    observeEvent(input$exitConfirm, {
      message("Shiny session has ended, the resulted model is returned.")
      session$sendCustomMessage(type = "shinymaterialJS", js$closewindow())
      stopApp(isolate({model()}))
    })
    
    observeEvent(input$exitCancel, {
      message("Shiny session has been canceled by the user, no model has been returned.")
      session$sendCustomMessage(type = "shinymaterialJS", js$closewindow())
      stopApp(baseModel)
    })
    
    session$onSessionEnded(function() {
      stopApp(baseModel)
    })
  }
  
  
  
  # UI ----
  ui <- tagList(
    ## 1.0 ShinyJS ----
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      text = jsFunctions,
      functions = c("closewindow")
    ),
    
    ## 2.0 Page ----
    bslib::page_fluid(class = "certara-page", 
      tags$head(tags$style(styleCSS)),
      title = "Model Builder",
      
      ## 3.0 Header ----
      certara_header(header_title = "Model Builder"),
    
      div(class = "model-content",
        bslib::layout_column_wrap(
          width = NULL,
          style = htmltools::css(grid_template_columns = "2fr 1fr"),
          bslib::navset_card_underline(
            ## 4.0 Content Window ----
            ### 4.1 Model Tab ----
          bslib::nav_panel(
            title = "Model",
            value = "side_nav_tab_model",
            bslib::card_body(
                #### 4.1a Model Type/Name Bar ----
              fluidRow(class = "multi-input-with-checkbox",
                column(width = 3, 
                       selectInput(
                       inputId = "modeltype",
                       label = "Model Type",
                       choices = c("PK", "Emax", "PK/Emax", "PK/Indirect", "Linear", "PK/Linear")
                       )
                ),
                column(width = 3, 
                       textInput(inputId = "modelNameInput", label = "Model Name", value = modelName) %>% shinyjs::disabled()
                ),
                column(class = "col-checkbox",
                       width = 3,
                       checkboxInput(inputId = "pk_switch_population", label = "Population", value = TRUE)
                )
              ), 
                                      
                #### 4.1b Main Body ----
              bslib::card(
                  ##### Model = Linear ----
                conditionalPanel("input.modeltype == 'Linear'",
                                 htmltools::h4("Linear Structural Model"),
                                 fluidRow(
                                          column(width = 3,
                                                 selectInput(
                                                 inputId = "l_type",
                                                 label = "Type",
                                                 choices = c("Constant", "Linear", "Quadratic")
                                                 )
                                          )
                                 )
                ),
                                                    
                  ##### Model != Emax or Linear ----
                conditionalPanel("input.modeltype != 'Emax' && input.modeltype != 'Linear'",
                  htmltools::h4("PK Structural Model"),
                  fluidRow(class = "multi-input-with-checkbox",
                           column(width = 3, 
                                  selectInput(
                                    inputId = "pk_num_comp", 
                                    label = "Number of Compartments", 
                                    choices = c(1:3), selected = 1
                                  )
                           ),
                           column(width = 3, 
                                  selectInput(
                                    inputId = "pk_elimination", 
                                    label = "Elimination", 
                                    choices = c("Linear", "Michaelis-Menten"), 
                                    selected = "Linear"
                                  )
                           ),
                           column(width = 3, 
                                  selectInput(
                                    inputId = "pk_administration", 
                                    label = "Administration-Absorption", 
                                    choices = c("Intravenous", "First-Order", "Gamma", "InverseGaussian", "Weibull"), 
                                    selected = "Intravenous"
                                  )
                           ),
                           column(class = "col-checkbox",
                                  width = 2, 
                                  checkboxInput(
                                    inputId = "pk_time_lag",
                                    label = "Time Lag",
                                    value = FALSE)
                           )
                  ),
                              
                                ###### Options: General ----
                              bslib::accordion(
                                open = FALSE,
                                bslib::accordion_panel(
                                  title = "Options",
                                  value = "options_panel",
                                  fluidRow(class = "multi-input-with-checkbox",
                                    column(
                                      width = 3, 
                                      selectInput(
                                        inputId = "pk_parameterization", 
                                        label = "Parameterization", 
                                        choices = c("Clearance", "Micro", "Macro", "Macro1"), 
                                        selected = "Clearance"
                                      )
                                    ),
                                    column(class = "col-checkbox",
                                      width = 3, 
                                      conditionalPanel(condition = "input.pk_parameterization == 'Clearance' && input.pk_administration == 'Intravenous' || 
                                                                    input.pk_parameterization == 'Micro' && input.pk_administration == 'Intravenous' ||
                                                                    input.pk_parameterization == 'Clearance' && input.pk_administration == 'First-Order' || 
                                                                    input.pk_parameterization == 'Micro' && input.pk_administration == 'First-Order'",
                                                       checkboxInput(inputId = "pk_closed_form", label = "Closed Form", value = TRUE)
                                      )
                                    ),
                                    column(class = "col-checkbox",
                                      width = 3, 
                                      checkboxInput(inputId = "pk_infusion", label = "Infusion", value = FALSE)
                                    ),
                                    column(class = "col-checkbox",
                                      width = 2, 
                                      conditionalPanel(condition = "input.pk_infusion == true",
                                                       checkboxInput(inputId = "pk_duration", label = "Duration", value = FALSE)
                                      )
                                    )
                                  ),
                                  fluidRow(class = "multi-input-with-checkbox",
                                    column(class = "col-checkbox",
                                      width = 3, offset = 3,
                                      conditionalPanel("input.pk_parameterization == 'Clearance' || input.pk_parameterization == 'Micro'",
                                                       checkboxInput(inputId = "pk_eliminationcomp", label = "Elimination Compartment", value = FALSE),
                                      )
                                    ),
                                    column(class = "col-checkbox",
                                      width = 3,
                                      conditionalPanel(condition = "input.pk_eliminationcomp == true && input.pk_parameterization == 'Clearance' || input.pk_eliminationcomp == true && input.pk_parameterization == 'Micro'",
                                                       checkboxInput(inputId = "pk_fe", label = "Fraction Excreted Parameter", value = FALSE)
                                      )
                                    )
                                  ),
                                                        
                                ###### Options: PK/Emax ----
                              conditionalPanel("input.modeltype == 'PK/Emax'",
                                               fluidRow(
                                                 column(width = 3, 
                                                        checkboxInput(inputId = "pd_hasEffectsCompartment", label = "Effect Compartment", value = FALSE)
                                                 ),
                                                 column(width = 3, 
                                                        checkboxInput(inputId = "pd_isPkFrozen", label = "Freeze PK", value = FALSE)
                                                 ),
                                                 column(width = 2, 
                                                        checkboxInput(inputId = "pd_isEmaxFrozen", label = "Freeze Emax", value = FALSE)
                                                 )
                                               )
                              ),
                                                        
                                ###### Options: PK/Indirect ----
                              conditionalPanel("input.modeltype == 'PK/Indirect'",
                                               fluidRow(
                                                 column(width = 3,
                                                        checkboxInput(inputId = "pki_hasEffectsCompartment", label = "Effect Compartment", value = FALSE)
                                                 ),
                                                 column(width = 3,
                                                        checkboxInput(inputId = "pki_isPkFrozen", label = "Freeze PK", value = FALSE)
                                                 ),
                                                 column(width = 3, 
                                                        checkboxInput(inputId = "pki_indirectFrozen", label = "Freeze Indirect", value = FALSE)
                                                 )
                                               )
                              ),
                                                        
                                ###### Options: PK/Linear ----
                              conditionalPanel("input.modeltype == 'PK/Linear'",
                                               fluidRow(
                                                 column(width = 3,
                                                        checkboxInput(inputId = "pkl_hasEffectsCompartment", label = "Effect Compartment", value = FALSE)
                                                 ),
                                                 column(width = 3, 
                                                        checkboxInput(inputId = "pkl_isPkFrozen", label = "Freeze PK", value = FALSE)
                                                 ),
                                                 column(width = 3, 
                                                        checkboxInput(inputId = "pkl_isLinearFrozen", label = "Freeze Linear", value = FALSE)
                                                 )
                                               )
                              )
                            )
                          )
              ),
                                                    
                  ##### Model = Emax ----
                conditionalPanel(condition = "input.modeltype == 'Emax' || input.modeltype == 'PK/Emax'",
                                 htmltools::h4("Emax Structural Model"),
                                 fluidRow(style = "padding-top: 20px;",
                                   column(width = 2,
                                          checkboxInput(inputId = "pd_checkBaseline", label = "Baseline", value = FALSE)
                                   ),
                                   column(width = 2,
                                          checkboxInput(inputId = "pd_checkFractional", label = "Fractional", value = FALSE) %>% shinyjs::disabled()
                                   ),
                                   column(width = 2,
                                          checkboxInput(inputId = "pd_checkInhibitory", label = "Inhibitory", value = FALSE)
                                   ),
                                   column(width = 2,
                                          checkboxInput(inputId = "pd_checkSigmoid", label = "Sigmoid", value = FALSE)
                                   )
                                 ),
                ),
                                                    
                  ##### Model = PK/Indirect ----
                conditionalPanel("input.modeltype == 'PK/Indirect'",
                                 div(style = "padding:5px;"),
                                 htmltools::h4("Indirect Structural Model"),
                                 fluidRow(class = "multi-input-with-checkbox",
                                   column(
                                     width = 3,
                                     selectInput(
                                       inputId = "pki_indirectType", 
                                       label = "Type", 
                                       choices = c("Limited Stimulation", "Limited Inhibition", "Infinite Stimulation",  "Inverse Inhibition", "Linear Stimulation", "Log Linear Stimulation")
                                     )
                                   ),
                                   column(class = "col-checkbox",
                                     width = 2, 
                                     checkboxInput(inputId = "pki_isBuildup", label = "Buildup", value = TRUE)
                                   ),
                                   column(class = "col-checkbox",
                                     width = 2, 
                                     checkboxInput(inputId = "pki_isExponent", label = "Exponent", value = FALSE)
                                   )
                                 )
                ),
                                                    
                  ##### Model = PK/Linear ----
                conditionalPanel("input.modeltype == 'PK/Linear'",
                                 div(style = "padding:5px;"),
                                 htmltools::h4("Linear Structural Model"),
                                 fluidRow(
                                   column(
                                     width = 3,
                                     selectInput(
                                       inputId = "pkl_linearType",
                                       label = "Linear Type",
                                       choices = c("Constant", "Linear", "Quadratic"))
                                   )
                                 )
                ),
             
                  ##### Residual Error Model ----
                # div(style = "padding: 10px;"),
                htmltools::h4("Residual Error Model"),
              div(class = "residual_errors",
                uiOutput("residEffect1")
              )
              )
            )
          ),
                                    
            ### 4.2 Parameters Tab ----
          bslib::nav_panel(
            title = "Parameters",
            value = "side_nav_tab_parm",
            bslib::navset_pill_list(
              widths = c(2, 10), 
              
                #### 4.2a Structural Parameters ----
              bslib::nav_panel(
                title = "Structural Parameters",
                value = "tab_structuralparam",
                fluidRow(
                  column(width = 2,
                         div(style = "padding:5px;"),
                         h6("Structural Parameter"),
                         div(style = "padding:13px;"),
                         uiOutput("structuralParamsNames")
                  ),
                  column(width = 3,
                         div(style = "padding:5px;"),
                         h6("Style"),
                         div(style = "padding:13px;"),
                         div(class = "multi-input-selectInputs",
                             uiOutput("structuralParamsStyle")
                         )
                  ),
                  column(width = 2,
                         div(style = "padding:5px;"),
                         h6("Fixed Effect"),
                         div(style = "padding:13px;"),
                         uiOutput("structuralParamsFixEffName")
                  ),
                  column(width = 5,
                         div(style = "padding:5px;"),
                         conditionalPanel(condition = "input.pk_switch_population == true",
                                          h6("Random Effect"),
                                          fluidRow(
                                            column(width = 2, 
                                                   div(style = "padding:16px;"),
                                                   uiOutput("structuralParamsRanEff")
                                            ),
                                            column(width = 8,
                                                   div(style = "padding:13px;"),
                                                   uiOutput("structuralParamsRanEffName")
                                            )
                                          )
                         )
                  )
                )
              ),
              
                #### 4.2b Covariates ----
              bslib::nav_panel(
                title = "Covariates",
                value = "tab_covariate",
                fluidRow(
                  style = "padding-bottom: 1rem;",
                  column(width = 1, offset = 11, 
                    shinyWidgets::actionBttn("addCovPkBtn", label = NULL, icon = shiny::icon("plus"), style = "material-circle" )
                  )
                ),
                div(id = "add_panels_here")
              ),
                #### 4.2c Fixed Effects ----
              bslib::nav_panel(
                title = "Fixed Effects",
                value = "tab_fixedeffects",
                fluidRow(
                  column(width = 2, 
                         div(style = "padding:5px;"),
                         h6("Fixed Effects"),
                         div(style = "padding:13px;"),
                         uiOutput("fixedEffectNames")
                  ),
                  column(width = 2, 
                         div(style = "padding:5px;"),
                         h6("Initial"),
                         div(style = "padding:13px;"),
                         uiOutput("fixedEffectInitialNumeric")
                  ),
                  column(width = 2, 
                         div(style = "padding:5px;"),
                         h6("Lower"),
                         div(style = "padding:13px;"),
                         uiOutput("fixedEffectLowerNumeric")
                  ),
                  column(width = 2, 
                         div(style = "padding:5px;"),
                         h6("Upper"),
                         div(style = "padding:13px;"),
                         uiOutput("fixedEffectUpperNumeric")
                  ),
                  column(width = 1, class = "col-checkbox",
                         div(style = "padding:5px;"),
                         h6("Freeze"),
                         div(style = "padding:16px;"),
                         uiOutput("fixedEffectFreeze")
                  ),
                  column(width = 2, 
                         div(style = "padding:5px;"),
                         h6("Units"),
                         div(style = "padding:13px;"),
                         uiOutput("fixedEffectUnits")
                  )
                ),
                fluidRow(
                  uiOutput("feRangeValidation")
                )
              ),
                #### 4.2d Random Effects ----
              bslib::nav_panel(
                title = "Random Effects",
                value = "tab_randomeffects",
                class = "raneff-pane",
                bslib::card(
                  style = "border: none; padding: 0px;",
                  fluidRow(
                    column(width = 12,
                      div(class = "ran_effects_div",
                          style = "padding:8px;",
                          #uiOutput("diagonalStr0"),
                          uiOutput("ranEffSelections1"),
                          uiOutput("ranEffMatrix1"),
                          uiOutput("ranEffSelections2"),
                          uiOutput("ranEffMatrix2"),
                          uiOutput("ranEffSelections3"),
                          uiOutput("ranEffMatrix3"),
                          uiOutput("ranEffSelections4"),
                          uiOutput("ranEffMatrix4"),
                          uiOutput("ranEffSelections5"),
                          uiOutput("ranEffMatrix5"),
                          uiOutput("ranEffSelections6"),
                          uiOutput("ranEffMatrix6"),
                          uiOutput("ranEffSelections7"),
                          uiOutput("ranEffMatrix7"),
                          uiOutput("ranEffSelections8"),
                          uiOutput("ranEffMatrix8"),
                          uiOutput("ranEffSelections9"),
                          uiOutput("ranEffMatrix9"),
                          uiOutput("ranEffSelections10"),
                          uiOutput("ranEffMatrix10")
                      )
                    )
                  )
                ), 
                div(style = "padding:25px;")
              )
            )
          ),
                                    
            ### 4.3 Input Options Tab ----
          bslib::nav_panel(
            title = "Input Options",
            value = "side_nav_tab_inputoptions",
            bslib::card(
              style = "flex-grow: 0; margin-top: 0;",
              bslib::card_title("Input Options"),
              bslib::card_body(
                div(style = "text-align: left",
                  #### 4.3a Reset Checkbox ----
                fluidRow(
                  column(width = 12, 
                    checkboxInput(inputId = "reset", label = "Reset", value = FALSE),
                    conditionalPanel("input.reset == true",
                      column(width = 1, 
                        numericInput(inputId = "resetLow", label = "Low", value = 4, min = 0, max = 1000)
                      ),
                      column(width = 1, 
                        numericInput(inputId = "resetHi", label = "High", value = 4, min = 1, max = 1000)
                      )
                    )
                  )
                ),
                  #### 4.3b MDV Checkbox ----
                fluidRow(
                  column(width = 12,
                    checkboxInput(inputId = "mdv", label = "MDV", value = FALSE)
                  )
                ),
                  #### 4.3c Steady State / ADDL Checkboxes ----
                fluidRow(
                  column(width = 12, 
                    checkboxInput(inputId = "isSS", label = "Steady State (Column Definition)", value = FALSE),
                    conditionalPanel("input.isSS == true",
                      div(style = "padding-left: 30px;",
                          checkboxInput(inputId = "isSSOffset", label = "SS = 2", value = FALSE),
                      )
                    ),
                    checkboxInput(inputId = "isADDL", label = "ADDL (Column Definition)", value = FALSE ),
                    checkboxInput(inputId = "isSSDoseCycle", label = "Steady State (Dose Cycle)", value = FALSE),
                    checkboxInput(inputId = "isADDLDoseCycle", label = "ADDL (Dose Cycle)", value = FALSE)
                  )
                ),
                  #### 4.4d Dose Cycle Dropdowns ----
                # SS Dropdown
                conditionalPanel(
                  "input.isSSDoseCycle == true",
                  fluidRow(style = "padding-bottom: 1rem;",
                    column(width = 12, 
                           bslib::accordion(
                             bslib::accordion_panel(
                               title = "Steady State",
                               div(id = "add_SS_here"),
                               fluidRow(
                                 column(width = 1, offset = 11, 
                                        div(style = "padding-bottom: 17px; margin-left:-7px",
                                            shinyWidgets::actionBttn("addSSBtn", label = NULL, icon = shiny::icon("plus"), style = "material-circle" )
                                        )
                                 ),
                                 verbatimTextOutput("timeTotalSS"),
                               )
                             )
                           )
                    )
                  )
                ),
                # ADDL Dropdown 
                conditionalPanel(
                  "input.isADDLDoseCycle == true",
                  fluidRow(style = "padding-bottom: 1rem;",
                    column(width = 12, 
                           bslib::accordion(
                             bslib::accordion_panel(
                               title = "ADDL",
                               div(id = "add_ADDL_here"),
                               fluidRow(
                                 column(width = 11),
                                 column(width = 1, 
                                        div(style = "padding-bottom: 17px; margin-left:-7px",
                                            shinyWidgets::actionBttn("addADDLBtn", label = NULL, icon = shiny::icon("plus"), style = "material-circle" )
                                        )
                                 )
                               ),
                               verbatimTextOutput("timeTotalADDL")
                             )
                           )
                    )
                  )
                )
             )
           )
         )
       ),
                    
            ### 4.4 Column Mapping Tab ----
          bslib::nav_panel(
            title = "Column Mapping",
            value = "side_nav_tab_columndef",
            bslib::card(
              bslib::card_title("Column Mapping (* Required Fields)"),
              bslib::layout_columns(
                col_widths = c(5, 7),
                
                  #### 4.4a Column Selectors ----
                bslib::card(
                  class = "column-mapping-selectors",
                  style = "border: none;",
                  uiOutput("columnMap"),
                  # uiOutput("columnRandMap"),
                  uiOutput("columnDoseMapping"),
                  uiOutput("colValidationDup"),
                  uiOutput("colValidationIDLength"),
                  bslib::accordion(
                    style = "padding-top: 5px",
                    open = FALSE,
                    bslib::accordion_panel(
                      title = "User Defined Column Definitions",
                      fluidRow(
                        column(width = 1, 
                               div(style = "padding-bottom: 17px; margin-left:-7px",
                                   shinyWidgets::actionBttn("addExtraDefBtn", label = NULL, icon = shiny::icon("plus"), style = "material-circle", size = "xs")
                               )
                        )
                      ),
                      div(id = "add_extradef_here")
                    )
                  )
                ),
                
                  #### 4.4b Code Generator ----
                  bslib::card(
                    style = "border: none;",
                    shinyAce::aceEditor(
                      outputId = "colsout",
                      autoScrollEditorIntoView = TRUE,
                      minLines = 5,
                      maxLines = 35,
                      value = NULL,
                      readOnly = TRUE,
                      placeholder = "Click 'Preview' to view column definition file"
                    ),
                    actionButton(inputId = "updateColsOut", label = "Preview"),
                  )
                )
              )
            )
          ),
                      
          ## 5.0 Code Window ----
          bslib::navset_card_underline(
            ### 5.1 PML Tab ----
        bslib::nav_panel(
          "PML",
          bslib::card_body(
            shinyAce::aceEditor(
              outputId = "ace",
              selectionId = "selection",
              autoScrollEditorIntoView = TRUE,
              minLines = 5,
              maxLines = 35,
              value = NULL,
              readOnly = TRUE,
              placeholder = "PML function definition required"
            ),
            bslib::accordion(open = FALSE,
                             bslib::accordion_panel(
                               title = "Options",
                               selectInput(
                                 inputId = "theme",
                                 label = "Theme: ",
                                 choices = themes,
                                 selected = "dreamweaver"
                               )
                             )
            )
          )
        ),
            ### 5.2 Data Tab ----
        bslib::nav_panel(title = "Data", style = "overflow: scroll;",
                         height = "100%",
                         bslib::card(style = "overflow: scroll;",
                                     div(DT::dataTableOutput("userData")),
                                     full_screen = TRUE)
        ),
            ### 5.3 RsNLME Tab ----
            bslib::nav_panel(
              "RsNLME",
              bslib::card_body(
                h5("RsNLME Code"),
                shinyAce::aceEditor(
                  outputId = "rsnlme",
                  autoScrollEditorIntoView = TRUE,
                  minLines = 5,
                  maxLines = 35,
                  value = NULL,
                  readOnly = TRUE,
                  placeholder = "PML function definition required"
                ),
                actionButton(inputId = "generate_code", label = "Generate")
              )
            )
          )
        )
      ),
      
      ## 6.0 Footer ----
      certara_footer(url = 'https://certara.github.io/R-RsNLME-model-builder/')

    )
  )
  
  
  runApp(
    shinyApp(ui = ui, server = server),
    launch.browser = TRUE
  )
  
}

#' Build RsNLME model from Shiny GUI and generate corresponding RsNLME code
#'
#' Shiny application to build RsNLME model from Shiny GUI and generate corresponding RsNLME code 
#' based on input selections.
#'
#' @param data Input dataset.
#' @param modelName Name of the model; if \code{missing}, named as 'PKPDmodel.'
#' @param workingDir Working directory to run the model. Current working directory 
#' will be used if \code{workingDir} not specified or does not exist.
#' @param baseModel The model object from where the input dataset and model name are recovered if 
#' arguments \code{data} and \code{modelName} are not specified.
#'  
#' @examples
#' if (interactive()) {
#' model <- modelBuilderUI(data = Certara.RsNLME::pkData, modelName = "PK_Model")
#' }
#'  
#' @return A model object of class \code{NlmePmlModel}
#' @export
modelBuilderUI <- function(data, modelName = "PKPDmodel", workingDir = "", baseModel = NULL) {
  if (!missing(baseModel)) {
    if(!missing(data)) {
      warning("Both data and baseModel are given. ",
              "Data from the model will be used", 
              immediate. = TRUE)
    }
    
    data <- baseModel@inputData
    
    if(!missing(modelName)) {
      warning("Both modelName and baseModel are given. ",
              "ModelName from the model will be used", 
              immediate. = TRUE)
    }
    
    modelName <- baseModel@modelInfo@modelName
  }
  
  wd <- file.path(tempdir(TRUE), modelName)
  
  if (!dir.exists(wd)) {
    dir.create(wd, recursive = TRUE)
    if (!dir.exists(wd)) {
      warning("Cannot create directory ", wd, "\nExiting modelBuilderUI", 
              call. = FALSE, immediate. = TRUE)
      return("")
    }
  }
  
  if(missing(data) | !is.data.frame(data)){
    warning("Missing data argument, using Certara.RsNLME::pkData", immediate. = TRUE)
    data <- as.data.frame(Certara.RsNLME::pkData)
  }
  
  # TDL does not recognize tilda
  shinypmlfile_full <- path.expand(file.path(wd, "test.mdl"))
  if (is.null(baseModel)) {
    model <-
      Certara.RsNLME::pkmodel(
        isPopulation = TRUE,
        parameterization = "Clearance",
        absorption = "Intravenous",
        numCompartments = 1,
        isClosedForm = TRUE,
        isTlag = FALSE,
        hasEliminationComp = FALSE,
        isFractionExcreted = FALSE,
        isSaturating = FALSE,
        infusionAllowed = FALSE,
        isDuration = FALSE,
        data = NULL,
        columnMap = FALSE,
        modelName = modelName,
        workingDir = wd
      )
    model <-
      Certara.RsNLME::residualError(
        model,
        predName = "C",
        errorType = "Multiplicative",
        SD = 0.1,
        isFrozen = FALSE,
        isBQL = FALSE
      )
    statements <- unlist(model@statements)
  } else {
    statements <- unlist(baseModel@statements)
  }
  
  cat(paste(statements, collapse = "\n"), file = shinypmlfile_full)
  model <- .run_shiny_model_builder(pmlfile_out = shinypmlfile_full, modelName, data, baseModel, dataName = deparse(substitute(data)))
  
  if (!is.null(model)) {
    if (dir.exists(workingDir)) {
      model@modelInfo@workingDir <- file.path(workingDir, modelName)
    } else {
      model@modelInfo@workingDir <- file.path(normalizePath(".", winslash = "/", mustWork = FALSE), modelName)
    }
  }
  
  return(invisible(model))
}

# JS Code to navigate between side nav tabs programatically see also side_nav_tabs_info

jsFunctions <- "shinyjs.closewindow = function() { window.close(); }"

#    $('li.tab a[href$=\"#tab2\"]').trigger('hide');
#    $('li.tab a[href$=\"#tab2\"]').trigger('hidden');


replace_data_arg <- function(code, dataName){
  sub("dataMapping\\(data\\)", paste0("dataMapping\\(", dataName, "\\)"), code)
}

remove_working_dir <- function(code){
  sub(", workingDir.*",") %>%",code)
}
