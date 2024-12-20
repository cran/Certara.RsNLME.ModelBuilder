#' Edit textual RsNLME model from Shiny GUI
#'
#' Shiny application to update RsNLME model from Shiny GUI and directly edit PML statements using Ace editor. Syntax and semantic check is performed
#' by TDL executable (if presented). The Shiny application also allows adding input options and column mappings from Shiny GUI.
#'
#' @param baseModel The model object from where the information is recovered.
#' @param initpml Initial PML model file to be edited. Overrides
#'   \code{baseModel@statements}, if presented.
#' @param data Input data frame.
#' Overrides \code{baseModel@inputData}, if presented.
#' @param modelName Name of the model; if \code{missing}, named as 'PKPDmodel'.
#' Overrides \code{baseModel@modelInfo@modelName}, if presented.
#' 
#' @examples
#' if (interactive()) {
#' model <- modelBuilderUI(data = Certara.RsNLME::pkData, modelName = "PK_Model")
#' 
#' model <- modelTextualUI(baseModel = model)
#' }
#'
#' @return A model object of class \code{NlmePmlModel}
#' @export
modelTextualUI <- function(baseModel, initpml, data, modelName = "PKPDmodel") {
  
  # App Setup ----
  if (!missing(data)) {
    stopifnot(is.data.frame(data))
  }

  if (!missing(modelName)) {
    stopifnot(is.character(modelName))
  }

  if (!missing(initpml)) {
    stopifnot(is.character(initpml))
  }

  if (missing(baseModel) &&
    (missing(initpml) || missing(data))) {
    stop(
      "Cannot start model Builder since essential arguments are not given:\n",
      "model | (initpml & data)"
    )
  }
  
  wd <- file.path(tempdir(TRUE), modelName)
  if (!dir.exists(wd)) {
    dir.create(wd, recursive = TRUE)
    if (!dir.exists(wd)) {
      warning("Cannot create directory ", wd, "\nExiting modelBuilderUI",
        call. = FALSE, immediate. = TRUE
      )
      return("")
    }
  }

  if (!missing(baseModel)) {
    stopifnot(class(baseModel)[1] == "NlmePmlModel")

    if (!missing(data)) {
      warning("Both data and baseModel are given. \n",
        "Data in the model will be substituted, please map it.",
        immediate. = TRUE
      )

      baseModel@inputData <- data
      dataColNames <- colnames(data)
      newmapping <- lapply(baseModel@columnMapping@mapping, function(x) {
        if (!x@columnName %in% dataColNames) {
          x@columnName <- "?"
        }
        x
      })

      baseModel@columnMapping@mapping <- newmapping
    } else {
      data <- baseModel@inputData
    }

    if (!missing(modelName)) {
      warning("Both modelName and baseModel are given. ",
        "ModelName in the model will be substituted",
        immediate. = TRUE
      )

      baseModel@modelInfo@modelName <- modelName
    } else {
      modelName <- baseModel@modelInfo@modelName
    }
  } else {
   
    suppressWarnings(
    baseModel <- textualmodel(modelName = modelName, 
                              workingDir = wd,
                              data = data, 
                              mdl = initpml)
    )

    baseModel <- Certara.RsNLME::parsePMLColMap(baseModel)
  }

  shinypmlfile_full <- path.expand(file.path(wd, "test.mdl"))
  statements <- unlist(baseModel@statements)
  cat(paste(statements, collapse = "\n"), file = shinypmlfile_full)
  model <- .run_shiny_model_textual(baseModel, pmlfile_out = shinypmlfile_full, modelName, data)
  model
}


.run_shiny_model_textual <- function(baseModel, pmlfile_out, modelName, data) {
  if (.Platform$OS.type == "windows") {
    installDir <- gsub("\\", "/", utils::shortPathName(Sys.getenv("INSTALLDIR")), fixed = TRUE)
    TDLLocation <- file.path(installDir, "TDL5.exe")
  } else {
    installDir <- Sys.getenv("INSTALLDIR")
    TDLLocation <- file.path(installDir, Sys.getenv("PML_BIN_DIR"), "TDL5")
  }

  tdl_warnings_file <- "TDL5Warnings"

  syntaxcheck <- file.exists(TDLLocation)
  if (!syntaxcheck) {
    warning(
      "TDL5 was not found in \n", TDLLocation,
      "\nSyntax check will be disabled."
    )
  }

  themes <- shinyAce::getAceThemes()
  pml_wordlist <- .create_pml_wordlist()

  # we should not touch the baseModel since that is a model we need to return in case of troubles
  exUserDef <- splitDefs(baseModel@userDefinedExtraDefs)

  exMDV <- getUserDefMDV(exUserDef)
  exReset <- getUserDefReset(baseModel, exUserDef)
  exSS <- getUserDefSS(exUserDef)
  exSSOff <- getUserDefSSOff(exUserDef)
  exADDL <- getUserDefADDL(exUserDef)
  exII <- getUserDefII(exUserDef)
  
  exUserDoseCycle <- getUserDefDoseCycle(exUserDef)
  extraDoses <- baseModel@extraDoses
  
  if (length(extraDoses) > 0) {
    #SteadyStateDose; doseType = 1
  doseCycleSS <- any(lapply(extraDoses, function(x) x@doseType) == 1)
    #ADDLDose; doseType = 2
  doseCycleADDL <- any(lapply(extraDoses, function(x) x@doseType) == 2)
  } else {
    doseCycleSS <- FALSE
    doseCycleADDL <- FALSE
  }
  
  if (!is.null(exUserDoseCycle) && any(grepl("ss", exUserDoseCycle))) {
    doseCycleSS <- TRUE
  }
  
  if (!is.null(exUserDoseCycle) && any(grepl("addl", exUserDoseCycle))) {
    doseCycleADDL <- TRUE
  }
  
  # Remove tables parsed to extra col defs so shiny does not display as one of user defined defs
  newdefs <- rmTablesFromColDef(exUserDef)
  baseModel@userDefinedExtraDefs <- as.list(newdefs)
  
  #Check whether infusion exists in basemodel
  exInfusionDose <- list()
  for(dose in doseNames(baseModel)){
    #Check Rate
    if(paste0(dose, "_Rate") %in% names(baseModel@columnMapping@mapping)){
      exInfusionDose[[paste0(dose)]]$Rate <- TRUE
    } else {
      exInfusionDose[[paste0(dose)]]$Rate <- FALSE
    }
    if(paste0(dose, "_Duration") %in% names(baseModel@columnMapping@mapping)){
      exInfusionDose[[paste0(dose)]]$Duration <- TRUE
    } else {
      exInfusionDose[[paste0(dose)]]$Duration <- FALSE
    }
  }
  
  
  # do not compare whitespaces
  basePML <- .remove_comments(paste0(unlist(baseModel@statements), collapse = "\n"))

  # Server ----
  server <- function(input, output, session) {

    reactiveModel <- reactiveValues(model = baseModel)
    
    
    ## Column Mappings ----
    output$columnMap <- renderUI({
      req(reactiveModel$model)
      noreqcols <- c("A1Strip")
      dosenames <- Certara.RsNLME::doseNames(reactiveModel$model)
      for(d in dosenames){
        #req(input[[paste0("dose_", d)]])
        if(!is.null(input[[paste0("dose_", d)]]) && input[[paste0("dose_", d)]] == FALSE){
        reactiveModel$model@columnMapping@mapping[[paste0(d, "_Rate")]] <- NULL
        }
      }
      
      colnames <- reactiveModel$model@columnMapping@mapping
      
      uiColMapDropdowns <- vector(mode = "list", length = length(colnames))
      
      for(i in names(colnames)){
        uiColMapDropdowns[[i]] <- tagList(
          selectInput(
            inputId = paste0("col_", i),
            label = if(colnames[[i]]@variableName %in% noreqcols || colnames[[i]]@variableType$type == "observation"){
              colnames[[i]]@variableName
            } else {
              paste0(colnames[[i]]@variableName, " *")
            },
            selected = if(colnames[[i]]@columnName == "?"){
              " " 
            } else {
              if(colnames[[i]]@variableName == "id"){
                c(trimws(unlist(strsplit(colnames[[i]]@columnName, split = ","))))
              } else {
                colnames[[i]]@columnName
              }
            },
            choices = if(colnames[[i]]@variableName == "id"){
              names(baseModel@inputData)
            } else {
              c(" ", names(baseModel@inputData))
            },
            multiple = if(colnames[[i]]@variableName == "id") TRUE else FALSE
          ),
          div(style = "padding: 3px;")
        )
      }

        uiColMapDropdowns

    })
    
    outputOptions(output, "columnMap", suspendWhenHidden = TRUE)
   

    
    ## Column Definition File ----
    
    
    
    ## Reset ----
    observeEvent(c(input$reset, input$resetSelectDose, input$resetLow, input$resetHi),{
      #modbod <<- reactiveModel$model@userDefinedExtraDefs
      
      extradefs <- splitDefs(reactiveModel$model@userDefinedExtraDefs)
      
      posReset <- grep(pattern = "^reset", extradefs)
      
      if(length(posReset) > 0){
        extradefs <- extradefs[-posReset]
      }
      
      if(input$reset){
        resetstatement <-paste0("reset(\"", input$resetSelectDose, "\", c(", input$resetLow, ",", input$resetHi, "))")
        reactiveModel$model@hasResetInfo <- TRUE
        reactiveModel$model@resetInfo <- Certara.RsNLME::ResetColumnInfo(input$resetLow,input$resetHi)
      } else {
        resetstatement <- NULL
        reactiveModel$model@hasResetInfo <- FALSE
        reactiveModel$model@resetInfo <- NULL
      }
      extradefs <- c(extradefs, resetstatement)
      
      reactiveModel$model@userDefinedExtraDefs <- as.list(paste0(extradefs, collapse = "\n"))

    }, ignoreInit = TRUE)
  
    
    
    ## MDV ----
    observeEvent(c(input$mdvSelectDose, input$mdv),{
      extradefs <- splitDefs(reactiveModel$model@userDefinedExtraDefs)
      posMDV <- grep(pattern = "^mdv", extradefs)
      
      if(length(posMDV) > 0){
        extradefs <- extradefs[-posMDV]
      }

      if(input$mdv){
        mdvstatement <-paste0("mdv(\"", input$mdvSelectDose, "\")" )
      } else {
        mdvstatement <- NULL
      }
      extradefs <- c(extradefs, mdvstatement)
      
      reactiveModel$model@userDefinedExtraDefs <- as.list(paste0(extradefs, collapse = "\n"))
      
     
    }, ignoreInit = TRUE)
    
    
    
    ## ADDL ----
    observeEvent(c(input$addlSelectDose, input$isADDL, input$iiSelectDose),{
      extradefs <- splitDefs(reactiveModel$model@userDefinedExtraDefs)
      posADDL <- grep(pattern = "^addlcol", extradefs)
      posII <- grep(pattern = "^iicol", extradefs)

      
      if(length(posADDL) > 0){
        extradefs <- extradefs[-grep(pattern = "^addlcol", extradefs)]
      }
      if(length(posII) > 0){
        extradefs <- extradefs[-grep(pattern = "^iicol", extradefs)]
      }

      if(input$isADDL){
        addlstatement <-paste0("addlcol(", input$addlSelectDose, ")" )
        iistatement <-paste0("iicol(", input$iiSelectDose, ")" )
      } else {
        addlstatement <- NULL
        iistatement <- NULL
      }
      
      extradefs <- c(extradefs, addlstatement, iistatement)
      
      reactiveModel$model@userDefinedExtraDefs <- as.list(paste0(extradefs, collapse = "\n"))
    }, ignoreInit = TRUE)
    
    
    
    ## SS ----
    observeEvent(c(input$ssSelectDose, input$isSS, input$iiSelectDose, input$isSSOffset, input$ssOffsetSelectDose),{
      extradefs <- splitDefs(reactiveModel$model@userDefinedExtraDefs)
      
      posSS <- grep(pattern = "^sscol", extradefs)
      posII <- grep(pattern = "^iicol", extradefs)
      posSSoffset <- grep(pattern = "^ssoffcol", extradefs)

      if(length(posSS) > 0){
        extradefs <- extradefs[-grep(pattern = "^sscol", extradefs)]
        
      }
      if(length(posSSoffset) > 0){
        extradefs <- extradefs[-grep(pattern = "^ssoffcol", extradefs)]
      }
      if(length(posII) > 0 && input$isADDL == FALSE){
        extradefs <- extradefs[- grep(pattern = "^iicol", extradefs)]
      }

      ssstatement <- NULL
      iistatement <- NULL
      ssoffsetstatement <- NULL
      
      if(input$isSS && input$isADDL){
        ssstatement <-paste0("sscol(", input$ssSelectDose, ")" )
      } 
      
      if(input$isSSOffset && input$isSS){
        ssoffsetstatement <-paste0("ssoffcol(", input$ssOffsetSelectDose, ")" )
      }
      
      if(input$isSS && input$isADDL == FALSE){
        ssstatement <-paste0("sscol(", input$ssSelectDose, ")" )
        iistatement <-paste0("iicol(", input$iiSelectDose, ")" )
      }
      
      extradefs <- c(extradefs, ssstatement, ssoffsetstatement, iistatement)
   
      reactiveModel$model@userDefinedExtraDefs <- as.list(paste0(extradefs, collapse = "\n"))
    }, ignoreInit = TRUE)
    

    
    ## Infusion ----
    assign("tick", value = 1, envir = model_builder_env)
    output$infusion <- renderUI({
      req(reactiveDoseInputs$values)
      req(reactiveDoseNames())
      tick_count <- get("tick", envir = model_builder_env)
      dosenames <- reactiveDoseNames()
      #dosenames <- Certara.RsNLME::doseNames(reactiveModel$model)
      dosenamesUI <- vector(mode = "list", length(dosenames))
      for(d in dosenames){
        # infusion_val <- isolate(ifelse(!is.null(reactiveDoseInputs$values$input[[d]]), reactiveDoseInputs$values$input[[d]], FALSE))
        # duration_val <- isolate(ifelse(!is.null(reactiveDoseInputs$values$isDuration[[d]]), reactiveDoseInputs$values$isDuration[[d]], FALSE))
        infusion_val <- ifelse(!is.null(reactiveDoseInputs$values$input[[d]]), reactiveDoseInputs$values$input[[d]], FALSE)
        duration_val <- ifelse(!is.null(reactiveDoseInputs$values$isDuration[[d]]), reactiveDoseInputs$values$isDuration[[d]], FALSE)
        if(tick_count == 1) {
          if (length(exInfusionDose) > 0) {
            infusion_val <- any(c(exInfusionDose[[d]]$Rate, exInfusionDose[[d]]$Duration))
            duration_val <- exInfusionDose[[d]]$Duration
          }
        }
        dosenamesUI[[d]] <- tagList(
          fluidRow(
            column(width = 3,
              checkboxInput(inputId = paste0("dose_", d), label = paste0("Infusion: ", d), value = infusion_val)
            )
          ),
          conditionalPanel(
            paste0("input.dose_", d, " == true"),
            fluidRow(
              column(width = 3, offset = 1, 
                             checkboxInput(inputId = paste0("isDuration_", d), label = "Duration", value = duration_val)
              ),
            )
          )
        )
      }
      
      assign("tick", value = tick_count + 1, envir = model_builder_env)

      dosenamesUI
    })
    
    outputOptions(output, "infusion", suspendWhenHidden = TRUE)
    
    
    reactiveDoseInputs <- reactiveValues(values = list(input = NULL,
                                                       col = NULL,
                                                       isDuration = NULL,
                                                       isSecondDose = NULL))
    
    
    observeEvent(doseNames(reactiveModel$model), {
      for(dose in doseNames(reactiveModel$model)){
        req(input[[paste0("dose_", dose)]])
        reactiveDoseInputs$values$input[[paste0(dose)]] <- input[[paste0("dose_", dose)]]
        reactiveDoseInputs$values$col[[paste0(dose)]] <- input[[paste0("col_", dose)]]
        reactiveDoseInputs$values$isDuration[[paste0(dose)]] <- input[[paste0("isDuration_", dose)]]
        #reactiveDoseInputs$values$isSecondDose[[paste0(dose)]] <- input[[paste0("isSecondDose_", dose)]]
      }
    }, priority = 1)

    reactiveDoseNames <- eventReactive(pmlinput(), {
      Certara.RsNLME::doseNames(reactiveModel$model)
    })
    observe({
      #lapply(Certara.RsNLME::doseNames(reactiveModel$model), function(x) {
      lapply(reactiveDoseNames(), function(x) {
        observeEvent(c(input[[paste0("dose_", x)]], input[[paste0("isDuration_", x)]]),{
              if(input[[paste0("dose_", x)]] == TRUE){
                if(input[[paste0("isDuration_", x)]]){
                    if(paste0(x, "_Duration") %notin% names(reactiveModel$model@columnMapping@mapping)){
                      reactiveModel$model@columnMapping@mapping[[paste0(x, "_Rate")]] <- NULL
                    reactiveModel$model <- Certara.RsNLME::addInfusion(reactiveModel$model, doseCptName = x, isDuration = TRUE, isSecondDose = FALSE)
                  }
                } else {
                  if(paste0(x, "_Rate") %notin% names(reactiveModel$model@columnMapping@mapping)){
                      reactiveModel$model@columnMapping@mapping[[paste0(x, "_Duration")]] <- NULL
                      reactiveModel$model <- Certara.RsNLME::addInfusion(reactiveModel$model, doseCptName = x, isDuration = FALSE, isSecondDose = FALSE)
                  }
                }
              } else {
                reactiveModel$model@columnMapping@mapping[[paste0(x, "_Rate")]] <- NULL
                reactiveModel$model@columnMapping@mapping[[paste0(x, "_Duration")]] <- NULL
              }
        }, ignoreInit = TRUE)
      })
    })

    
    
    ## Column Mapping & Update Column Def File ----
    observe({
      modelvarnames <- names(reactiveModel$model@columnMapping@mapping)
      for(n in modelvarnames){
                req(input[[paste0("col_", n)]])
      }
      
      for(i in modelvarnames){
         reactiveModel$model@columnMapping@mapping[[i]]@columnName <- paste0(gsub(" ", "?", input[[paste0("col_", i)]]), collapse = ",")
       }
    })
    

    
    ## Column Validation ----
    ### Duplicated Columns ----
    reactive_col_validation_dup <- reactive({
      posCol <- grep(pattern = "^col", names(input))
      cols <- vector(mode = "list", length = length(posCol))

      colnames <- reactiveModel$model@columnMapping@mapping
      for(i in names(colnames)){
        cols[[i]] <- input[[paste0("col_", i)]]
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
    
    # Selected Column is Character
    # reactive_col_validation_numeric <- reactive({
    #   posCol <- grep(pattern = "^col", names(input))
    #   cols <- vector(mode = "list", length = length(posCol))
    #   
    #   colnames <- reactiveModel$model@columnMapping@mapping
    #   for(i in names(colnames)){
    #     cols[[i]] <- input[[paste0("col_", i)]]
    #   }
    #   
    #   cols <- unlist(cols)
    #   cols <- cols[cols != " "]
    #   
    #   extradefs <- c(input$iiSelectDose, input$ssSelectDose, input$ssOffsetSelectDose, input$addlSelectDose, input$mdvSelectDose, input$resetSelectDose)
    #   extradefs <- extradefs[extradefs != " "]
    #   
    #   cols <- c(cols, extradefs)
    #   
    #   colclass <- sapply(data[,cols], class)
    #   
    #   validate(
    #     need(!any(colclass == "character"), "Error: Column is class character in input data")
    #   )
    #   
    # })
    # 
    # output$colValidationNumeric <- renderPrint({
    #   reactive_col_validation_numeric()
    # })
    
    # ID coumn mappings are more than 5
    
    # reactive_col_validation_ID <- reactive({
    #   if(!is.null(input$col_id)){
    #   validate(
    #     need(length(input$col_id) < 6, "Error: Cannot specify more than 5 column mappings for 'id'")
    #   )
    #   }
    # })
    # 
    # output$colValidationID <- renderPrint({
    #   reactive_col_validation_ID()
    # })
    
    
    
    ## Cov Levels Labels ----
    output$covLevelsLabels <- renderUI({
      req(reactiveModel$model)
        colnames <- reactiveModel$model@columnMapping@mapping
        covlist <- get_cat_cov(reactiveModel$model@covariateList)
        if(length(covlist) == 0) return()
        

        ui <- list()
         for(i in seq_along(covlist)){
           cov <- names(covlist)[[i]]
           lvl <- get_lvl_from_covlist(covlist, cov)
           lbl <- get_lbl_from_covlist(covlist, cov)
  
           ui[[i]] <- tagList(
             fluidRow(h5(cov)),
             fluidRow(
               column(width = 5,
                      textInput(paste0(cov,"_lvl"), label = paste0(cov, " Levels"), value = lvl)
               ),
               column(width = 5,
                      textInput(paste0(cov,"_lbl"), label = paste0(cov, " Labels"), value = lbl)
               )
            )
          )
        }
        
        tagList(
          div(h4("Categorical Covariates", style = "margin-top:0px; padding-bottom: 5px;"),
              h6("* Levels/Labels required if covariate column is of class character in the data. Leave empty if column is of class numeric.", style = "display: contents;"),
              div(style = "padding: 5px;"),
              ui,
              fluidRow(
                column(width = 4, offset = 8,
                       actionLink("save_lvl_lbl", 
                                  label = list("Save Levels/Labels", 
                                               HTML("&nbsp; &nbsp;"), 
                                               icon("save")), 
                                  style = "font-size: 12px; font-family:Segoe UI Light, Arial, sans-serif;")
                )
              )
          )
        )
    })
        
        
  
    observeEvent(input$save_lvl_lbl,{
      
      covlist <- reactiveModel$model@covariateList
      
      for(i in seq_along(covlist)){
        cov <- covlist[[i]]@name
        type <- covlist[[i]]@type
        
        if(type != 2) {
          next
        } else {
        lvl <- input[[paste0(cov, "_lvl")]]
        lvl <- as.numeric(unlist(strsplit(lvl, ",")))
        lbl <- input[[paste0(cov, "_lbl")]]
        lbl <-  unlist(strsplit(lbl, ","))
        
        stopifnot(length(lvl) == length(lbl))
        
        for(j in seq_along(lvl)){
          covlist[[i]]@covarItems[[j]] <- NlmeCovarItem(name = lbl[[j]],
                                                        value = lvl[[j]])
            
          }
        }
        
        }
      
      reactiveModel$model@covariateList <- covlist
      
      showNotification(
        "Levels/Labels have been updated in the model.",
        duration = 3,
        closeButton = TRUE,
        type =  "message"
      )
      
    })
    
    
    
    observeEvent(input$updateColsOut, {
      # req(output$covLevelsLabels)
      # colnames <- reactiveModel$model@columnMapping@mapping
      # reactiveLevelLabel$levels <- vector(mode = "list", length = length(colnames))
      # reactiveLevelLabel$labels <- vector(mode = "list", length = length(colnames))
      # 
      # for(i in names(colnames)){
      #    if(colnames[[i]]@variableType$type == "covariate" &&
      #       colnames[[i]]@variableType$covType == 2 &&
      #       colnames[[i]]@columnName != "?" &&
      #       is.character(data[,colnames[[i]]@columnName])){
      #       req(input[[paste0("covLevels", i)]])
      #       req(input[[paste0("covLabels", i)]])
      #       if(input[[paste0("covLevels", i)]] != "" && input[[paste0("covLabels", i)]] != ""){
      #     reactiveModel$model <- Certara.RsNLME::addLabel(reactiveModel$model,
      #                         covariate = colnames[[i]]@variableName,
      #                         levels = as.numeric(unlist(strsplit(input[[paste0("covLevels", i)]], split = ","))),
      #                         labels = trimws(unlist(strsplit(input[[paste0("covLabels", i)]], split = ","))))
      #     reactiveLevelLabel$levels[[i]] <- input[[paste0("covLevels", i)]]
      #     reactiveLevelLabel$labels[[i]] <- input[[paste0("covLabels", i)]]
      #       
      #       }
      #   }
      # }
      
      shinyAce::updateAceEditor(
        session,
        "outcols",
        theme = input$theme,
        mode = "r",
        tabSize = 4,
        useSoftTabs = FALSE,
        showInvisibles = FALSE,
        showLineNumbers = TRUE,
        value = paste0(c(unlist(writeColsOut(reactiveModel$model)), ""), collapse = "\n"))
    })
    

    
    ## Data ----
    output$userData <- DT::renderDataTable(options = list(scrollX = TRUE), {
      data
    })
    
    

    ##  Dosing Cycle ----
    ### SS Col Def and SS Dose Cycle Observer ----
    observe({
      if(doseCycleSS){
        #shinyjs::disable(id = "isSSDoseCycle")
        shinyjs::disable(id = "isSS")
      }
      if (doseCycleADDL) {
        shinyjs::disable(id = "isADDL")
      }
    })
    # 
    # # ADDL Col Def and ADDL Dose Cycle Observer
    # observe({
    #   if(length(baseModel@extraDoses) > 0){
    #     shinyjs::disable(id = "isADDLDoseCycle")
    #     shinyjs::disable(id = "isADDL")
    #   } else {
    #     if (input$isADDL == TRUE) {
    #       shinyjs::disable(id = "isADDLDoseCycle")
    #     } else {
    #       shinyjs::enable(id = "isADDLDoseCycle")
    #   }
    #     if (input$isADDLDoseCycle == TRUE) {
    #       shinyjs::disable(id = "isADDL")
    #     } else {
    #       shinyjs::enable(id = "isADDL")
    #     }
    #   }
    # })

    
    
    # Steady State ---###---###---###---
    
    # reactiveDoseNames <- reactiveValues(value = c())
    # 
    # observe({
    #   #req(model())
    #   reactiveDoseNames$value <- doseNames(reactiveModel$model)
    # })
    # 
    # counter_panels_ss <- 1
    # 
    # observeEvent(input$addSSBtn, {
    #   current_id_ss <- paste0("ss_panel_", counter_panels_ss)
    #   ss_panel(current_id_ss, doseName = reactive(reactiveDoseNames$value), data = data)
    #   insertUI(
    #     selector = "#add_SS_here",
    #     where = "beforeBegin",
    #     ui = ss_panel_ui(current_id_ss)
    #   )
    # 
    #   # update counter
    #   counter_panels_ss <<- counter_panels_ss + 1
    # })
    # 
    # #Total Cycle Time Output - SS
    # 
    #   output$timeTotalSS <- renderPrint({
    #   constantSSII <- names(input)[grepl("ssIIConstant", names(input))]
    #   cat(paste0("Cycle Time = "), sum(sapply(constantSSII, function(x) as.numeric(input[[x]])), na.rm = T))
    #    })

    
    
    # ADDL ---###---###---###---

    # counter_panels_addl <- 1
    # 
    # observeEvent(input$addADDLBtn, {
    #   current_id_addl <- paste0("addl_panel_", counter_panels_addl)
    #   addl_panel(current_id_addl, doseName = reactive(reactiveDoseNames$value), data = data)
    #   insertUI(
    #     selector = "#add_ADDL_here",
    #     where = "beforeBegin",
    #     ui = addl_panel_ui(current_id_addl)
    #   )
    # 
    #   # update counter
    #   counter_panels_addl <<- counter_panels_addl + 1
    # })
    # 
    # # Total Cycle Time Output - SS
    # 
    #     output$timeTotalADDL <- renderPrint({
    #       constantADDLII <- names(input)[grepl("addlIIConstant", names(input))]
    #       cat(paste0("Cycle Time = "), sum(sapply(constantADDLII, function(x) as.numeric(input[[x]])), na.rm = T))
    #     })
        
        
        
    # Extra Col Def ---###---###---###---

    # counter_panels_extradef <- 1
    # 
    # 
    # panels_on_startup <- length(exUserDoseCycle)
    # 
    # startupSelectedUserDefs <- list(
    #   doseCycles = exUserDoseCycle
    # )
    # # 
    # # # add counters on startup
    # lapply(seq_len(panels_on_startup), function(i) {
    #   current_id <- paste0("panel_", counter_panels_extradef)
    #   extradef_panel(current_id,  value = startupSelectedUserDefs$doseCycles[i])
    #   insertUI(selector = "#add_extradef_here",
    #             where = "afterBegin",
    #            ui = extradef_panel_ui(current_id))
    # 
    #   # update counter
    #   counter_panels_extradef <<- counter_panels_extradef + 1
    # })
    # 
    # 
    # observeEvent(input$addExtraDefBtn, {
    #   current_id_extradef <- paste0("extradef_panel_", counter_panels_extradef)
    #   extradef_panel(current_id_extradef)
    #   insertUI(selector = "#add_extradef_here",
    #            where = "afterBegin",
    #            ui = extradef_panel_ui(current_id_extradef))
    #   
    #   # update counter
    #   counter_panels_extradef <<- counter_panels_extradef + 1
    # })


    
    ## PML Editor ----
    # updating ace with options
    observeEvent(c(input$enableAutocomplete, input$theme), {
      #req(model())
      if (input$enableAutocomplete) {
        if (input$enableLiveCompletion) {
          autoComplete <- "live"
        } else {
          autoComplete <- "enabled"
        }
      } else {
        autoComplete <- "disabled"
      }


      shinyAce::updateAceEditor(
        session,
        "ace",
        theme = input$theme,
        mode = "pml",
        tabSize = 4,
        useSoftTabs = FALSE,
        showInvisibles = FALSE,
        showLineNumbers = TRUE,
        autoComplete = autoComplete,
        autoCompleters = c("text", "static", "keyword"),
        autoCompleteList = pml_wordlist)
    })

    pmlinput <- eventReactive(input$ace,{
      isolate({
        input$ace
       })
    })
    
    observeEvent(pmlinput(), {
      writeLines(pmlinput(), con = pmlfile_out)
      if (syntaxcheck) {
        # suppressing warnings since in case of TDL errors it is expected
        tdllog <- .check_syntax(pmlfile_out, TDLLocation)

        if (all(nchar(tdllog) == 0)) {
          tdllog <- "No warnings or errors"
          warningcolor <- "#235C30"
        } else {
          warningcolor <- "#FF0000"
        }

        if (pmlinput() != "") {
          reactiveModel$model@statements <- as.list(pmlinput())
          newPML <- .remove_comments(pmlinput())
          reactiveModel$model <- Certara.RsNLME::parsePMLColMap(reactiveModel$model)
          if (!baseModel@isTextual) {
            reactiveModel$model@isTextual <- !identical(basePML, newPML)
          }
       
        }

        output$tdllog <-
          renderUI(HTML(paste0('<pre><font color=\"', warningcolor, '\">', tdllog, "</font></pre>")))
      } else {
        tdllog <- c("Syntax check and column mapping update disabled, since TDL5 was not found")
        output$tdllog <-
          renderUI(HTML(paste0('<pre><font color=\"#FF0000\">', tdllog, "</font></pre>")))
      }
    }, priority = 10)
    
    
    
    ## Exit Shiny ----
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
      message("Shiny session has ended, the model has been updated")
      session$sendCustomMessage(type = "shinymaterialJS", js$closewindow())
      stopApp(isolate({reactiveModel$model}))
    })
    
    observeEvent(input$exitCancel, {
      message("Shiny session has been canceled by the user, the model has not been updated")
      session$sendCustomMessage(type = "shinymaterialJS", js$closewindow())
      stopApp(baseModel)
    })
    
  }
  
  # UI ----
  ui <- tagList(
    ## 1.0 ShinyJS ----
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      text = jsFunctions,
      functions = c("closewindow")),
    
    ## 2.0 Page ----
    bslib::page_fluid(class = "certara-page",
      tags$head(tags$style(styleCSS)),
      title = "Model Builder",
      
      ### 3.0 Header ----
      certara_header("Model Builder"),
    
      ### 3.0 Body ----
      div(class = "model-content",
      bslib::layout_column_wrap(
        width = NULL,
        style = htmltools::css(grid_template_columns = "2fr 3fr"),
      
        #### 5.0 Left Window Pane ----
        bslib::card(
          bslib::card_body(

            ##### 5.1 Code Window ----
            shinyAce::aceEditor(
              outputId = "ace",
              selectionId = "selection",
              autoScrollEditorIntoView = TRUE,
              minLines = 5,
              maxLines = 35,
              value = paste0(c(unlist(baseModel@statements), ""), collapse = "\n"),
              readOnly = FALSE,
              placeholder = "PML function definition required"
            ),
            
            uiOutput("tdllog"),
            
            ##### 5.2 Options Accordion ----
            bslib::accordion(
              open = FALSE,
              class = "textual-options",
              bslib::accordion_panel(
                title = "Options",
                selectInput(
                  inputId = "theme",
                  label = "Theme: ",
                  choices = themes,
                  selected = "dreamweaver" 
                ),
                checkboxInput(inputId = "enableAutocomplete", label = "Enable AutoComplete (Ctrl+Space)", value = TRUE),
                conditionalPanel(
                  "input.enableAutocomplete",
                  checkboxInput(inputId = "enableLiveCompletion", label = "Live auto completion", value = FALSE)
                )
              )
            )
          )
        ),
        
        #### 6.0 Right Window Pane ----
        bslib::navset_card_underline(
        
          ##### 6.1 Column Mapping Tab ----
          bslib::nav_panel(
            title = "Column Mapping",
            bslib::card_body(
              fluidRow(
                bslib::card(
                  fluidRow(
                    ###### 6.1a Dropdowns ----
                    column(width = 5,
                           h4("Column Mapping", style = "margin-top:0px; padding-bottom: 10px;"),
                           uiOutput("columnMap"),
                           conditionalPanel("input.reset == true",
                                            div(style = "padding: 3px;"),
                                            selectInput(
                                              inputId = "resetSelectDose",
                                              label = "Reset *",
                                              choices = c(" ", names(data)),
                                              selected = ifelse(length(exReset) > 0, exReset$col, " ")
                                            )
                           ),
                           conditionalPanel("input.mdv == true",
                                            div(style = "padding: 3px;"),
                                            selectInput(
                                              inputId = "mdvSelectDose",
                                              label = "MDV *",
                                              choices = c(" ", names(data)),
                                              selected = ifelse(length(exMDV) > 0, exMDV, " ")
                                            )
                           ),
                           conditionalPanel("input.isADDL == true",
                                            div(style = "padding: 3px;"),
                                            selectInput(
                                              inputId = "addlSelectDose",
                                              label = "ADDL *",
                                              choices = c(" ", names(data)),
                                              selected = ifelse(length(exADDL) > 0, exADDL, " ")
                                            )
                           ),
                           conditionalPanel("input.isADDL == true || input.isSS == true",
                                            div(style = "padding: 3px;"),
                                            selectInput(
                                              inputId = "iiSelectDose",
                                              label = "II *",
                                              choices = c(" ", names(data)),
                                              selected = ifelse(length(exII) > 0, exII, " ")
                                            )
                           ),
                           conditionalPanel("input.isSS == true",
                                            div(style = "padding: 3px;"),
                                            selectInput(
                                              inputId = "ssSelectDose",
                                              label = "SteadyState *",
                                              choices = c(" ", names(data)),
                                              selected = ifelse(length(exSS) > 0, exSS, " ")
                                            )
                           ),
                           conditionalPanel("input.isSSOffset == true && input.isSS == true",
                                            div(style = "padding: 3px;"),
                                            selectInput(
                                              inputId = "ssOffsetSelectDose",
                                              label = "SSOffset *",
                                              choices = c(" ", names(data)),
                                              selected = ifelse(length(exSSOff) > 0, exSSOff, " ")
                                            )
                           ),
                           uiOutput("colValidationDup"),
                           uiOutput("colValidationNumeric"),
                           uiOutput("colValidationID")
                    ), 
                    
                    ###### 6.1b Code Generator ----
                    column(width = 7,
                           fluidRow(
                             uiOutput("covLevelsLabels"),
                             h4("Column Definition File", style = "padding-bottom: 10px;"),
                             shinyAce::aceEditor(
                               outputId = "outcols",
                               autoScrollEditorIntoView = TRUE,
                               minLines = 5,
                               maxLines = 35,
                               value = NULL,
                               readOnly = TRUE
                             )
                             
                             # This was here before the bslib overhaul - MT 6/11/2024
                             
                             # material_collapsible(
                             #   depth = 0, color = NULL,     
                             #   material_collapsible_item(label = "User Defined Column Definitions", active = TRUE,
                             #                             material_row(
                             #                               material_column(width = 1, 
                             #                                               div(style = "padding-bottom: 17px; margin-left:-7px",
                             #                                                   shinyWidgets::actionBttn("addExtraDefBtn", label = NULL, icon = shiny::icon("plus"), style = "material-circle", size = "xs")
                             #                                               )
                             #                                               
                             #                               )
                             #                             ),
                             #                             div(id = "add_extradef_here")
                             #                             
                             #                             
                             #   )
                             # )
                           ),
                           fluidRow(
                             actionButton(inputId = "updateColsOut", label = "Preview", width = "100%")
                           )
                    )
                  )
                )
              )
            )
          ),
        
          ##### 6.2 Input Options Tab ----
          bslib::nav_panel(
            title = "Input Options",
            bslib::card_body(
              fluidRow(
                bslib::card(
                  bslib::card_title("Input Options"),
                  fluidRow(
                    column(width = 12,
                           
                      ###### 6.2a Reset ----
                      checkboxInput(inputId = "reset", label = "Reset", value = ifelse(length(exReset) > 0, TRUE, FALSE)),
                      conditionalPanel("input.reset == true",
                                       fluidRow(
                                         column(width = 2,
                                                numericInput(inputId = "resetLow", min = 0, max = 1000, label = "Low", value = ifelse(length(exReset) > 0, exReset$low, 4))
                                         ),
                                         column(width = 2,
                                                numericInput(inputId = "resetHi", min = 1, max = 1000, label = "High", value = ifelse(length(exReset) > 0, exReset$hi, 4))
                                         )
                                       )
                      ),
                      
                      ###### 6.2b MDV ----
                      checkboxInput(inputId = "mdv", label = "MDV", value = ifelse(length(exMDV) > 0, TRUE, FALSE)),
                      
                      ###### 6.2c Steady State ----
                      checkboxInput(inputId = "isSS", label = "Steady State (Column Definition)", value = ifelse(length(exSS) > 0, TRUE, FALSE)),
                      conditionalPanel("input.isSS == true",
                                       fluidRow(
                                         column(width = 12, offset = 1,
                                                checkboxInput(inputId = "isSSOffset", label = "SS = 2", value = ifelse(length(exSSOff) > 0, TRUE, FALSE))
                                         )
                                       )
                      ),
                      
                      ###### 6.2d ADDL ----
                      checkboxInput(inputId = "isADDL", label = "ADDL (Column Definition)", value = ifelse(length(exADDL) > 0, TRUE, FALSE)),
                      
                      ###### 6.3e Infusion ----
                      uiOutput("infusion")
                      
                      # This was here before the bslib overhaul - MT 6/11/2024
                      
                      # material_checkbox("isSSDoseCycle", label = "Steady State (Dose Cycle)", initial_value = FALSE),
                      # material_checkbox("isADDLDoseCycle", label = "ADDL (Dose Cycle)", initial_value = FALSE)
                    )
                  )
                  # conditionalPanel(
                  #   "input.isSSDoseCycle == true",
                  #   material_row(
                  #     material_column(
                  #       width = 12,
                  #       material_collapsible(
                  #         depth = 0, color = NULL,
                  #         material_collapsible_item(
                  #           label = "Steady State", active = TRUE,
                  #           div(id = "add_SS_here"),
                  #           material_row(
                  #             material_column(
                  #               width = 1, offset = 11,
                  #               div(
                  #                 style = "padding-bottom: 17px; margin-left:-7px",
                  #                 shinyWidgets::actionBttn("addSSBtn", label = NULL, icon = shiny::icon("plus"), style = "material-circle")
                  #               ),
                  #             )
                  #           ),
                  #           verbatimTextOutput("timeTotalSS"),
                  #         )
                  #       )
                  #     )
                  #   )
                  # ),
                  # conditionalPanel(
                  #   "input.isADDLDoseCycle == true",
                  #   material_row(
                  #     material_column(
                  #       width = 12,
                  #       material_collapsible(
                  #         depth = 0, color = NULL,
                  #         material_collapsible_item(
                  #           label = "ADDL", active = TRUE,
                  #           div(id = "add_ADDL_here"),
                  #           material_row(
                  #             material_column(
                  #               width = 1, offset = 11,
                  #               div(
                  #                 style = "padding-bottom: 17px; margin-left:-7px",
                  #                 shinyWidgets::actionBttn("addADDLBtn", label = NULL, icon = shiny::icon("plus"), style = "material-circle")
                  #               ),
                  #             )
                  #           ),
                  #           verbatimTextOutput("timeTotalADDL")
                  #         )
                  #       )
                  #     )
                  #   )
                  # )
                )
              )
            )
          ),
          
          ##### 6.3 Data Tab ----
          bslib::nav_panel(
            title = "Data",
            bslib::card_body(
              fluidRow(
                bslib::card(
                  bslib::card_title("Input Data"),
                  full_screen = TRUE,
                  div(
                    style = "margin: 1.1rem;",
                    DT::dataTableOutput("userData")
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    ### 7.0 Footer ----
    certara_footer(url = 'https://certara.github.io/R-RsNLME-model-builder/')
    
    )
  )


  runApp(
    shinyApp(ui = ui, server = server),
    launch.browser = TRUE
  )
}
