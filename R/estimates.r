#' Shiny GUI to examine the model and evaluate estimates for fixed effects.
#'
#' Shiny GUI to examine the model and evaluate estimates for fixed effects.
#'
#' @param model        Model object.
#' @param host         Optional host parameter of class \code{hostParams}.
#' If \code{NULL}, local host will be used.
#'
#' @examples
#' if (interactive()) {
#' library(Certara.RsNLME)
#' host <- hostParams(
#'   parallelMethod = "None",
#'   hostName = "local",
#'   numCores = 1
#' )
#'
#' model <- pkmodel(
#'   parameterization = "Clearance",
#'   absorption = "Intravenous",
#'   numCompartments = 2,
#'   data = pkData,
#'   ID = "Subject",
#'   A1 = "Amount",
#'   CObs = "Conc",
#'   Time = "Act_Time",
#'   modelName = "pk_model"
#' )
#'
#' model <- estimatesUI(model, host)
#' }
#' 
#' @return A model object of class \code{NlmePmlModel}
#' @export
estimatesUI <- function(model, host = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package \"shiny\" required to use estimatesUI()",
         call. = FALSE)
  }

  stopifnot(inherits(model, "NlmePmlModel"))

  if (is.null(model@inputData)) {
    stop("Input data not found in model object")
  }
  
  # App Setup ----
  wd <- tempdir(TRUE)
  modelName <- model@modelInfo@modelName
  fsep <- ifelse(.Platform$OS.type == "unix", "/", "\\")
  modelDir <- file.path(wd, modelName, fsep = fsep)
  if (!dir.exists(modelDir)) {
    modelDir <- .prepare_wd(modelDir)
  }

  dataset <- model@dataset

  mf <- dataset@modelFile
  cf <- dataset@colDefFile
  df <- dataset@dataFile

  writeInputData(model, df, modelDir)
  ColumnMapping1 <- writeColumnMapping(model = model,
                     filename = cf,
                     workingDir = modelDir)
  writeModelStatements(model, mf, modelDir)

  # checkhost
  if (is.null(host)) {
    host <- Certara.RsNLME::NlmeParallelHost(
      sharedDirectory = modelDir,
      installationDirectory = Sys.getenv("INSTALLDIR"),
      parallelMethod = Certara.RsNLME::NlmeParallelMethod("None"),
      hostName = "Local",
      numCores = 1
    )
  } else {
    stopifnot(inherits(host, "NlmeParallelHost"))
  }

  if (!Certara.RsNLME::checkHostParams(host)) {
    stop("Check host specification. See warnings below:", call. = FALSE)
  }

  if (!compileModel(model, host, modelDir)) {
    return(FALSE)
  }

  mname <- deparse(substitute(model, env = environment()))

  colMap <- model@columnMapping@mapping

  idCols <- colMap$id@columnName
  idColsVector <- unlist(strsplit(idCols, split = "\\W*,\\W*"))

  mdata <- as.data.frame(model@inputData)

  if (model@isTimeBased) {
    TimecolName <- lookupColname(colMap, "time")
    data <- mdata[c(idColsVector, TimecolName)]
    # the following DF is merged with the Subject input

    dataStringIDs <-
      cbind(apply(data[idColsVector], 2, function(x) {
        trimws(as.character(x))
      }),
      data[TimecolName])
  } else {
    data <- mdata[c(idColsVector)]
    dataStringIDs <-
      data.frame(apply(data[idColsVector], 2, function(x) {
        trimws(as.character(x))
      }))
  }

  subjectIds <-
    apply(unique(dataStringIDs[idColsVector]), 1, paste0, collapse = ", ")
  names(subjectIds) <- subjectIds
  numSubjects <- length(subjectIds)

  modelInfo <- Certara.RsNLME::createModelInfo(model, ForceRun = TRUE)

  # get thetas
  splittedFixefsStrings <- .get_fixefStrings(model, modelInfo)
  thetas <-
    .transform_FixefsStringsToThetas(splittedFixefsStrings, Part = "Value")
  
  FrozenThetas <-
    as.logical(as.numeric(unlist(
      .transform_FixefsStringsToThetas(splittedFixefsStrings, Part = "Freeze")
    )))
  numThetas <- length(thetas)
  
  PMLCode <- paste(unlist(model@statements), collapse = "\n")
  PMLCode <- .clean_PML(PMLCode)
  FixefValuesBounds <- 
    .parse_Fixefs(CustomCodeToSearch = PMLCode, Statement = "fixef")
  FixefValuesBounds <- data.frame(FixefValuesBounds)
  AllowPositiveLower <- FixefValuesBounds["Lower",] < 0
  AllowPositiveUpper <- FixefValuesBounds["Upper",] > 0
  
  statement_string <- paste0(modelInfo, collapse = '|')

  # get stparms
  st_params <-
    .get_rowValuesModelInfo(statement_string, lookupString = "sparms")

  # get observations
  observations <-
    .extract_ResponsesObservations(statement_string,
                                   "observations")

  # get assigned
  assigned <-
    .get_rowValuesModelInfo(statement_string, lookupString = "assigned")

  # get epsilons
  epsilons <- .get_rowValuesModelInfo(statement_string,
                                      lookupString =  "error",
                                      lookupPattern = "[a-zA-Z\\d_]+")

  # get integrators
  integrators <-
    .get_rowValuesModelInfo(statement_string, lookupString = "integrator")

  # get observe statements
  statements <- .remove_comments(model@statements)
  observeStatements <- unlist(regmatches(
    statements,
    gregexpr(
      "(?<=observe)(?=\\()(?:(?=.*?\\((?!.*?\\1)(.*\\)(?!.*\\2).*))(?=.*?\\)(?!.*?\\2)(.*)).)+?.*?(?=\\1)[^(]*(?=\\2$)",
      statements,
      perl = TRUE
    )
  ))
  
  # get covariates
  Covariates <- c()
  continouousCovariates <- c()

  AllCovariates <- .get_rowValuesModelInfo(statement_string,
                                        lookupString = "covariates",
                                        lookupPattern = "(?:[^()]*|\\([^()]*\\))*(?=\\))")

  Covariates <- gsub("\\(|\\)", "", AllCovariates)
  continouousCovariates <- Covariates[!grepl("\\(|\\)", AllCovariates)]

  observationNames <- character(0)
  if (length(observeStatements) > 0) {
    observeWords <- unlist(regmatches(
      observeStatements,
      gregexpr("[^\\W_]+(?:['_-][^\\W_]+)*", observeStatements, perl = TRUE)
    ))
    # filtering
    observeWordsFiltered <-
      observeWords[!observeWords %in% c(names(thetas),
                                        st_params,
                                        Covariates,
                                        observations,
                                        epsilons)]
    # look for names of predictables in integrators and assigned
    observationNames <-
      observeWordsFiltered[observeWordsFiltered %in% c(assigned, integrators)]
    observationNames <- unique(observationNames)
  }

  if (length(observationNames) == 0) {
    stop(
      "There are no continuous observed variables (defined through observe statement) in the model.",
      "There is no y-variable to plot.",
      "\nNeither observed variables defined through LL statement nor discontinuous observed variables are supported."
    )
  }

  if (model@isTimeBased) {
    independentVarNames <- c("time", continouousCovariates, observationNames)

    # get the first subject
    Subject <- data[idColsVector][1,]
    SubjectTimeDF <- merge(Subject, data)

    defStartTime <- min(SubjectTimeDF[TimecolName])
    defEndTime <- max(SubjectTimeDF[TimecolName])
  } else {
    # first word in observe statements is observation, the second could be an x variable
    # only then we can build the plot
    LHS <- sapply(strsplit(observeStatements, "="), '[[', 1)
    LHSindependent <- unlist(regmatches(LHS,
                                  gregexpr("\\w+(?=\\))", LHS, perl = TRUE)))

    if (length(continouousCovariates) == 0) {
      stop("There are no independent variables in the model (i.e. observe(Eobs(C) = ...) ",
           "\nThere's no x variable to plot.")
    }

    independentVarNames <- LHSindependent

    defStartTime <- 0
    defEndTime <- 25
  }

  # preparing the dose table
  Dosepoints <- .get_rowValuesModelInfo(statement_string, lookupString = "dosepoints1")
  Dosepoints2 <- .get_rowValuesModelInfo(statement_string, lookupString = "dosepoints2")

  if (length(Dosepoints2) > 0) {
    warning("Alternative dose table is not enabled since 'dosepoint2()' statement is detected.",
            call. = FALSE,
            immediate. = TRUE)
  }
  
  DoseTableBuilt <- 
    length(Dosepoints) > 0 && 
    length(Dosepoints2) == 0 && 
    model@isTimeBased
  
  if (DoseTableBuilt) {
    DoseTableInitial <-
      data.frame(lapply(1:(4 + length(Dosepoints)*2), function(x) 0))
    DosepointsWithDur <- paste0(Dosepoints, "\nDuration")
    colnames(DoseTableInitial) <- c("Time", c(rbind(Dosepoints, DosepointsWithDur)), "II", "SS", "ADDL")
  } else {
    DoseTableInitial <- data.frame()
  }
  
  ODEs <- c("Stiff", #1
            "Non-stiff DVERK", #3
            "Auto-detect", #4
            "Matrix Exponent", #6
            "Non-stiff DOPRI5", #7
            "Exponent Higham") #8
  
  # UI ----
  ui <- tagList(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jsFunctions,
                           functions = c("closewindow")),
    tags$head(tags$style(styleCSS)),
    
    ## 1.0 Header ----
    certara_header("Initial Estimates"),
    
    ## 2.0 Page ----
    bslib::page_sidebar(
      id = "main_nav",
      title = NULL,
      window_title = "Initial Estimates",

      ## 3.0 Sidebar ----
      sidebar = bslib::sidebar(
        width = 400,
        open = TRUE,
        class = "sidebar",
        ### 3.1 Subject / Overlay Inputs ----
        fluidRow(class = "multi-input-with-checkbox",
          column(
            width = 6,
            uiOutput("subject")
          ),
          column(class = "col-checkbox",
            width = 6,
            checkboxInput("overlay", "Overlay", value = FALSE)
          )
        ),
        
        ### 3.2 Facet / Log / Page Inputs ----
        fluidRow(
          column(
            width = 6,
            checkboxInput("facet", "Facet Ind", value = FALSE)
          ),
          column(
            width = 6,
            checkboxInput("log", "Log", value = FALSE)
          ),
          conditionalPanel(
            "input.facet == true",
            column(style = "padding-left: inherit; padding-right: inherit;",
              width = 6,
              uiOutput("page")
            )
          )
        ),
        
        ### 3.3 Conditional Facet Inputs ----
        fluidRow(
          conditionalPanel(
            "input.facet == true",
            column(style = "padding-left: inherit; padding-right: inherit;",
              width = 6,
              numericInput(
                "nrow",
                label = "N rows",
                value = 1,
                min = 1
              )
            ),
            column(style = "padding-left: inherit; padding-right: inherit;",
              width = 6,
              numericInput(
                "ncol",
                label = "N cols",
                value = 1,
                min = 1
              )
            )
          )
        ),
        
        ### 3.4 Obs. / Ind. Variable Inputs ----
        fluidRow(
          column(width = 6,
                 uiOutput("observation")),
          column(width = 6,
                 uiOutput("independent_var"))),
        
        ### 3.5 Start Time / Duration / Simulation / Thetas Inpnuts ----
        fluidRow(
          column(
            width = 12,
            uiOutput("start"),
            div(style = "padding: 5px"),
            uiOutput("dur"),
            div(style = "padding: 5px"),
            uiOutput("simpoints"),
            div(style = "padding: 5px"),
            uiOutput("thetas")
          )
        ),
        
        ### 3.6 ODE and optimization ----
        fluidRow(
          style = "display: flex; align-items: baseline;",
          column(
            style = "align-items: flex-end;",
            width = 7,
            shinyWidgets::pickerInput(inputId = "ODE", 
                                      label = "ODE solver", 
                                      choices = ODEs, 
                                      selected = "Matrix Exponent",
                                      width = "fit",
                                      inline = TRUE,
                                      options = list(container = "body"))
          ),
          column(
            style = "align-items: flex-end;",
            width = 5,
            bslib::input_task_button(
              id = "Optimize",
              label = "Fit",
              label_busy = "Fitting..."
            )
          )
        ),
        
        ### 3.7 Dose table ----
        conditionalPanel(condition = "output.DoseTableReady",
                         bslib::accordion(
                           class = "estimates-accordion",
                           bslib::accordion_panel(
                             title = "Alternative Dose Table",
                             open = FALSE,
                             fluidRow(class = "estimates-dosetable",
                                      tags$head(tags$style(HTML("table {table-layout: fixed;}"))),
                                      column(width = 12, DT::DTOutput('DoseTable'))),
                             fluidRow(
                               class = "multi-input-with-checkbox",
                               column(
                                 class = "col-checkbox",
                                 width = 6,
                                 checkboxInput("useDoseTable", "Use Dose Table", value = FALSE)
                               ),
                               column(
                                 width = 3,
                                 numericInput(
                                   "NRows",
                                   "NRows:",
                                   value = 1,
                                   min = 1,
                                   step = 1,
                                   width = "100%"
                                 )
                               ),
                               column(
                                 width = 3,
                                 numericInput(
                                   "DoseTableSize",
                                   "Table Size:",
                                   value = 11,
                                   min = 1,
                                   max = 30,
                                   step = 1,
                                   width = "100%"
                                 )
                               )
                             )
                           ),
                           open = FALSE
                         ))
        #,div(style = "padding: 17px")
      ), 
      
      ## 4.0 Main Panel ----
      bslib::card(style = "width: 100%; height: 100%;",
                  full_screen = TRUE,
                  bslib::card_body(plotOutput("plot", height = "100%", width = "100%")))),
    
    ## 5.0 Footer ----
    certara_footer(url = 'https://certara.github.io/R-RsNLME-model-builder/')
  )
  
  # Server ----
  server <- function(input, output, session) {
    
    ## 1.0 Sidebar Inputs ----
    
    ### 1.1 Subject ----
    output$subject <- shiny::renderUI({
      selectInput(
        "Subject",
        label = "Subject",
        choices = subjectIds
      )
    })
    
    ### 1.2 Page ----
    output$page <- shiny::renderUI({
      selectInput(
        "page",
        label = "Page",
        choices = seq(1, ceiling(
          length(subjectIds) / input$nrow / input$ncol)
        ),
        selected = 1
      )
    })
    
    ### 1.3 Observation ----
    output$observation <- shiny::renderUI({
      selectInput(
        "Observation",
        label = "Observation",
        choices = observationNames
      )
    })
    
    ### 1.4 Ind. Variable ----
    output$independent_var <- shiny::renderUI({
      selectInput(
        "independent_var",
        label = "Ind. Variable",
        choices = independentVarNames
      )
    })
    
    ### 1.5 Start Time ----
    output$start <- renderUI({
      uiStart <- tagList(
        textInput(
          inputId = "num_starttime",
          label = "Start Time",
          value = defStartTime
        ),
        verbatimTextOutput("value_starttime"),
        sliderInput(
          inputId = "starttime",
          label = NULL,
          min = 0,
          max = .get_sliderMax(defStartTime, defEndTime),
          value = defStartTime
        )
      )
      
      uiStart
    })
    
    ### 1.6 Duration ----
    output$dur <- renderUI({
      uiDur <- tagList(
        textInput(
          inputId = "num_duration",
          label = "Duration",
          value = defEndTime - defStartTime
        ),
        verbatimTextOutput("value_duration"),
        sliderInput(
          inputId = "duration",
          label = NULL,
          min = ifelse(
            defEndTime - defStartTime < 1,
            (defEndTime - defStartTime)  /  2,
            1
          ),
          max = (defEndTime - defStartTime) * 2 + 1,
          value = defEndTime - defStartTime
        )
      )
      
      uiDur
    })
    
    ### 1.7 Simpoints ----
    output$simpoints <- renderUI({
      uiSim <- tagList(
        sliderInput(
          "simpoints",
          label = "Simpoints Number",
          min = 100,
          max = 10000,
          value = 500,
          step = 1
        ),
      )
      
      uiSim
    })
    
    observeEvent(
      input$simpoints,
      {
        updateSliderInput(session, "simpoints", max = input$simpoints * 2)
      }, ignoreInit = T)
    
    ### 1.8 Thetas output ----
    output$thetas <- shiny::renderUI({
      isolate({
        uiTheta <- vector(mode = "list", length = length(thetas))
        for (thetaIndex in seq_along(thetas))
        {
          ThetaName <- names(thetas)[thetaIndex]
          ThetaVal <- as.numeric(thetas[[thetaIndex]])
          validate(need(
            !is.null(ThetaVal) && !is.na(ThetaVal),
            paste("Theta", ThetaName, "is not numeric")
          ))
          
          numThetaIndexName <- paste0("num_theta", thetaIndex)
          stepThetaIndexName <- paste0("step_theta", thetaIndex)
          
          validationThetaIndexName <-
            paste0("valid_theta", thetaIndex)
          checkboxTheta <- paste0("check_theta", thetaIndex)
          
          Lower <- FixefValuesBounds["Lower", thetaIndex]
          Upper <- FixefValuesBounds["Upper", thetaIndex]
          
          step <- .step_sliders(ThetaVal, Lower, Upper)
          
          checkboxThetaLabel <- "Positive"
          
          labelClass <- ""
          checkboxClass <- "col-checkbox"
          inputClass <- "allowed-input"
          if (FrozenThetas[thetaIndex]) {
            checkboxThetaLabel <- paste0("Frozen")
            inputClass <- "frozen-input"
          } else if (!AllowPositiveUpper[thetaIndex]) {
            checkboxThetaLabel <- paste0("UpperBound:", signif(Upper, 2))
          } else if (!AllowPositiveLower[thetaIndex]) {
            checkboxThetaLabel <- paste0("LowerBound:", signif(Lower, 2))
          }
          
          if (checkboxThetaLabel != "Positive") {
            labelClass <- "theta-warning"
            checkboxClass <- paste(checkboxClass, "theta-warning")
          }
          
          uiTheta[[thetaIndex]] <- tagList(
            fluidRow(
              class = "multi-input-with-checkbox",
              column(
                class = inputClass,
                width = 7,
                numericInput(
                  inputId = numThetaIndexName,
                  label = ThetaName,
                  value = ThetaVal,
                  step = step
                )
              ),
              column(
                class = checkboxClass,
                width = 5,
                checkboxInput(
                  inputId = checkboxTheta,
                  label = tags$span(class = labelClass, checkboxThetaLabel),
                  value = FALSE
                )
              )
            ),
            verbatimTextOutput(validationThetaIndexName)
          )
          
        }
        uiTheta
      })
    })
    
    
    ## 2.0 Main Plot ----
    output$plot <- renderPlot({
      req(modelData(), input$independent_var)
      out <- isolate({
        modelData()
      })
      
      output_plot(out, input)
    })

    ### 2.1 Facets event ----
    observeEvent(input$facet, {
      if (input$facet) {
        shinyjs::disable(id = "Subject")
        shinyjs::disable(id = "overlay")
      } else {
        shinyjs::enable(id = "Subject")
        shinyjs::enable(id = "overlay")
      }
    }, ignoreInit = TRUE)

    ### 2.2 Overlay event ----
    observeEvent(
      input$overlay,
      {
        if (model@isTimeBased && !is.null(input$overlay))
        {
          if (input$overlay)
          {
            shinyjs::disable(id = "Subject")
            shinyjs::disable(id = "facet")
            defStartTime <- min(data[TimecolName])
            defEndTime <- max(data[TimecolName])
          } else {
            shinyjs::enable(id = "Subject")
            shinyjs::enable(id = "facet")
            # reset to the 1st subject in the dataset
            Subject <- data[idColsVector][1,]
            SubjectTimeDF <- merge(Subject, data)

            SubjIDDF <-
              t(data.frame(unlist(strsplit(
                input$Subject, ", "
              ))))
            colnames(SubjIDDF) <-
              colnames(data)[1:length(idColsVector)]
            SubjectTimeDF <- merge(SubjIDDF, dataStringIDs)

            defStartTime <- min(SubjectTimeDF[TimecolName])
            defEndTime <- max(SubjectTimeDF[TimecolName])
          }
        }
      },
      ignoreInit = T)

    ### 2.3 Subject event ----
    observeEvent(input$Subject,
                 {
                   if (!is.null(input$overlay) &&
                       !input$overlay && model@isTimeBased)
                   {
                     SubjIDDF <- t(data.frame(unlist(
                       strsplit(
                         input$Subject, ", "
                       ))))
                     colnames(SubjIDDF) <-
                       colnames(data)[1:length(idColsVector)]
                     SubjectTimeDF <- merge(SubjIDDF, dataStringIDs)

                     defStartTime <- min(SubjectTimeDF[TimecolName])
                     defEndTime <- max(SubjectTimeDF[TimecolName])

                     updateTextInput(session,
                                     inputId = "num_starttime",
                                     label = "Start Time",
                                     value = defStartTime)

                     updateSliderInput(
                       session,
                       inputId = "starttime",
                       min = 0,
                       max = .get_sliderMax(defStartTime, defEndTime),
                       value = defStartTime
                     )

                     updateTextInput(
                       session,
                       inputId = "num_duration",
                       label = "Duration",
                       value = defEndTime - defStartTime
                     )

                     updateSliderInput(
                       session,
                       inputId = "duration",
                       min = ifelse(
                         defEndTime - defStartTime < 1,
                         (defEndTime - defStartTime)  /  2,
                         1
                       ),
                       max = (defEndTime - defStartTime) * 2 + 1,
                       value = defEndTime - defStartTime
                     )
                   }
                 },
                 ignoreInit = T)

    ### 2.4 StartTime event ----
    starttime_validation <- reactive({
      validate(need(!is.na(
        as.numeric(
          input$num_starttime
        )), "Please input a number"))
    })

    output$value_starttime <- renderPrint({
      starttime_validation()
    })


    observeEvent(
      input$starttime,
      {
        if (!is.na(as.numeric(input$num_starttime)) &&
            input$num_starttime != input$starttime)
        {
          defStartTime <- input$starttime
          updateSliderInput(session,
                            "starttime",
                            value = defStartTime,
                            max = defStartTime * 2 + 10)
          updateTextInput(session, "num_starttime", value = defStartTime)
        }
      },
      ignoreInit = T,
      priority = 2)

    observeEvent(
      input$num_starttime,
      {
        defStartTime <- as.numeric(input$num_starttime)

        if (!is.na(defStartTime) &&
            defStartTime != input$starttime)
        {
          updateSliderInput(session,
                            "starttime",
                            value = defStartTime,
                            max = defStartTime * 2 + 10)
        }
      },
      ignoreInit = T,
      priority = 1)

    ### 2.5 Duration event ----
    duration_validation <- reactive({
      validate(need(!is.na(
        as.numeric(
          input$num_duration
        )), "Please input a number"))
    })
    output$value_duration <- renderPrint({
      duration_validation()
    })

    observeEvent(
      input$duration,
      {
        req(input$duration)
        if (!is.na(as.numeric(input$num_duration)) &&
            as.numeric(input$num_duration) != input$duration)
        {
          defEndTime <- defStartTime + input$duration
          updateSliderInput(session, "duration", max = as.numeric(input$duration) * 2 + 1)
          updateTextInput(session, "num_duration", value = input$duration)
        }
      },
      ignoreInit = T,
      priority = 2)

    observeEvent(
      input$num_duration,
      {
        req(input$num_duration)
        if (!is.na(as.numeric(input$num_duration))  &&
            as.numeric(input$num_duration) != input$duration)
        {
          updateSliderInput(
            session,
            "duration",
            value = as.numeric(input$num_duration),
            max = as.numeric(input$num_duration) * 2 + 1
          )
        }
      },
      ignoreInit = T,
      priority = 1)

    ## 3.0 Other events ----
    ### 3.1 Thetas init ----
    reactiveThetasNum <-
      reactiveValues(values =
                       list(thetas = setNames(
                         lapply(seq_along(thetas),
                                function(thetaIndex) {
                                  isolate(input[[paste0("num_theta", thetaIndex)]])
                                }),
                         paste0("num_theta", seq_along(thetas))
                       )))
    
    ### 3.2 Fit event ----
    observeEvent(input$Optimize, {
      shinyjs::disable(selector = ".allowed-input")
      thetasPositive <- logical()
      for (thetaIndex in seq_along(thetas)) {
        thetas[[thetaIndex]] <- input[[paste0("num_theta", thetaIndex)]]
        thetasPositive <- c(thetasPositive, input[[paste0("check_theta", thetaIndex)]] )
      }
    
      UnlistedThetas <- unlist(thetas)  
      ThetasOK <- all(!is.null(UnlistedThetas), !is.na(UnlistedThetas), is.numeric(UnlistedThetas))
      if (!ThetasOK) {
        warning("Cannot fit the model since some thetas have non-numeric values.")
        shinyjs::enable(selector = ".allowed-input")
        req(ThetasOK)
      }
      
      tryCatch({
        UpdatedThetas <- optimize_Model(
          modelDir = modelDir,
          ColumnMapping1 = ColumnMapping1,
          thetas = thetas,
          thetasPositive = thetasPositive,
          FixefValuesBounds = FixefValuesBounds,
          DoseTable = reactiveDoseTable$DoseTable,
          useDoseTable = input$useDoseTable,
          ODE = input$ODE,
          fsep = fsep
        )
        
        for (thetaIndex in seq_along(thetas)) {
          ThetaInputName <- paste0("num_theta", thetaIndex)

          updateNumericInput(session = session,
                             inputId = ThetaInputName,
                             value = UpdatedThetas[[thetaIndex]])
        }
      },
      warning = function(cond) {
        warning(conditionMessage(cond))
        showModal(modalDialog(conditionMessage(cond)))
      },
      error = function(cond) {
        warning(conditionMessage(cond))
        showModal(modalDialog(conditionMessage(cond)))
      })
      
      shinyjs::enable(selector = ".allowed-input")
    })
    
    ### 3.3 Dose Table ----
    output$DoseTableReady <-
      reactive({
        DoseTableBuilt
      })
    
    outputOptions(output, "DoseTableReady", suspendWhenHidden = FALSE)
    
    reactiveDoseTable <-
      reactiveValues(DoseTable = DoseTableInitial)
    
    # used in dose table:
    DTJSFunctions <- c(
      "table.on('click', 'td', function(e) { ",
      "  if(e.target.localName != 'input') {",
      "    $(this).dblclick();",
      "  }",
      "});",
      "table.on('key', function(e, datatable, key, cell, originalEvent){",
      "  var targetName = originalEvent.target.localName;",
      "  if(key == 13 && targetName == 'body'){",
      "    $(cell.node()).trigger('dblclick.dt');",
      "  }",
      "});",
      "table.on('keydown', function(e){",
      "  var keys = [9,13,37,38,39,40];",
      "  if(e.target.localName == 'input' && keys.indexOf(e.keyCode) > -1){",
      "    $(e.target).trigger('blur');",
      "  }",
      "});",
      "table.on('key-focus', function(e, datatable, cell, originalEvent){",
      "  var targetName = originalEvent.target.localName;",
      "  var type = originalEvent.type;",
      "  if(type == 'keydown' && targetName == 'input'){",
      "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
      "      $(cell.node()).trigger('dblclick.dt');",
      "    }",
      "  }",
      "});"
    )
    
    output$DoseTable <- DT::renderDT({
      
        DT::datatable(
          reactiveDoseTable$DoseTable,
          rownames = FALSE,
          filter = "none",
          callback = DT::JS(DTJSFunctions),
          extensions = "KeyTable",
          options = list(
            info = FALSE,
            paging = FALSE,
            searching = FALSE,
            ordering = FALSE,
            keys = TRUE,
            autoWidth = FALSE,
            scrollX = TRUE,
            initComplete = htmlwidgets::JS(
              "function(settings, json) {",
              paste0("$(this.api().table().container()).css({'font-size': '", input$DoseTableSize, "pt'});"),
              "}"),
            columnDefs = list(
              list(width = '45px', targets = "_all"),
              #list(width = '80px', targets = which(nchar(colnames(reactiveDoseTable$DoseTable)) < 4, arr.ind = TRUE)),
              #list(width = paste0("'", round(100/NCOL(reactiveDoseTable$DoseTable), 0), "%'"), targets = "_all"),
              list(className = 'dt-center', targets = "_all")
            )
          ),
          selection =
            list(mode = "single", target = "cell"),
          editable = list(target = "cell", numeric = "all")
        ) 
    })
    
    DTproxy <- DT::dataTableProxy('DoseTable')
    
    ### 3.4 NRows event ----
    observeEvent(input$NRows, {
      if (NROW(reactiveDoseTable$DoseTable) < input$NRows) {
        DTNRows <- NROW(reactiveDoseTable$DoseTable)
        reactiveDoseTable$DoseTable[(DTNRows + 1):input$NRows, ] <- 0
      } else if (NROW(reactiveDoseTable$DoseTable) > input$NRows) {
        reactiveDoseTable$DoseTable <-
          reactiveDoseTable$DoseTable[1:input$NRows, ]
      }
    })
    
    observeEvent(input$DoseTable_cell_edit, {
      info <- input$DoseTable_cell_edit
      i <- info$row
      j <- info$col + 1
      v <- info$value
      if (v < 0) {
        warning("Dose table does not support negative values. They will be substituted to 0.")
        reactiveDoseTable$DoseTable[i, j] <-
          DT::coerceValue(0, reactiveDoseTable$DoseTable[i, j])
      } else {
        ColumnName <- colnames(reactiveDoseTable$DoseTable)[j]
        
        reactiveDoseTable$DoseTable[i, j] <-
          DT::coerceValue(v, reactiveDoseTable$DoseTable[i, j])
      }
      
      DT::replaceData(
        DTproxy,
        reactiveDoseTable$DoseTable,
        resetPaging = FALSE,
        rownames = FALSE
      )
      
    })
    
    ### 3.5 Simulation event ----
    modelData <-
      eventReactive(
        c(
          input$Observation,
          input$independent_var,
          input$starttime,
          input$duration,
          input$simpoints,
          reactiveThetasNum$values$thetas,
          input$DoseTable_cell_edit,
          input$useDoseTable,
          input$NRows,
          input$ODE
        ),
        {
          isolate({
            req(input$simpoints)
            sweepStart <- input$starttime
            sweepLength <- input$duration
            numSweepSteps <- input$simpoints

            for (thetaIndex in seq_along(thetas)) {
              if (!FrozenThetas[thetaIndex]) {
                # !is.null(reactiveThetasNum$values$thetas[[thetaIndex]])
                thetas[[thetaIndex]] <- reactiveThetasNum$values$thetas[[thetaIndex]]
              } else {
                thetas[[thetaIndex]] <- input[[paste0("num_theta", thetaIndex)]]
              }
            }

            UnlistedThetas <- unlist(thetas)
            ThetasOK <- all(!is.null(UnlistedThetas), !is.na(UnlistedThetas), is.numeric(UnlistedThetas))
            if (!ThetasOK) {
              shinyjs::enable(selector = ".allowed-input")
              req(ThetasOK)
            }
            
            plotVariables <-
              c(input$independent_var[input$independent_var != "time"], input$Observation)
            
            DoseTable <- reactiveDoseTable$DoseTable
            useDoseTable <- input$useDoseTable
            if (useDoseTable) {
              if (sum(DoseTable$ADDL > 0 |
                      DoseTable$SS > 0) != sum((DoseTable$ADDL > 0 |
                                                DoseTable$SS > 0) & DoseTable$II > 0)) {
                ADDLSS_IIMissing <- TRUE
              } else {
                ADDLSS_IIMissing <- FALSE
              }
              
              if (ADDLSS_IIMissing) {
                showModal(modalDialog(
                  paste(
                    "Interdose interval should be given (II > 0) for the rows where",
                    "ADDL/SS is specified.",
                    "The Graph won't be updated."
                  )
                ))
                
                req(!ADDLSS_IIMissing)
              }
              
            }
            
            ODE <- input$ODE
            
            # initialize to FALSE, so if not changed, req will interrupt it
            out <- FALSE

            tryCatch({
              out <- get_NLMEIEstimatesData(
                modelDir = modelDir,
                ColumnMapping1 = ColumnMapping1,
                thetas = thetas,
                plotVariables = plotVariables,
                sweepStart = sweepStart,
                sweepLength = sweepLength,
                numSweepSteps = numSweepSteps,
                DoseTable = DoseTable,
                useDoseTable = useDoseTable,
                ODE = ODE,
                fsep = fsep
              )
            },
            warning = function(cond) {
              warning(conditionMessage(cond))
              showModal(modalDialog(conditionMessage(cond)))
            },
            error = function(cond) {
              warning(conditionMessage(cond))
              showModal(modalDialog(conditionMessage(cond)))
            })
            
            shinyjs::delay(500,
                           shinyjs::enable(selector = ".allowed-input"))
            
            req(out)
            
            out
          })
        }
      )
    
    ### 3.6 Thetas checkboxes event ----
    lapply(
      X = seq_along(thetas),
      FUN = function(thetaIndex) {
        CurrentCheckboxId <- paste0("check_theta", thetaIndex)
        observeEvent(input[[CurrentCheckboxId]], {
          EnableCheckBox <-
            AllowPositiveLower[thetaIndex] &
            AllowPositiveUpper[thetaIndex] &
            !FrozenThetas[thetaIndex]
            
          if (EnableCheckBox) {
            .update_ThetaNumericValue(input = input,
                                      thetaIndex = thetaIndex,
                                      session = session,
                                      FixefValuesBounds = FixefValuesBounds)
            
            if (is.null(input[[paste0("num_theta", thetaIndex)]])) {
              reactiveThetasNum$values$thetas[[thetaIndex]] <- NA
            } else { 
              input[[paste0("num_theta", thetaIndex)]]
            }
          } else {
            shinyjs::disable(id = CurrentCheckboxId)
          }
        })
      }
    )
    
    ### 3.7 Thetas event ----
    lapply(
      X = seq_along(thetas),
      FUN = function(thetaIndex) {
        CurrentThetaId <- paste0("num_theta", thetaIndex)
        ThetaDebounce <-
          reactive({
            input[[CurrentThetaId]]
          }) %>% debounce(1000)
        observeEvent(ThetaDebounce(), {
          if (!FrozenThetas[thetaIndex]) {
            shinyjs::disable(selector = ".allowed-input")

            .update_ThetaNumericValue(
              input = input,
              thetaIndex = thetaIndex,
              session = session,
              FixefValuesBounds
            )

            InputThetaNotGood <- 
              is.null(input[[CurrentThetaId]]) ||
              is.na(input[[CurrentThetaId]])
            
            ReactiveThetaNotChanged <- 
              !is.null(reactiveThetasNum$values$thetas[[thetaIndex]]) &&
              !is.na(reactiveThetasNum$values$thetas[[thetaIndex]]) &&
              reactiveThetasNum$values$thetas[[thetaIndex]] ==
              input[[CurrentThetaId]]
            
            if (InputThetaNotGood ||
                ReactiveThetaNotChanged) {
              # no need to disable since simulation won't run
              shinyjs::enable(selector = ".allowed-input")
              if (InputThetaNotGood) {
                reactiveThetasNum$values$thetas[[thetaIndex]] <- NA
              } 
            } else {
              for (ThetaId in seq_along(thetas)) {
                reactiveThetasNum$values$thetas[[ThetaId]] <-
                  input[[paste0("num_theta", ThetaId)]]
              } 
            }
            
          } else {
            shinyjs::disable(id = CurrentThetaId)
          }
        })
      }
    )
    
    ## 4.0 Save&Exit ----
    observeEvent(input$exitShiny, {
      showModal(
        modalDialog(
          size = "m",
          title = "Exit Initial Estimates",
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

    observeEvent(input$exitConfirm,
                 {
                   for (thetaIndex in seq_along(thetas)) {
                     thetas[[thetaIndex]] <- input[[paste0("num_theta", thetaIndex)]]
                   }
                   
                   Certara.RsNLME::initFixedEffects(model) <- thetas
                   message("Shiny session has ended, fixed effects have been updated")
                   js$closewindow()
                   stopApp(model)
                 })
    
    observeEvent(input$exitCancel,
                 {
                   message("Shiny session has ended, fixed effects have not been updated")
                   js$closewindow()
                   stopApp(model)
                 })

    session$onSessionEnded(function()
    {
      stopApp(model)
    })

  }

  runApp(shinyApp(ui = ui, server = server),
         launch.browser = TRUE)
}

