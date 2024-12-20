
`%notin%` <- Negate(`%in%`)


cov_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cov"))
  )
}

cov_panel <- function(id, effectsAvail, covSelect = NULL, covType = NULL, covCenter = NULL, covCenterVal = NULL, covIsPositive = NULL, covLevels = NULL, covHasLabels = NULL, covLabels = NULL, covDir = NULL, covEffects = NULL, occRanEffDiag = NULL, valsOcc = NULL, isPopulation = TRUE, session) {
  moduleServer(id,
               function(input, output, session) {
                 # generate the UI on the server side
                 ns <- session$ns
                 panel_number <- regmatches(id,
                                            regexpr("[0-9]+", id))

                 output$cov <- renderUI({
                   tags$div(id = id,
                            bslib::card(
                              bslib::card_title("Covariate"),
                              fluidRow(
                                column(width = 3,
                                       textInput(inputId = ns("covSelect"), label = "Covariate", value = covSelect)
                                ),
                                column(width = 3,
                                       selectInput(inputId = ns("covType"), label = "Type", choices = if (isPopulation) c("Continuous", "Categorical", "Occasion") else c("Continuous", "Categorical"), selected = covType),
                                       conditionalPanel(condition = "input.covType == 'Continuous'", ns = ns,
                                                        column(width = 12, style = "padding: inherit;",
                                                               selectInput(inputId = ns("covCenter"), label = "Center", choices = c("Mean", "Median", "Value", "None"), selected = covCenter),
                                                               conditionalPanel(condition = "input.covCenter == 'Value'", ns = ns,
                                                                                numericInput(inputId = ns("covCenterVal"), label = "Value", min = 1, max = 1000, value = ifelse(is.null(covCenterVal) || is.na(covCenterVal), 1, covCenterVal), step = 1 )
                                                               )
                                                        )
                                       ),
                                       conditionalPanel(condition = "input.covType == 'Categorical' || input.covType == 'Occasion'", ns = ns,
                                                        column(width = 12, style = "padding: inherit",
                                                               textInput(inputId = ns("covLevels"), label = "Levels", value = covLevels, placeholder = "0,1"),
                                                               uiOutput(ns("covLevelsValidation")),
                                                        )
                                       )
                                ),
                                column(width = 3,
                                       selectInput(inputId = ns("covDir"), label = "Direction", choices = c("Forward", "Backward", "Interpolate"), selected = covDir),
                                       conditionalPanel(condition =  "input.covType == 'Categorical' || input.covType == 'Occasion'", ns = ns,
                                                        textInput(inputId = ns("covLabels"), label = "Labels", value = covLabels, placeholder = "Label1, Label2"),
                                                        uiOutput(ns("covLabelsValidation")),
                                       )
                                ),
                                column(width = 3,
                                       selectInput(inputId = ns("covEffects"), label = "Effects", choices = effectsAvail, multiple = TRUE, selected = covEffects)
                                )
                              ),
                              conditionalPanel(condition = "input.covType == 'Occasion'", ns = ns,
                                               bslib::card(
                                                 bslib::card_body(
                                                   class = "cov-eff-matrix-card-body",
                                                   bslib::card_title("Occasion Random Effects"),
                                                   fluidRow(
                                                     style = "padding-top: 1rem; margin-bottom: -1.0rem;",
                                                     checkboxInput(inputId = ns("occRanEffDiag"), label = "Diagonal", value = ifelse(is.null(occRanEffDiag), TRUE, occRanEffDiag))
                                                   ),
                                                   fluidRow(
                                                     column(width = 12,
                                                            div(style = "padding:10px;"),
                                                            uiOutput(ns("occRanEff"))
                                                     )
                                                   )
                                                 )
                                               )
                              ),
                              fluidRow(
                                column(width =1, offset = 11, 
                                       shinyWidgets::actionBttn(ns("removeCovPkBtn"), label = NULL, icon = shiny::icon("minus"), style = "material-circle" )
                                )
                              )
                            ),
                            div(style = "padding:10px;")
                     )
                 })
                 
                 cov_levels_validation <- reactive({
                   validate(
                     need(length(unlist(strsplit(input$covLevels, split = ","))) > 1, "Please specify 2 or more levels given unique values in data.")
                   )
                 })
                 output$covLevelsValidation <- renderPrint({
                   cov_levels_validation()
                 })
                 
                 cov_labels_validation <- reactive({
                   lbl <- unlist(strsplit(input$covLabels, split = ","))
                   lvl <- unlist(strsplit(input$covLevels, split = ","))
                   validate(
                     need(length(lbl) == length(lvl), "Only applicable if covariate in data is of class character. Number of labels and levels must be equal.")
                   )
                 })
                 output$covLabelsValidation <- renderPrint({
                   cov_labels_validation()
                 })
                 
                 output$occRanEff <- renderUI({
                   
                   if(length(input$covEffects) == 1){
                     # CovEff = 1 ----
                     uiOccRanEff <- tagList(
                       fluidRow(
                         numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]]))
                       )
                     )
                   } else if(length(input$covEffects) == 2){
                     # CovEff = 2 ----
                     if(input$occRanEffDiag == FALSE){
                       uiOccRanEff <- tagList(
                         fluidRow(class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/2,
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]])),
                           )
                         ),
                         fluidRow(class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/2,
                             numericInput(inputId = ns("occRanEff11"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]]))
                           )
                         )
                       )
                     } else {
                       uiOccRanEff <- tagList(
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/2,
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]])),
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/2,
                             !!!space_cols(1),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]]))
                           )
                         )
                       )
                     }
                   } else if(length(input$covEffects) == 3){
                     # CovEff = 3 ----
                     if (input$occRanEffDiag == FALSE){
                       uiOccRanEff <- tagList(
                         fluidRow(class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/3, 
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]])),
                           )
                         ),
                         fluidRow(class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/3, 
                             numericInput(inputId = ns("occRanEff11"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]]))
                           )
                         ),
                         fluidRow(class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/3, 
                             numericInput(inputId = ns("occRanEff111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff22"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]])),
                           )
                         )
                       )
                     } else {
                       uiOccRanEff <- tagList(
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/3, 
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]])),
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/3, 
                             !!!space_cols(1),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/3, 
                             !!!space_cols(2),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]])),
                           )
                         )
                       )
                     }
                   } else if(length(input$covEffects) == 4){
                     # CovEff = 4 ----
                     if (input$occRanEffDiag == FALSE){
                       uiOccRanEff <- tagList(
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/4, 
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/4, 
                             numericInput(inputId = ns("occRanEff11"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]])),
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/4, 
                             numericInput(inputId = ns("occRanEff111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff22"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]])),
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/4, 
                             numericInput(inputId = ns("occRanEff1111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff33"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff4"), label = input$covEffects[[4]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 1, valsOcc[[4]]))
                           )
                         )
                       )
                     } else {
                       uiOccRanEff <- tagList(
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/4, 
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]]))
                           ),
                           bslib::layout_column_wrap(
                             width = 1/4, 
                             !!!space_cols(1),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]])),
                           ),
                           bslib::layout_column_wrap(
                             width = 1/4, 
                             !!!space_cols(2),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]])),
                           ),
                           bslib::layout_column_wrap(
                             width = 1/4, 
                             !!!space_cols(3),
                             numericInput(inputId = ns("occRanEff4"), label = input$covEffects[[4]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 1, valsOcc[[4]]))
                           )
                         )
                       )
                     }
                   } else if(length(input$covEffects) == 5){
                     # CovEff = 5 ----
                     if (input$occRanEffDiag == FALSE){
                       uiOccRanEff <- tagList(
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/5,
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]]))
                           ),
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/5, 
                             numericInput(inputId = ns("occRanEff11"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]]))
                           ),
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/5,
                             numericInput(inputId = ns("occRanEff111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff22"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]]))
                           ),
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/5, 
                             numericInput(inputId = ns("occRanEff1111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff33"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff4"), label = input$covEffects[[4]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 1, valsOcc[[4]])),
                           ),
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/5,
                             numericInput(inputId = ns("occRanEff11111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff44"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff5"), label = input$covEffects[[5]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 1, valsOcc[[5]]))
                           )
                         )
                       )
                     } else {
                       uiOccRanEff <- tagList(
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/5,
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/5, 
                             !!!space_cols(1),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/5,
                             !!!space_cols(2),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/5, 
                             !!!space_cols(3),
                             numericInput(inputId = ns("occRanEff4"), label = input$covEffects[[4]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 1, valsOcc[[4]])),
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/5,
                             !!!space_cols(4),
                             numericInput(inputId = ns("occRanEff5"), label = input$covEffects[[5]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 1, valsOcc[[5]]))
                           )
                         )
                       )
                     }
                   } else if(length(input$covEffects) == 6){
                     # CovEff = 6 ----
                     if (input$occRanEffDiag == FALSE){
                       uiOccRanEff <- tagList(
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/6, 
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/6, 
                             numericInput(inputId = ns("occRanEff11"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]])),
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/6, 
                             numericInput(inputId = ns("occRanEff111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff22"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]]))
                           )   
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/6, 
                             numericInput(inputId = ns("occRanEff1111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff33"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff4"), label = input$covEffects[[4]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 1, valsOcc[[4]]))
                           ) 
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/6, 
                             numericInput(inputId = ns("occRanEff11111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff44"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff5"), label = input$covEffects[[5]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 1, valsOcc[[5]])),
                           ) 
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/6,
                             numericInput(inputId = ns("occRanEff111111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff22222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff3333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff444"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff55"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 0, valsOcc[[5]])),
                             numericInput(inputId = ns("occRanEff6"), label = input$covEffects[[6]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 1, valsOcc[[6]]))
                           ) 
                         )
                       )
                     } else {
                       uiOccRanEff <- tagList(
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/6, 
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/6, 
                             !!!space_cols(1),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]])),
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/6, 
                             !!!space_cols(2),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]]))
                           )   
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/6, 
                             !!!space_cols(3),
                             numericInput(inputId = ns("occRanEff4"), label = input$covEffects[[4]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 1, valsOcc[[4]]))
                           ) 
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/6, 
                             !!!space_cols(4),
                             numericInput(inputId = ns("occRanEff5"), label = input$covEffects[[5]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 1, valsOcc[[5]])),
                           ) 
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/6,
                             !!!space_cols(5),
                             numericInput(inputId = ns("occRanEff6"), label = input$covEffects[[6]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 1, valsOcc[[6]]))
                           ) 
                         )
                       )
                     }
                   } else if(length(input$covEffects) == 7){
                     # CovEff = 7 ----
                     if (input$occRanEffDiag == FALSE){
                       uiOccRanEff <- tagList(
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/7, 
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]])),
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/7, 
                             numericInput(inputId = ns("occRanEff11"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/7, 
                             numericInput(inputId = ns("occRanEff111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff22"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/7, 
                             numericInput(inputId = ns("occRanEff1111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff33"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff4"), label = input$covEffects[[4]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 1, valsOcc[[4]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/7, 
                             numericInput(inputId = ns("occRanEff11111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff44"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff5"), label = input$covEffects[[5]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 1, valsOcc[[5]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/7,
                             numericInput(inputId = ns("occRanEff111111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff22222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff3333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff444"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff55"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 0, valsOcc[[5]])),
                             numericInput(inputId = ns("occRanEff6"), label = input$covEffects[[6]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 1, valsOcc[[6]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/7, 
                             numericInput(inputId = ns("occRanEff1111111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff222222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff33333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff4444"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff555"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 0, valsOcc[[5]])),
                             numericInput(inputId = ns("occRanEff66"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 0, valsOcc[[6]])),
                             numericInput(inputId = ns("occRanEff7"), label = input$covEffects[[7]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[7]]), 1, valsOcc[[7]]))
                           )
                         )
                       )
                     } else {
                       uiOccRanEff <- tagList(
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/7, 
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]])),
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/7, 
                             !!!space_cols(1),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/7, 
                             !!!space_cols(2),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/7, 
                             !!!space_cols(3),
                             numericInput(inputId = ns("occRanEff4"), label = input$covEffects[[4]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 1, valsOcc[[4]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/7, 
                             !!!space_cols(4),
                             numericInput(inputId = ns("occRanEff5"), label = input$covEffects[[5]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 1, valsOcc[[5]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/7,
                             !!!space_cols(5),
                             numericInput(inputId = ns("occRanEff6"), label = input$covEffects[[6]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 1, valsOcc[[6]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/7, 
                             !!!space_cols(6),
                             numericInput(inputId = ns("occRanEff7"), label = input$covEffects[[7]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[7]]), 1, valsOcc[[7]]))
                           )
                         )
                       )
                     }
                   } else if(length(input$covEffects) == 8){
                     # CovEff = 8 ----
                     if (input$occRanEffDiag == FALSE){
                       uiOccRanEff <- tagList(
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/8, 
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]])),
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/8,
                             numericInput(inputId = ns("occRanEff11"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]])),
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/8,
                             numericInput(inputId = ns("occRanEff111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff22"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]])),
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/8, 
                             numericInput(inputId = ns("occRanEff1111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff33"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff4"), label = input$covEffects[[4]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 1, valsOcc[[4]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/8,
                             numericInput(inputId = ns("occRanEff11111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff44"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff5"), label = input$covEffects[[5]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 1, valsOcc[[5]])),
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/8,
                             numericInput(inputId = ns("occRanEff111111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff22222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff3333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff444"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff55"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 0, valsOcc[[5]])),
                             numericInput(inputId = ns("occRanEff6"), label = input$covEffects[[6]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 1, valsOcc[[6]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/8,
                             numericInput(inputId = ns("occRanEff1111111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff222222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff33333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff4444"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff555"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 0, valsOcc[[5]])),
                             numericInput(inputId = ns("occRanEff66"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 0, valsOcc[[6]])),
                             numericInput(inputId = ns("occRanEff7"), label = input$covEffects[[7]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[7]]), 1, valsOcc[[7]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/8,
                             numericInput(inputId = ns("occRanEff11111111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2222222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff333333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff44444"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff5555"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 0, valsOcc[[5]])),
                             numericInput(inputId = ns("occRanEff666"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 0, valsOcc[[6]])),
                             numericInput(inputId = ns("occRanEff77"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[7]]), 0, valsOcc[[7]])),
                             numericInput(inputId = ns("occRanEff8"), label = input$covEffects[[8]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[8]]), 1, valsOcc[[8]]))
                           )
                         )
                       )
                     } else {
                       uiOccRanEff <- tagList(
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/8, 
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]])),
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/8,
                             !!!space_cols(1),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]])),
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/8,
                             !!!space_cols(2),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]])),
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/8, 
                             !!!space_cols(3),
                             numericInput(inputId = ns("occRanEff4"), label = input$covEffects[[4]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 1, valsOcc[[4]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/8,
                             !!!space_cols(4),
                             numericInput(inputId = ns("occRanEff5"), label = input$covEffects[[5]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 1, valsOcc[[5]])),
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/8,
                             !!!space_cols(5),
                             numericInput(inputId = ns("occRanEff6"), label = input$covEffects[[6]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 1, valsOcc[[6]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/8,
                             !!!space_cols(6),
                             numericInput(inputId = ns("occRanEff7"), label = input$covEffects[[7]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[7]]), 1, valsOcc[[7]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/8,
                             !!!space_cols(7),
                             numericInput(inputId = ns("occRanEff8"), label = input$covEffects[[8]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[8]]), 1, valsOcc[[8]]))
                           )
                         )
                       )
                     }
                   } else if(length(input$covEffects) == 9){
                     # CovEff = 9 ----
                     if (input$occRanEffDiag == FALSE){
                       uiOccRanEff <- tagList(
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/9,
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/9,
                             numericInput(inputId = ns("occRanEff11"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/9,
                             numericInput(inputId = ns("occRanEff111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff22"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/9,
                             numericInput(inputId = ns("occRanEff1111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff33"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff4"), label = input$covEffects[[4]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 1, valsOcc[[4]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/9,
                             numericInput(inputId = ns("occRanEff11111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff44"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff5"), label = input$covEffects[[5]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 1, valsOcc[[5]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/9,
                             numericInput(inputId = ns("occRanEff111111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff22222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff3333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff444"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff55"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 0, valsOcc[[5]])),
                             numericInput(inputId = ns("occRanEff6"), label = input$covEffects[[6]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 1, valsOcc[[6]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/9,
                             numericInput(inputId = ns("occRanEff1111111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff222222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff33333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff4444"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff555"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 0, valsOcc[[5]])),
                             numericInput(inputId = ns("occRanEff66"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 0, valsOcc[[6]])),
                             numericInput(inputId = ns("occRanEff7"), label = input$covEffects[[7]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[7]]), 1, valsOcc[[7]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/9,
                             numericInput(inputId = ns("occRanEff11111111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff2222222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff333333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff44444"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff5555"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 0, valsOcc[[5]])),
                             numericInput(inputId = ns("occRanEff666"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 0, valsOcc[[6]])),
                             numericInput(inputId = ns("occRanEff77"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[7]]), 0, valsOcc[[7]])),
                             numericInput(inputId = ns("occRanEff8"), label = input$covEffects[[8]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[8]]), 1, valsOcc[[8]]))
                           )
                         ),
                         fluidRow(
                           class = "matrix-row",
                           bslib::layout_column_wrap(
                             width = 1/9, 
                             numericInput(inputId = ns("occRanEff111111111"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 0, valsOcc[[1]])),
                             numericInput(inputId = ns("occRanEff22222222"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 0, valsOcc[[2]])),
                             numericInput(inputId = ns("occRanEff3333333"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 0, valsOcc[[3]])),
                             numericInput(inputId = ns("occRanEff444444"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 0, valsOcc[[4]])),
                             numericInput(inputId = ns("occRanEff55555"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 0, valsOcc[[5]])),
                             numericInput(inputId = ns("occRanEff6666"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 0, valsOcc[[6]])),
                             numericInput(inputId = ns("occRanEff777"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[7]]), 0, valsOcc[[7]])),
                             numericInput(inputId = ns("occRanEff88"), label = NULL, min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[8]]), 0, valsOcc[[8]])),
                             numericInput(inputId = ns("occRanEff9"), label = input$covEffects[[9]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[9]]), 1, valsOcc[[9]]))
                           )
                         )
                       )
                     } else {
                       uiOccRanEff <- tagList(
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/9,
                             numericInput(inputId = ns("occRanEff1"), label = input$covEffects[[1]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[1]]), 1, valsOcc[[1]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/9,
                             !!!space_cols(1),
                             numericInput(inputId = ns("occRanEff2"), label = input$covEffects[[2]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[2]]), 1, valsOcc[[2]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/9,
                             !!!space_cols(2),
                             numericInput(inputId = ns("occRanEff3"), label = input$covEffects[[3]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[3]]), 1, valsOcc[[3]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/9,
                             !!!space_cols(3),
                             numericInput(inputId = ns("occRanEff4"), label = input$covEffects[[4]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[4]]), 1, valsOcc[[4]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/9,
                             !!!space_cols(4),
                             numericInput(inputId = ns("occRanEff5"), label = input$covEffects[[5]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[5]]), 1, valsOcc[[5]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/9,
                             !!!space_cols(5),
                             numericInput(inputId = ns("occRanEff6"), label = input$covEffects[[6]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[6]]), 1, valsOcc[[6]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/9,
                             !!!space_cols(6),
                             numericInput(inputId = ns("occRanEff7"), label = input$covEffects[[7]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[7]]), 1, valsOcc[[7]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/9,
                             !!!space_cols(7),
                             numericInput(inputId = ns("occRanEff8"), label = input$covEffects[[8]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[8]]), 1, valsOcc[[8]]))
                           )
                         ),
                         fluidRow(
                           bslib::layout_column_wrap(
                             width = 1/9, 
                             !!!space_cols(8),
                             numericInput(inputId = ns("occRanEff9"), label = input$covEffects[[9]], min = 0, max = 100, step = 1, value = ifelse(is.null(valsOcc[[9]]), 1, valsOcc[[9]]))
                           )
                         )
                       )
                     }
                   } else {
                     return()
                   }
                 })
                 
                 outputOptions(output, "cov", suspendWhenHidden = FALSE)  
                 
                 # remove the element
                 observeEvent(input$removeCovPkBtn, {
                   removeUI(selector = paste0("div#", id))
                   runjs(paste0("Shiny.onInputChange('", id, "-covSelect', null)"))
                   runjs(paste0("Shiny.onInputChange('", id, "-covType', null)"))
                 })
               })
  
  return(list(
    dropdown = reactive(input$covSelect)
  ))
}

renderMetaCov <- function(modelUser, input){
  posCovSelect <- grep(pattern = "covSelect", names(input))
  
  for(i in posCovSelect){
    n <- names(input)[[i]]
    if(!is.null(input[[n]])){
      cov <- input[[n]]
      panel <- sub("-.*", "", n)
      if(input[[paste0(panel, "-covType")]] == "Continuous"){
        if(input[[paste0(panel, "-covCenter")]] == "Value"){
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addCovariate(covariate = ..(cov), effect = ..(input[[paste0(panel, "-covEffects")]]), type = "Continuous", direction = ..(input[[paste0(panel, "-covDir")]]), center = "Value", centerValue = ..(input[[paste0(panel, "-covCenterVal")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
        } else {
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addCovariate(covariate = ..(cov), effect = ..(input[[paste0(panel, "-covEffects")]]), type = "Continuous", direction = ..(input[[paste0(panel, "-covDir")]]), center = ..(input[[paste0(panel, "-covCenter")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
          
        }
      } else if(input[[(paste0(panel, "-covType"))]] == "Categorical") {
        if(!is.null(input[[paste0(panel, "-covLabels")]]) && input[[paste0(panel, "-covLabels")]] != ""){
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addCovariate(covariate = ..(cov), effect = ..(input[[paste0(panel, "-covEffects")]]), type = "Categorical", direction = ..(input[[paste0(panel, "-covDir")]]), levels = ..(as.numeric(unlist(strsplit(input[[paste0(panel, "-covLevels")]], split = ",")))),  labels = ..(trimws(unlist(strsplit(input[[paste0(panel, "-covLabels")]], split = ",")))))
            })
          },
          error = function(e) {
            return(modelUser)
          })
        } else {
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addCovariate(covariate = ..(cov), effect = ..(input[[paste0(panel, "-covEffects")]]), type = "Categorical", direction = ..(input[[paste0(panel, "-covDir")]]), levels = ..(as.numeric(unlist(strsplit(input[[paste0(panel, "-covLevels")]], split = ",")))))
            })
          },
          error = function(e) {
            return(modelUser)
          })
          
        }
      } else {
        #valsOcc <- c(input[[paste0(panel, "-occRanEff1")]], input[[paste0(panel, "-occRanEff2")]])
        if(input[[paste0(panel, "-occRanEffDiag")]] == TRUE){
          valsOcc <- c(input[[paste0(panel, "-occRanEff1")]], input[[paste0(panel, "-occRanEff2")]], input[[paste0(panel, "-occRanEff3")]], input[[paste0(panel, "-occRanEff4")]], input[[paste0(panel, "-occRanEff5")]], input[[paste0(panel, "-occRanEff6")]], input[[paste0(panel, "-occRanEff7")]], input[[paste0(panel, "-occRanEff8")]], input[[paste0(panel, "-occRanEff9")]], input[[paste0(panel, "-occRanEff10")]] )
        } else {
          valsOcc <- c(input[[paste0(panel, "-occRanEff1")]],
                       input[[paste0(panel, "-occRanEff11")]], input[[paste0(panel, "-occRanEff2")]],
                       input[[paste0(panel, "-occRanEff111")]], input[[paste0(panel, "-occRanEff22")]], input[[paste0(panel, "-occRanEff3")]],
                       input[[paste0(panel, "-occRanEff1111")]], input[[paste0(panel, "-occRanEff222")]], input[[paste0(panel, "-occRanEff33")]], input[[paste0(panel, "-occRanEff4")]],
                       input[[paste0(panel, "-occRanEff11111")]], input[[paste0(panel, "-occRanEff2222")]], input[[paste0(panel, "-occRanEff333")]], input[[paste0(panel, "-occRanEff44")]], input[[paste0(panel, "-occRanEff5")]],
                       input[[paste0(panel, "-occRanEff111111")]], input[[paste0(panel, "-occRanEff22222")]], input[[paste0(panel, "-occRanEff3333")]], input[[paste0(panel, "-occRanEff444")]], input[[paste0(panel, "-occRanEff55")]], input[[paste0(panel, "-occRanEff6")]],
                       input[[paste0(panel, "-occRanEff1111111")]], input[[paste0(panel, "-occRanEff222222")]], input[[paste0(panel, "-occRanEff33333")]], input[[paste0(panel, "-occRanEff4444")]], input[[paste0(panel, "-occRanEff555")]], input[[paste0(panel, "-occRanEff66")]], input[[paste0(panel, "-occRanEff7")]],
                       input[[paste0(panel, "-occRanEff11111111")]], input[[paste0(panel, "-occRanEff2222222")]], input[[paste0(panel, "-occRanEff333333")]], input[[paste0(panel, "-occRanEff44444")]], input[[paste0(panel, "-occRanEff5555")]], input[[paste0(panel, "-occRanEff666")]], input[[paste0(panel, "-occRanEff77")]], input[[paste0(panel, "-occRanEff8")]],
                       input[[paste0(panel, "-occRanEff111111111")]], input[[paste0(panel, "-occRanEff22222222")]], input[[paste0(panel, "-occRanEff3333333")]], input[[paste0(panel, "-occRanEff444444")]], input[[paste0(panel, "-occRanEff55555")]], input[[paste0(panel, "-occRanEff6666")]], input[[paste0(panel, "-occRanEff777")]], input[[paste0(panel, "-occRanEff88")]], input[[paste0(panel, "-occRanEff9")]],
                       input[[paste0(panel, "-occRanEff1111111111")]], input[[paste0(panel, "-occRanEff222222222")]], input[[paste0(panel, "-occRanEff33333333")]], input[[paste0(panel, "-occRanEff4444444")]], input[[paste0(panel, "-occRanEff555555")]], input[[paste0(panel, "-occRanEff66666")]], input[[paste0(panel, "-occRanEff7777")]], input[[paste0(panel, "-occRanEff888")]], input[[paste0(panel, "-occRanEff99")]], input[[paste0(panel, "-occRanEff10")]]
          )
        }

        if(!is.null(input[[paste0(panel, "-covLabels")]]) && input[[paste0(panel, "-covLabels")]] != ""){
          if(input[[paste0(panel, "-occRanEffDiag")]] == TRUE){
            if(length(input[[paste0(panel, "-covEffects")]]) != length(valsOcc)){
              valsOcc <- valsOcc[1:length(input[[paste0(panel, "-covEffects")]])]
            }
            tryCatch({
              modelUser <- metaExpr({
                ..(modelUser) %>%
                  addCovariate(covariate = ..(cov), effect = ..(input[[paste0(panel, "-covEffects")]]), type = "Occasion", direction = ..(input[[paste0(panel, "-covDir")]]), levels = ..(as.numeric(unlist(strsplit(input[[paste0(panel, "-covLevels")]], split = ",")))), labels = ..(trimws(unlist(strsplit(input[[paste0(panel, "-covLabels")]], split = ",")))), isDiagonal = TRUE, values = ..(as.numeric(valsOcc)))
              })
            }, error = function(e){
              return(modelUser)
            })
          } else {
            if(reqBlockNum(length(input[[paste0(panel, "-covEffects")]])) != length(valsOcc)){
              valsOcc <- valsOcc[1:reqBlockNum(length(input[[paste0(panel, "-covEffects")]]))]
            }
            tryCatch({
              modelUser <- metaExpr({
                ..(modelUser) %>%
                  addCovariate(covariate = ..(cov), effect = ..(input[[paste0(panel, "-covEffects")]]), type = "Occasion", direction = ..(input[[paste0(panel, "-covDir")]]), levels = ..(as.numeric(unlist(strsplit(input[[paste0(panel, "-covLevels")]], split = ",")))), labels = ..(trimws(unlist(strsplit(input[[paste0(panel, "-covLabels")]], split = ",")))), isDiagonal = FALSE, values = ..(as.numeric(valsOcc)))
              })
            }, error = function(e){
              checkPositiveDefinite(input[[paste0(panel, "-covEffects")]], as.numeric(valsOcc))
              return(modelUser)
            })
          }
        } else {
          if(input[[paste0(panel, "-occRanEffDiag")]] == TRUE){
            if(length(input[[paste0(panel, "-covEffects")]]) != length(valsOcc)){
              valsOcc <- valsOcc[1:length(input[[paste0(panel, "-covEffects")]])]
            }
            tryCatch({
              modelUser <- metaExpr({
                ..(modelUser) %>%
                  addCovariate(covariate = ..(cov), effect = ..(input[[paste0(panel, "-covEffects")]]), type = "Occasion", direction = ..(input[[paste0(panel, "-covDir")]]), levels = ..(as.numeric(unlist(strsplit(input[[paste0(panel, "-covLevels")]], split = ",")))), isDiagonal = TRUE, values = ..(as.numeric(valsOcc)))
              })
            }, error = function(e){
              return(modelUser)
            })
          } else {
            if(reqBlockNum(length(input[[paste0(panel, "-covEffects")]])) != length(valsOcc)){
              valsOcc <- valsOcc[1:reqBlockNum(length(input[[paste0(panel, "-covEffects")]]))]
            }
            tryCatch({
              modelUser <- metaExpr({
                ..(modelUser) %>%
                  addCovariate(covariate = ..(cov), effect = ..(input[[paste0(panel, "-covEffects")]]), type = "Occasion", direction = ..(input[[paste0(panel, "-covDir")]]), levels = ..(as.numeric(unlist(strsplit(input[[paste0(panel, "-covLevels")]], split = ",")))), isDiagonal = FALSE, values = ..(as.numeric(valsOcc)))
              })
            }, error = function(e){
              checkPositiveDefinite(input[[paste0(panel, "-covEffects")]], as.numeric(valsOcc))
              return(modelUser)
            })
          }
          
        }
      }
    }
  }
  return(modelUser)
}



# occ_panel_ui <- function(id, effects) {
#   ns <- NS(id)
#     tagList(
#       material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                       h6(effects[[1]]),
#                       material_number_box(ns("occRan1"), label = NULL, initial_value = 1, min_value = 0, max_value = 1000, step_size = 1)
#       )
#     )
# }
# 
# occ_panel_server <-  function(id) {
#     moduleServer(
#       id,
#       function(input, output, session) {
#         count <- reactiveVal(0)
#         observeEvent(input$button, {
#           count(count() + 1)
#         })
#         output$out <- renderText({
#           count()
#         })
#         count
#       }
#     )
# }
# 
# 
# renderOccasionRanEff <- function(id, effects, isDiagonal ){
# 
#   nEff <- length(effects)
# 
#   if(nEff == 0){
#     return()
#   }
# 
#   if(nEff == 1){
#     uiRanEff <-  tagList(
#       material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                       h6(effects[[1]]),
#                       material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 1000, step_size = 1)
#       )
#     )
#   } else if(nEff == 2) {
#     if(isDiagonal == FALSE){
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0(id, "-occRan1"), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0(id, "-occRan11"), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0(id, "-occRan2"), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
#     } else {
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0(id, "-occRan1"), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0(id, "-occRan2"), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
# 
#     }
#   } else if(nEff == 3) {
#     if(isDiagonal == FALSE){
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
#     } else {
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
#     }
# 
#   } else if(nEff == 4){
#     if(isDiagonal == FALSE){
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan1111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan33_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[4]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
#     } else {
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[4]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
# 
#     }
# 
#   } else if(nEff == 5){
#     if(isDiagonal == FALSE){
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan1111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan2222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan33_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[4]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan44_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:200px; padding-bottom:0px',
#                           h6(effects[[5]]),
#                           material_number_box(paste0("occRan5_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
#     } else {
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[4]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:200px; padding-bottom:0px',
#                           h6(effects[[5]]),
#                           material_number_box(paste0("occRan5_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
# 
#     }
# 
#   } else if(nEff == 6){
#     if(isDiagonal == FALSE){
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan1111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan2222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan33_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan3333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[4]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan44_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:200px; padding-bottom:0px',
#                           h6(effects[[5]]),
#                           material_number_box(paste0("occRan5_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan55_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:250px; padding-bottom:0px',
#                           h6(effects[[6]]),
#                           material_number_box(paste0("occRan6_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
#     } else {
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[4]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:200px; padding-bottom:0px',
#                           h6(effects[[5]]),
#                           material_number_box(paste0("occRan5_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:250px; padding-bottom:0px',
#                           h6(effects[[6]]),
#                           material_number_box(paste0("occRan6_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
# 
#     }
# 
#   } else if(nEff == 7){
#     if(isDiagonal == FALSE){
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan1111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan1111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan2222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan222222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan33_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan3333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan33333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[4]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan44_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan4444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:200px; padding-bottom:0px',
#                           h6(effects[[5]]),
#                           material_number_box(paste0("occRan5_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan55_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan555_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:250px; padding-bottom:0px',
#                           h6(effects[[6]]),
#                           material_number_box(paste0("occRan6_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan66_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:300px; padding-bottom:0px',
#                           h6(effects[[7]]),
#                           material_number_box(paste0("occRan7_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
#     } else {
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[4]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:200px; padding-bottom:0px',
#                           h6(effects[[5]]),
#                           material_number_box(paste0("occRan5_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:250px; padding-bottom:0px',
#                           h6(effects[[6]]),
#                           material_number_box(paste0("occRan6_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:300px; padding-bottom:0px',
#                           h6(effects[[7]]),
#                           material_number_box(paste0("occRan7_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
# 
#     }
# 
#   } else if(nEff == 8){
#     if(isDiagonal == FALSE){
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan1111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan1111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan2222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan222222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan2222222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan33_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan3333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan33333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan333333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[4]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan44_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan4444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan44444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:200px; padding-bottom:0px',
#                           h6(effects[[5]]),
#                           material_number_box(paste0("occRan5_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan55_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan555_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan5555_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:250px; padding-bottom:0px',
#                           h6(effects[[6]]),
#                           material_number_box(paste0("occRan6_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan66_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan666_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:300px; padding-bottom:0px',
#                           h6(effects[[7]]),
#                           material_number_box(paste0("occRan7_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan77_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:350px; padding-bottom:0px',
#                           h6(effects[[8]]),
#                           material_number_box(paste0("occRan8_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
#     } else {
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[4]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:200px; padding-bottom:0px',
#                           h6(effects[[5]]),
#                           material_number_box(paste0("occRan5_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:250px; padding-bottom:0px',
#                           h6(effects[[6]]),
#                           material_number_box(paste0("occRan6_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:300px; padding-bottom:0px',
#                           h6(effects[[7]]),
#                           material_number_box(paste0("occRan7_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:350px; padding-bottom:0px',
#                           h6(effects[[8]]),
#                           material_number_box(paste0("occRan8_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
# 
#     }
# 
#   } else if(nEff == 9){
#     if(isDiagonal == FALSE){
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan1111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan1111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan2222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan222222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan2222222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22222222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan33_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan3333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan33333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan333333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan3333333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[4]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan44_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan4444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan44444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan444444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:200px; padding-bottom:0px',
#                           h6(effects[[5]]),
#                           material_number_box(paste0("occRan5_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan55_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan555_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan5555_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan55555_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:250px; padding-bottom:0px',
#                           h6(effects[[6]]),
#                           material_number_box(paste0("occRan6_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan66_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan666_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan6666_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:300px; padding-bottom:0px',
#                           h6(effects[[7]]),
#                           material_number_box(paste0("occRan7_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan77_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan777_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:350px; padding-bottom:0px',
#                           h6(effects[[8]]),
#                           material_number_box(paste0("occRan8_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan88_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
# 
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:400px; padding-bottom:0px',
#                           h6(effects[[9]]),
#                           material_number_box(paste0("occRan9_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
#     } else {
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[4]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:200px; padding-bottom:0px',
#                           h6(effects[[5]]),
#                           material_number_box(paste0("occRan5_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:250px; padding-bottom:0px',
#                           h6(effects[[6]]),
#                           material_number_box(paste0("occRan6_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:300px; padding-bottom:0px',
#                           h6(effects[[7]]),
#                           material_number_box(paste0("occRan7_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:350px; padding-bottom:0px',
#                           h6(effects[[8]]),
#                           material_number_box(paste0("occRan8_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:400px; padding-bottom:0px',
#                           h6(effects[[9]]),
#                           material_number_box(paste0("occRan9_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
# 
#     }
# 
#   } else if(nEff == 10){
#     if(isDiagonal == FALSE){
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[1]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan1111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan1111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan11111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan111111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan1111111111_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[2]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan2222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan222222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan2222222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan22222222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan222222222_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[3]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan33_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan3333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan33333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan333333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan3333333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan33333333_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[4]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan44_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan4444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan44444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan444444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan4444444_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:200px; padding-bottom:0px',
#                           h6(effects[[5]]),
#                           material_number_box(paste0("occRan5_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan55_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan555_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan5555_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan55555_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan555555_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:250px; padding-bottom:0px',
#                           h6(effects[[6]]),
#                           material_number_box(paste0("occRan6_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan66_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan666_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan6666_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan66666_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:300px; padding-bottom:0px',
#                           h6(effects[[7]]),
#                           material_number_box(paste0("occRan7_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan77_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan777_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan7777_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:350px; padding-bottom:0px',
#                           h6(effects[[8]]),
#                           material_number_box(paste0("occRan8_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan88_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan888_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:400px; padding-bottom:0px',
#                           h6(effects[[9]]),
#                           material_number_box(paste0("occRan9_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1),
#                           material_number_box(paste0("occRan99_", id), label = NULL, initial_value = 0, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:450px; padding-bottom:0px',
#                           h6(effects[[10]]),
#                           material_number_box(paste0("occRan10_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
#     } else {
#       uiRanEff <- tagList(
#         material_row(
#           material_column(width = 1,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                           h6(effects[[10]]),
#                           material_number_box(paste0("occRan1_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:50px; padding-bottom:0px',
#                           h6(effects[[10]]),
#                           material_number_box(paste0("occRan2_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:100px; padding-bottom:0px',
#                           h6(effects[[10]]),
#                           material_number_box(paste0("occRan3_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:150px; padding-bottom:0px',
#                           h6(effects[[10]]),
#                           material_number_box(paste0("occRan4_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:200px; padding-bottom:0px',
#                           h6(effects[[10]]),
#                           material_number_box(paste0("occRan5_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:250px; padding-bottom:0px',
#                           h6(effects[[10]]),
#                           material_number_box(paste0("occRan6_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:300px; padding-bottom:0px',
#                           h6(effects[[10]]),
#                           material_number_box(paste0("occRan7_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:350px; padding-bottom:0px',
#                           h6(effects[[10]]),
#                           material_number_box(paste0("occRan8_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:400px; padding-bottom:0px',
#                           h6(effects[[10]]),
#                           material_number_box(paste0("occRan9_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           ),
#           material_column(width = 1, style='padding-left:5px; padding-right:0px; padding-top:450px; padding-bottom:0px',
#                           h6(effects[[10]]),
#                           material_number_box(paste0("occRan10_", id), label = NULL, initial_value = 1, min_value = 0, max_value = 100, step_size = 1)
#           )
#         )
#       )
# 
#     }
# 
#   } else {
#     return()
#   }
#   uiRanEff
# }

covpanelselect <- function(input){
  ret <- c(input[["panel_1-covSelect"]], input[["panel_2-covSelect"]], input[["panel_3-covSelect"]], input[["panel_4-covSelect"]], input[["panel_5-covSelect"]], input[["panel_6-covSelect"]],input[["panel_7-covSelect"]], input[["panel_8-covSelect"]], input[["panel_9-covSelect"]],input[["panel_10-covSelect"]],
           input[["panel_11-covSelect"]], input[["panel_12-covSelect"]], input[["panel_13-covSelect"]], input[["panel_14-covSelect"]], input[["panel_15-covSelect"]], input[["panel_16-covSelect"]],input[["panel_17-covSelect"]], input[["panel_18-covSelect"]], input[["panel_19-covSelect"]],input[["panel_10-covSelect"]],
           input[["panel_21-covSelect"]], input[["panel_22-covSelect"]], input[["panel_23-covSelect"]], input[["panel_24-covSelect"]], input[["panel_25-covSelect"]], input[["panel_26-covSelect"]],input[["panel_27-covSelect"]], input[["panel_28-covSelect"]], input[["panel_29-covSelect"]],input[["panel_30-covSelect"]],
           input[["panel_31-covSelect"]], input[["panel_32-covSelect"]], input[["panel_33-covSelect"]], input[["panel_34-covSelect"]], input[["panel_35-covSelect"]], input[["panel_36-covSelect"]],input[["panel_37-covSelect"]], input[["panel_38-covSelect"]], input[["panel_39-covSelect"]],input[["panel_40-covSelect"]],
           input[["panel_41-covSelect"]], input[["panel_42-covSelect"]], input[["panel_43-covSelect"]], input[["panel_44-covSelect"]], input[["panel_45-covSelect"]], input[["panel_46-covSelect"]],input[["panel_47-covSelect"]], input[["panel_48-covSelect"]], input[["panel_49-covSelect"]],input[["panel_50-covSelect"]],
           input[["panel_51-covSelect"]], input[["panel_52-covSelect"]], input[["panel_53-covSelect"]], input[["panel_54-covSelect"]], input[["panel_55-covSelect"]], input[["panel_56-covSelect"]],input[["panel_57-covSelect"]], input[["panel_58-covSelect"]], input[["panel_59-covSelect"]],input[["panel_60-covSelect"]])
  
  return(ret)
}



#model textual helper functions 
get_cat_cov <- function(covlist){
  l <- list()
  
  for(i in seq_along(covlist)){
    cov <- covlist[[i]]@name
    type <- covlist[[i]]@type
    if(type == 2){
      if(length(covlist[[i]]@covarItems) == 0){
        l[[cov]]$lvl[[1]] <- ""
        l[[cov]]$lbl[[1]] <- ""
      } else {
        for(j in seq_along(covlist[[i]]@covarItems)){
          lvl <-  covlist[[i]]@covarItems[[j]]@value
          lbl <-  covlist[[i]]@covarItems[[j]]@name
          l[[cov]]$lvl[[j]] <- lvl
          l[[cov]]$lbl[[j]] <- lbl
        }
      }
    }
  }
  
  return(l)
}

get_lvl_from_covlist <- function(covlist, cov){
  
  lvl <- c()
  covlist_lvl <- covlist[[cov]]$lvl
  for(i in seq_along(covlist_lvl)){
    lvl <- c(lvl, covlist_lvl[[i]])
  }
  
  lvl <- paste0(lvl, collapse = ", ")
  
  return(lvl)
}

get_lbl_from_covlist <- function(covlist, cov){
  
  lbl <- c()
  covlist_lbl <- covlist[[cov]]$lbl
  for(i in seq_along(covlist_lbl)){
    lbl <- c(lbl, covlist_lbl[[i]])
  }
  
  lbl <- paste0(lbl, collapse = ", ")
  
  return(lbl)
}

