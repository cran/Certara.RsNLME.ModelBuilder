
ss_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("ssModule"))
  )
}

ss_panel <- function(id, doseName, data) {
  moduleServer(id,
               function(input, output, session) {
                 # generate the UI on the server side
                 ns <- session$ns
                 panel_number <- regmatches(id,
                                            regexpr("[0-9]+", id))
                 output$ssModule <- renderUI({
                   tags$div(id = id,
                            bslib::card(class = "input-options",
                              bslib::card_title("Dose Cycle"),
                              fluidRow(
                                style = "margin-bottom: 1rem;",
                                column(width = 3,
                                       selectInput(inputId = ns("ssDoseOrder"), label = "Dosepoint", choices = c("dosepoint"))
                                ),
                                column(width = 3,
                                       selectInput(inputId = ns("ssDoseSelect"), label = "Dosing Compartment", choices = c(doseName()))
                                ),
                                column(width = 3,
                                       selectInput(inputId = ns("ssDoseAdministration"), label = "Administration", choices = c("Bolus", "Infusion"))
                                )
                              ),
                              fluidRow(
                                column(width = 3,
                                       selectInput(inputId = ns("ssAmountType"), label = "Amount", choices = c("Constant", "Column"))
                                ),
                                column(width = 3,
                                       conditionalPanel("input.ssAmountType == 'Constant'", ns = ns,
                                                        numericInput(inputId = ns("ssAmountConstant"), label = "Amount Value", min = 0, max = Inf, value = 1)
                                       ),
                                       conditionalPanel("input.ssAmountType == 'Column'", ns = ns,
                                                        selectInput(inputId = ns("ssAmountColumn"), label = "Amount Column",  choices = c("", names(data)))
                                       )
                                )
                              ),
                              conditionalPanel(
                                "input.ssDoseAdministration == 'Infusion'", ns = ns,
                                fluidRow(
                                  column(
                                    width = 4, style = "padding-top: 0.5rem;",
                                    checkboxInput(inputId = ns("ssisDuration"), label = "Duration", value = FALSE)
                                  )
                                ),
                                 conditionalPanel(
                                   "input.ssisDuration == true && input.ssDoseAdministration == 'Infusion'", ns = ns,
                                   fluidRow(
                                     column(width = 3,
                                            selectInput(inputId = ns("ssDurationType"), label = "Duration", choices = c("Constant", "Column"))
                                     ),
                                     column(width = 3,
                                            conditionalPanel("input.ssDoseAdministration == 'Infusion' && input.ssisDuration == true && input.ssDurationType == 'Constant'", ns = ns,
                                                             numericInput(inputId = ns("ssDurationConstant"), label = "Duration Value", min = 0, max = Inf, value = 1)
                                            ),
                                            conditionalPanel("input.ssDoseAdministration == 'Infusion' && input.ssisDuration == true && input.ssDurationType == 'Column'", ns = ns,
                                                             selectInput(inputId = ns("ssDurationColumn"), label = "Duration Column",  choices = c("", names(data)))
                                            )
                                     )
                                   )               
                                 ),
                                 conditionalPanel(
                                   "input.ssisDuration == false && input.ssDoseAdministration == 'Infusion'", ns = ns,
                                   fluidRow(
                                     column(width = 3,
                                            selectInput(inputId = ns("ssRateType"), label = "Rate", choices = c("Constant", "Column"))
                                     ),
                                     column(width = 3,
                                            conditionalPanel("input.ssDoseAdministration == 'Infusion' && input.ssisDuration == false && input.ssRateType == 'Constant'", ns = ns,
                                                             numericInput(inputId = ns("ssRateConstant"), label = "Rate Value", min = 0, max = Inf, value = 1)
                                            ),
                                            conditionalPanel("input.ssDoseAdministration == 'Infusion' && input.ssisDuration == false && input.ssRateType == 'Column'", ns = ns,
                                                             selectInput(inputId = ns("ssRateColumn"), label = "Rate Column",  choices = c("", names(data)))
                                            )
                                     )
                                   )
                                 )
                              ),
                              fluidRow(
                                column(width = 3,
                                       selectInput(inputId = ns("ssIIType"), label = "II", choices = c("Constant", "Column"))
                                ),
                                column(width = 3,
                                       conditionalPanel("input.ssIIType == 'Constant'", ns = ns,
                                                        numericInput(inputId = ns("ssIIConstant"), label = "II Value", min = 0, max = Inf, value = 24)
                                       ),
                                       conditionalPanel("input.ssIIType == 'Column'", ns = ns,
                                                        selectInput(inputId = ns("ssIIColumn"), label = "II Column",  choices = c("", names(data)))
                                       )
                                )
                              ),
                              fluidRow(
                                column(width = 1, offset = 11, 
                                       shinyWidgets::actionBttn(ns("removeSSBtn"), label = NULL, icon = shiny::icon("minus"), style = "material-circle")
                                )
                              )
                            ),
                            div(style = "padding:10px;")
                     )
                 })
                 
                 outputOptions(output, "ssModule", suspendWhenHidden = FALSE)  
                 
                 # remove the element
                 observeEvent(input$removeSSBtn, {
                   removeUI(selector = paste0("div#", id))
                   runjs(paste0("Shiny.onInputChange('", id, "-ssDoseAdministration', null)"))
                   runjs(paste0("Shiny.onInputChange('", id, "-ssIIConstant', null)"))
                   runjs(paste0("Shiny.onInputChange('", id, "-ssIIColumn', null)"))
                 })
            })
  
  return(list(
    dropdown = reactive(input$ssDoseAdministration)
  ))
}



renderMetaDoseSS <- function(modelUser, input){
  posSSSelect <- grep(pattern = "ssDoseAdministration", names(input))
  
  ss_panel_inputs <- c()
  for(i in posSSSelect){
    inputname <- names(input)[[i]]
    ss_panel_inputs <- c(ss_panel_inputs, inputname)
  }
  
  #panels are not automatically sorted when new ones created, so new dose cycle inputs appear out of order in 
  # possSSelect, must manually sort
  
  ss_panel_inputs <- sort(ss_panel_inputs)
  
  for(i in seq_along(ss_panel_inputs)){
    n <- ss_panel_inputs[[i]]
    if(!is.null(input[[n]])){
      ss_dosename <- input[[n]]
      panel <- sub("-.*", "", n)
      if(input[[paste0(panel, "-ssDoseAdministration")]] == "Bolus"){
        if(input[[paste0(panel, "-ssAmountType")]] == "Constant" && input[[paste0(panel, "-ssIIType")]] == "Constant"){
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Bolus", amount = ..(input[[paste0(panel, "-ssAmountConstant")]]), II = ..(input[[paste0(panel, "-ssIIConstant")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
        } else if(input[[paste0(panel, "-ssAmountType")]] == "Constant" && input[[paste0(panel, "-ssIIType")]] == "Column"){
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Bolus", amount = ..(input[[paste0(panel, "-ssAmountConstant")]]), II = ..(input[[paste0(panel, "-ssIIColumn")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
          
        } else if(input[[paste0(panel, "-ssAmountType")]] == "Column" && input[[paste0(panel, "-ssIIType")]] == "Constant"){
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Bolus", amount = ..(input[[paste0(panel, "-ssAmountColumn")]]), II = ..(input[[paste0(panel, "-ssIIConstant")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
          
        } else {
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Bolus", amount = ..(input[[paste0(panel, "-ssAmountColumn")]]), II = ..(input[[paste0(panel, "-ssIIColumn")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
        }
      } else { #Infusion Administration
       if(input[[paste0(panel, "-ssisDuration")]] == TRUE){
        if(input[[paste0(panel, "-ssAmountType")]] == "Constant" && input[[paste0(panel, "-ssIIType")]] == "Constant"){
          if(input[[paste0(panel, "-ssDurationType")]] == "Constant"){
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountConstant")]]), II = ..(input[[paste0(panel, "-ssIIConstant")]]), duration = ..(input[[paste0(panel, "-ssDurationConstant")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
          } else {
            modelUser <- tryCatch({
              metaExpr({
                ..(modelUser) %>%
                  addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountConstant")]]), II = ..(input[[paste0(panel, "-ssIIConstant")]]), duration = ..(input[[paste0(panel, "-ssDurationColumn")]]))
              })
            },
            error = function(e) {
              return(modelUser)
            })
          }
        } else if(input[[paste0(panel, "-ssAmountType")]] == "Constant" && input[[paste0(panel, "-ssIIType")]] == "Column"){
          if(input[[paste0(panel, "-ssDurationType")]] == "Constant"){
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountConstant")]]), II = ..(input[[paste0(panel, "-ssIIColumn")]]), duration = ..(input[[paste0(panel, "-ssDurationConstant")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
          } else {
            modelUser <- tryCatch({
              metaExpr({
                ..(modelUser) %>%
                  addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountConstant")]]), II = ..(input[[paste0(panel, "-ssIIColumn")]]), duration = ..(input[[paste0(panel, "-ssDurationColumn")]]))
              })
            },
            error = function(e) {
              return(modelUser)
            })
          }
        } else if(input[[paste0(panel, "-ssAmountType")]] == "Column" && input[[paste0(panel, "-ssIIType")]] == "Constant"){
          if(input[[paste0(panel, "-ssDurationType")]] == "Constant"){
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountColumn")]]), II = ..(input[[paste0(panel, "-ssIIConstant")]]), duration = ..(input[[paste0(panel, "-ssDurationConstant")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
          } else {
            modelUser <- tryCatch({
              metaExpr({
                ..(modelUser) %>%
                  addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountColumn")]]), II = ..(input[[paste0(panel, "-ssIIConstant")]]), duration = ..(input[[paste0(panel, "-ssDurationColumn")]]))
              })
            },
            error = function(e) {
              return(modelUser)
            })
          }
        } else {
          if(input[[paste0(panel, "-ssDurationType")]] == "Constant"){
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountColumn")]]), II = ..(input[[paste0(panel, "-ssIIColumn")]]), duration = ..(input[[paste0(panel, "-ssDurationConstant")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
          } else {
            modelUser <- tryCatch({
              metaExpr({
                ..(modelUser) %>%
                  addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountColumn")]]), II = ..(input[[paste0(panel, "-ssIIColumn")]]), duration = ..(input[[paste0(panel, "-ssDurationColumn")]]))
              })
            },
            error = function(e) {
              return(modelUser)
            })
          }
        }
       } else {
         if(input[[paste0(panel, "-ssAmountType")]] == "Constant" && input[[paste0(panel, "-ssIIType")]] == "Constant"){
           if(input[[paste0(panel, "-ssRateType")]] == "Constant"){
           modelUser <- tryCatch({
             metaExpr({
               ..(modelUser) %>%
                 addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountConstant")]]), II = ..(input[[paste0(panel, "-ssIIConstant")]]), rate = ..(input[[paste0(panel, "-ssRateConstant")]]))
             })
           },
           error = function(e) {
             return(modelUser)
           })
           } else {
             modelUser <- tryCatch({
               metaExpr({
                 ..(modelUser) %>%
                   addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountConstant")]]), II = ..(input[[paste0(panel, "-ssIIConstant")]]), rate = ..(input[[paste0(panel, "-ssRateColumn")]]))
               })
             },
             error = function(e) {
               return(modelUser)
             })
           }
         } else if(input[[paste0(panel, "-ssAmountType")]] == "Constant" && input[[paste0(panel, "-ssIIType")]] == "Column"){
           if(input[[paste0(panel, "-ssRateType")]] == "Constant"){
           modelUser <- tryCatch({
             metaExpr({
               ..(modelUser) %>%
                 addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountConstant")]]), II = ..(input[[paste0(panel, "-ssIIColumn")]]), rate = ..(input[[paste0(panel, "-ssRateConstant")]]))
             })
           },
           error = function(e) {
             return(modelUser)
           })
           } else {
             modelUser <- tryCatch({
               metaExpr({
                 ..(modelUser) %>%
                   addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountConstant")]]), II = ..(input[[paste0(panel, "-ssIIColumn")]]), rate = ..(input[[paste0(panel, "-ssRateColumn")]]))
               })
             },
             error = function(e) {
               return(modelUser)
             })
           }
         } else if(input[[paste0(panel, "-ssAmountType")]] == "Column" && input[[paste0(panel, "-ssIIType")]] == "Constant"){
           if(input[[paste0(panel, "-ssRateType")]] == "Constant"){
           modelUser <- tryCatch({
             metaExpr({
               ..(modelUser) %>%
                 addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountColumn")]]), II = ..(input[[paste0(panel, "-ssIIConstant")]]),  rate = ..(input[[paste0(panel, "-ssRateConstant")]]))
             })
           },
           error = function(e) {
             return(modelUser)
           })
           } else {
             modelUser <- tryCatch({
               metaExpr({
                 ..(modelUser) %>%
                   addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountColumn")]]), II = ..(input[[paste0(panel, "-ssIIConstant")]]),  rate = ..(input[[paste0(panel, "-ssRateColumn")]]))
               })
             },
             error = function(e) {
               return(modelUser)
             })
           }
         } else {
           if(input[[paste0(panel, "-ssRateType")]] == "Constant"){
           modelUser <- tryCatch({
             metaExpr({
               ..(modelUser) %>%
                 addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountColumn")]]), II = ..(input[[paste0(panel, "-ssIIColumn")]]),  rate = ..(input[[paste0(panel, "-ssRateConstant")]]))
             })
           },
           error = function(e) {
             return(modelUser)
           })
           } else {
             modelUser <- tryCatch({
               metaExpr({
                 ..(modelUser) %>%
                   addDoseCycle(type = "SteadyState", name = ..(input[[paste0(panel, "-ssDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-ssAmountColumn")]]), II = ..(input[[paste0(panel, "-ssIIColumn")]]),  rate = ..(input[[paste0(panel, "-ssRateColumn")]]))
               })
             },
             error = function(e) {
               return(modelUser)
             })
           }
         }
         
       }
      }
    }
  }
  return(modelUser)
}



addl_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("addlModule"))
  )
}

addl_panel <- function(id, doseName, data) {
  moduleServer(id,
               function(input, output, session) {
                 # generate the UI on the server side
                 ns <- session$ns
                 panel_number <- regmatches(id,
                                            regexpr("[0-9]+", id))
                 output$addlModule <- renderUI({
                   tags$div(id = id,
                            bslib::card(class = "input-options",
                              bslib::card_title("Dose Cycle"),
                              fluidRow(
                                style = "margin-bottom: 1rem;",
                                column(width = 3,
                                       selectInput(inputId = ns("addlDoseOrder"), label = "Dosepoint", choices = c("dosepoint"))
                                ),
                                column(width = 3,
                                       selectInput(inputId = ns("addlDoseSelect"), label = "Dosing Compartment", choices = c(doseName()))
                                ),
                                column(width = 3,
                                       selectInput(inputId = ns("addlDoseAdministration"), label = "Administration", choices = c("Bolus", "Infusion"))
                                )
                              ),
                              fluidRow(
                                column(width = 3,
                                       selectInput(inputId = ns("addlAmountType"), label = "Amount", choices = c("Constant", "Column"))
                                ),
                                column(width = 3,
                                       conditionalPanel("input.addlAmountType == 'Constant'", ns = ns,
                                                        numericInput(inputId = ns("addlAmountConstant"), label = "Amount Value", min = 0, max = Inf, value = 1)
                                       ),
                                       conditionalPanel("input.addlAmountType == 'Column'", ns = ns,
                                                        selectInput(inputId = ns("addlAmountColumn"), label = "Amount Column",  choices = c("", names(data)))
                                       )
                                )
                              ),
                              conditionalPanel(
                                "input.addlDoseAdministration == 'Infusion'", ns = ns,
                                fluidRow(
                                  column(
                                    width = 2, style = "padding-top: 10px;",
                                    checkboxInput(inputId = ns("addlisDuration"), label = "Duration", value = FALSE)
                                  )
                                ),
                                conditionalPanel(
                                  "input.addlisDuration == true && input.addlDoseAdministration == 'Infusion'", ns = ns,
                                  fluidRow(
                                    column(width = 3, 
                                           selectInput(inputId = ns("addlDurationType"), label = "Duration", choices = c("Constant", "Column"))
                                    ),
                                    column(width = 3,
                                           conditionalPanel("input.addlDoseAdministration == 'Infusion' && input.addlisDuration == true && input.addlDurationType == 'Constant'", ns = ns,
                                                            numericInput(inputId = ns("addlDurationConstant"), label = "Duration Value", min = 0, max = Inf, value = 1)
                                           ),
                                           conditionalPanel("input.addlDoseAdministration == 'Infusion' && input.addlisDuration == true && input.addlDurationType == 'Column'", ns = ns,
                                                            selectInput(inputId = ns("addlDurationColumn"), label = "Duration Column",  choices = c("", names(data)))
                                           )
                                    )
                                  )
                                ),
                                conditionalPanel(
                                  "input.addlisDuration == false && input.addlDoseAdministration == 'Infusion'", ns = ns,
                                  fluidRow(
                                    column(width = 3,
                                           selectInput(inputId = ns("addlRateType"), label = "Rate", choices = c("Constant", "Column"))
                                    ),
                                    column(width = 3,
                                           conditionalPanel("input.addlDoseAdministration == 'Infusion' && input.addlisDuration == false && input.addlRateType == 'Constant'", ns = ns,
                                                            numericInput(inputId = ns("addlRateConstant"), label = "Rate Value", min = 0, max = Inf, value = 1)
                                           ),
                                           conditionalPanel("input.addlDoseAdministration == 'Infusion' && input.addlisDuration == false && input.addlRateType == 'Column'", ns = ns,
                                                            selectInput(inputId = ns("addlRateColumn"), label = "Rate Column", choices = c("", names(data)))
                                           )
                                    )
                                  )
                                )
                              ),
                              fluidRow(
                                column(width = 3,
                                       selectInput(inputId = ns("addlIIType"), label = "II", choices = c("Constant", "Column"))
                                ),
                                column(width = 3,
                                       conditionalPanel("input.addlIIType == 'Constant'", ns = ns,
                                                        numericInput(inputId = ns("addlIIConstant"), label = "II Value", min = 0, max = Inf, value = 24)
                                       ),
                                       conditionalPanel("input.addlIIType == 'Column'", ns = ns,
                                                        selectInput(inputId = ns("addlIIColumn"), label = "II Column", choices = c("", names(data)))
                                       )
                                )
                              ),
                              fluidRow(
                                column(width = 1, offset = 11, 
                                       shinyWidgets::actionBttn(ns("removeaddlBtn"), label = NULL, icon = shiny::icon("minus"), style = "material-circle" )
                                )
                              )
                            ),
                            div(style = "padding:10px;")
                     )
                 })
                 
                 outputOptions(output, "addlModule", suspendWhenHidden = FALSE)  
                 
                 # remove the element
                 observeEvent(input$removeaddlBtn, {
                   removeUI(selector = paste0("div#", id))
                   runjs(paste0("Shiny.onInputChange('", id, "-addlDoseAdministration', null)"))
                   runjs(paste0("Shiny.onInputChange('", id, "-addlIIConstant', null)"))
                   runjs(paste0("Shiny.onInputChange('", id, "-addlIIColumn', null)"))
                 })
               })
  
  return(list(
    dropdown = reactive(input$addlDoseAdministration)
  ))
}



renderMetaDoseADDL <- function(modelUser, input){
  posADDLSelect <- grep(pattern = "addlDoseAdministration", names(input))
  
  
  addl_panel_inputs <- c()
  for(i in posADDLSelect){
    inputname <- names(input)[[i]]
    addl_panel_inputs <- c(addl_panel_inputs, inputname)
  }
  
  #panels are not automatically sorted when new ones created, so new dose cycle inputs appear out of order in 
  # possSSelect, must manually sort
  
  addl_panel_inputs <- sort(addl_panel_inputs)
  
  for(i in seq_along(addl_panel_inputs)){
    n <- addl_panel_inputs[[i]]
    if(!is.null(input[[n]])){
      addl_dosename <- input[[n]]
      panel <- sub("-.*", "", n)
      if(input[[paste0(panel, "-addlDoseAdministration")]] == "Bolus"){
        if(input[[paste0(panel, "-addlAmountType")]] == "Constant" && input[[paste0(panel, "-addlIIType")]] == "Constant"){
          tryCatch({
          modelUser <-  metaExpr({
              ..(modelUser) %>%
                addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Bolus", amount = ..(input[[paste0(panel, "-addlAmountConstant")]]), II = ..(input[[paste0(panel, "-addlIIConstant")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
        } else if(input[[paste0(panel, "-addlAmountType")]] == "Constant" && input[[paste0(panel, "-addlIIType")]] == "Column"){
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Bolus", amount = ..(input[[paste0(panel, "-addlAmountConstant")]]), II = ..(input[[paste0(panel, "-addlIIColumn")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
          
        } else if(input[[paste0(panel, "-addlAmountType")]] == "Column" && input[[paste0(panel, "-addlIIType")]] == "Constant"){
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Bolus", amount = ..(input[[paste0(panel, "-addlAmountColumn")]]), II = ..(input[[paste0(panel, "-addlIIConstant")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
          
        } else {
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Bolus", amount = ..(input[[paste0(panel, "-addlAmountColumn")]]), II = ..(input[[paste0(panel, "-addlIIColumn")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
        }
      } else { #Infusion Administration
        if(input[[paste0(panel, "-addlisDuration")]] == TRUE){
          if(input[[paste0(panel, "-addlAmountType")]] == "Constant" && input[[paste0(panel, "-addlIIType")]] == "Constant"){
            if(input[[paste0(panel, "-addlDurationType")]] == "Constant"){
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountConstant")]]), II = ..(input[[paste0(panel, "-addlIIConstant")]]), duration = ..(input[[paste0(panel, "-addlDurationConstant")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountConstant")]]), II = ..(input[[paste0(panel, "-addlIIConstant")]]), duration = ..(input[[paste0(panel, "-addlDurationColumn")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else if(input[[paste0(panel, "-addlAmountType")]] == "Constant" && input[[paste0(panel, "-addlIIType")]] == "Column"){
            if(input[[paste0(panel, "-addlDurationType")]] == "Constant"){
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountConstant")]]), II = ..(input[[paste0(panel, "-addlIIColumn")]]), duration = ..(input[[paste0(panel, "-addlDurationConstant")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountConstant")]]), II = ..(input[[paste0(panel, "-addlIIColumn")]]), duration = ..(input[[paste0(panel, "-addlDurationColumn")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else if(input[[paste0(panel, "-addlAmountType")]] == "Column" && input[[paste0(panel, "-addlIIType")]] == "Constant"){
            if(input[[paste0(panel, "-addlDurationType")]] == "Constant"){
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountColumn")]]), II = ..(input[[paste0(panel, "-addlIIConstant")]]), duration = ..(input[[paste0(panel, "-addlDurationConstant")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountColumn")]]), II = ..(input[[paste0(panel, "-addlIIConstant")]]), duration = ..(input[[paste0(panel, "-addlDurationColumn")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else {
            if(input[[paste0(panel, "-addlDurationType")]] == "Constant"){
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountColumn")]]), II = ..(input[[paste0(panel, "-addlIIColumn")]]), duration = ..(input[[paste0(panel, "-addlDurationConstant")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountColumn")]]), II = ..(input[[paste0(panel, "-addlIIColumn")]]), duration = ..(input[[paste0(panel, "-addlDurationColumn")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            }
          }
        } else {
          if(input[[paste0(panel, "-addlAmountType")]] == "Constant" && input[[paste0(panel, "-addlIIType")]] == "Constant"){
            if(input[[paste0(panel, "-addlRateType")]] == "Constant"){
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountConstant")]]), II = ..(input[[paste0(panel, "-addlIIConstant")]]), rate = ..(input[[paste0(panel, "-addlRateConstant")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountConstant")]]), II = ..(input[[paste0(panel, "-addlIIConstant")]]), rate = ..(input[[paste0(panel, "-addlRateColumn")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else if(input[[paste0(panel, "-addlAmountType")]] == "Constant" && input[[paste0(panel, "-addlIIType")]] == "Column"){
            if(input[[paste0(panel, "-addlRateType")]] == "Constant"){
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountConstant")]]), II = ..(input[[paste0(panel, "-addlIIColumn")]]), rate = ..(input[[paste0(panel, "-addlRateConstant")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountConstant")]]), II = ..(input[[paste0(panel, "-addlIIColumn")]]), rate = ..(input[[paste0(panel, "-addlRateColumn")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else if(input[[paste0(panel, "-addlAmountType")]] == "Column" && input[[paste0(panel, "-addlIIType")]] == "Constant"){
            if(input[[paste0(panel, "-addlRateType")]] == "Constant"){
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountColumn")]]), II = ..(input[[paste0(panel, "-addlIIConstant")]]),  rate = ..(input[[paste0(panel, "-addlRateConstant")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountColumn")]]), II = ..(input[[paste0(panel, "-addlIIConstant")]]),  rate = ..(input[[paste0(panel, "-addlRateColumn")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else {
            if(input[[paste0(panel, "-addlRateType")]] == "Constant"){
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountColumn")]]), II = ..(input[[paste0(panel, "-addlIIColumn")]]),  rate = ..(input[[paste0(panel, "-addlRateConstant")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                metaExpr({
                  ..(modelUser) %>%
                    addDoseCycle(type = "ADDL", name = ..(input[[paste0(panel, "-addlDoseSelect")]]), administration = "Infusion", amount = ..(input[[paste0(panel, "-addlAmountColumn")]]), II = ..(input[[paste0(panel, "-addlIIColumn")]]),  rate = ..(input[[paste0(panel, "-addlRateColumn")]]))
                })
              },
              error = function(e) {
                return(modelUser)
              })
            }
          }
          
        }
      }
    }
  }
  return(modelUser)
}


renderTextualDoseADDL <- function(modelUser, input){
  posADDLSelect <- grep(pattern = "addlDoseAdministration", names(input))
  
  for(i in posADDLSelect){
    n <- names(input)[[i]]
    if(!is.null(input[[n]])){
      addl_dosename <- input[[n]]
      panel <- sub("-.*", "", n)
      if(input[[paste0(panel, "-addlDoseAdministration")]] == "Bolus"){
        if(input[[paste0(panel, "-addlAmountType")]] == "Constant" && input[[paste0(panel, "-addlIIType")]] == "Constant"){
          tryCatch({
            modelUser <-  modelUser %>%
                addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Bolus", amount = input[[paste0(panel, "-addlAmountConstant")]], II = input[[paste0(panel, "-addlIIConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
          },
          error = function(e) {
            return(modelUser)
          })
        } else if(input[[paste0(panel, "-addlAmountType")]] == "Constant" && input[[paste0(panel, "-addlIIType")]] == "Column"){
          tryCatch({
          modelUser <- modelUser %>%
                addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Bolus", amount = input[[paste0(panel, "-addlAmountConstant")]], II = input[[paste0(panel, "-addlIIColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
          },
          error = function(e) {
            return(modelUser)
          })
          
        } else if(input[[paste0(panel, "-addlAmountType")]] == "Column" && input[[paste0(panel, "-addlIIType")]] == "Constant"){
          modelUser <- tryCatch({
              modelUser %>%
                addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Bolus", amount = input[[paste0(panel, "-addlAmountColumn")]], II = input[[paste0(panel, "-addlIIConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
          },
          error = function(e) {
            return(modelUser)
          })
          
        } else {
          modelUser <- tryCatch({
              modelUser %>%
                addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Bolus", amount = input[[paste0(panel, "-addlAmountColumn")]], II = input[[paste0(panel, "-addlIIColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
          },
          error = function(e) {
            return(modelUser)
          })
        }
      } else { #Infusion Administration
        if(input[[paste0(panel, "-addlisDuration")]] == TRUE){
          if(input[[paste0(panel, "-addlAmountType")]] == "Constant" && input[[paste0(panel, "-addlIIType")]] == "Constant"){
            if(input[[paste0(panel, "-addlDurationType")]] == "Constant"){
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountConstant")]], II = input[[paste0(panel, "-addlIIConstant")]], duration = input[[paste0(panel, "-addlDurationConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountConstant")]], II = input[[paste0(panel, "-addlIIConstant")]], duration = input[[paste0(panel, "-addlDurationColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else if(input[[paste0(panel, "-addlAmountType")]] == "Constant" && input[[paste0(panel, "-addlIIType")]] == "Column"){
            if(input[[paste0(panel, "-addlDurationType")]] == "Constant"){
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountConstant")]], II = input[[paste0(panel, "-addlIIColumn")]], duration = input[[paste0(panel, "-addlDurationConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountConstant")]], II = input[[paste0(panel, "-addlIIColumn")]], duration = input[[paste0(panel, "-addlDurationColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else if(input[[paste0(panel, "-addlAmountType")]] == "Column" && input[[paste0(panel, "-addlIIType")]] == "Constant"){
            if(input[[paste0(panel, "-addlDurationType")]] == "Constant"){
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountColumn")]], II = input[[paste0(panel, "-addlIIConstant")]], duration = input[[paste0(panel, "-addlDurationConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountColumn")]], II = input[[paste0(panel, "-addlIIConstant")]], duration = input[[paste0(panel, "-addlDurationColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else {
            if(input[[paste0(panel, "-addlDurationType")]] == "Constant"){
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountColumn")]], II = input[[paste0(panel, "-addlIIColumn")]], duration = input[[paste0(panel, "-addlDurationConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountColumn")]], II = input[[paste0(panel, "-addlIIColumn")]], duration = input[[paste0(panel, "-addlDurationColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          }
        } else {
          if(input[[paste0(panel, "-addlAmountType")]] == "Constant" && input[[paste0(panel, "-addlIIType")]] == "Constant"){
            if(input[[paste0(panel, "-addlRateType")]] == "Constant"){
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountConstant")]], II = input[[paste0(panel, "-addlIIConstant")]], rate = input[[paste0(panel, "-addlRateConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountConstant")]], II = input[[paste0(panel, "-addlIIConstant")]], rate = input[[paste0(panel, "-addlRateColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else if(input[[paste0(panel, "-addlAmountType")]] == "Constant" && input[[paste0(panel, "-addlIIType")]] == "Column"){
            if(input[[paste0(panel, "-addlRateType")]] == "Constant"){
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountConstant")]], II = input[[paste0(panel, "-addlIIColumn")]], rate = input[[paste0(panel, "-addlRateConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountConstant")]], II = input[[paste0(panel, "-addlIIColumn")]], rate = input[[paste0(panel, "-addlRateColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else if(input[[paste0(panel, "-addlAmountType")]] == "Column" && input[[paste0(panel, "-addlIIType")]] == "Constant"){
            if(input[[paste0(panel, "-addlRateType")]] == "Constant"){
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountColumn")]], II = input[[paste0(panel, "-addlIIConstant")]],  rate = input[[paste0(panel, "-addlRateConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountColumn")]], II = input[[paste0(panel, "-addlIIConstant")]],  rate = input[[paste0(panel, "-addlRateColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else {
            if(input[[paste0(panel, "-addlRateType")]] == "Constant"){
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountColumn")]], II = input[[paste0(panel, "-addlIIColumn")]],  rate = input[[paste0(panel, "-addlRateConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                  modelUser %>%
                    addDoseCycle(type = "ADDL", name = input[[paste0(panel, "-addlDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-addlAmountColumn")]], II = input[[paste0(panel, "-addlIIColumn")]],  rate = input[[paste0(panel, "-addlRateColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-addlDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              
            })
          }
          
        }
      }
     }
    }
  }
  return(modelUser)
}


renderTextualDoseSS <- function(modelUser, input){
  posSSSelect <- grep(pattern = "ssDoseAdministration", names(input))
  
  for(i in posSSSelect){
    n <- names(input)[[i]]
    if(!is.null(input[[n]])){
      ss_dosename <- input[[n]]
      panel <- sub("-.*", "", n)
      if(input[[paste0(panel, "-ssDoseAdministration")]] == "Bolus"){
        if(input[[paste0(panel, "-ssAmountType")]] == "Constant" && input[[paste0(panel, "-ssIIType")]] == "Constant"){
          tryCatch({
            modelUser <-  modelUser %>%
              addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Bolus", amount = input[[paste0(panel, "-ssAmountConstant")]], II = input[[paste0(panel, "-ssIIConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
          },
          error = function(e) {
            return(modelUser)
          })
        } else if(input[[paste0(panel, "-ssAmountType")]] == "Constant" && input[[paste0(panel, "-ssIIType")]] == "Column"){
          tryCatch({
            modelUser <- modelUser %>%
              addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Bolus", amount = input[[paste0(panel, "-ssAmountConstant")]], II = input[[paste0(panel, "-ssIIColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
          },
          error = function(e) {
            return(modelUser)
          })
          
        } else if(input[[paste0(panel, "-ssAmountType")]] == "Column" && input[[paste0(panel, "-ssIIType")]] == "Constant"){
          modelUser <- tryCatch({
            modelUser %>%
              addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Bolus", amount = input[[paste0(panel, "-ssAmountColumn")]], II = input[[paste0(panel, "-ssIIConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
          },
          error = function(e) {
            return(modelUser)
          })
          
        } else {
          modelUser <- tryCatch({
            modelUser %>%
              addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Bolus", amount = input[[paste0(panel, "-ssAmountColumn")]], II = input[[paste0(panel, "-ssIIColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
          },
          error = function(e) {
            return(modelUser)
          })
        }
      } else { #Infusion Administration
        if(input[[paste0(panel, "-ssisDuration")]] == TRUE){
          if(input[[paste0(panel, "-ssAmountType")]] == "Constant" && input[[paste0(panel, "-ssIIType")]] == "Constant"){
            if(input[[paste0(panel, "-ssDurationType")]] == "Constant"){
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountConstant")]], II = input[[paste0(panel, "-ssIIConstant")]], duration = input[[paste0(panel, "-ssDurationConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountConstant")]], II = input[[paste0(panel, "-ssIIConstant")]], duration = input[[paste0(panel, "-ssDurationColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else if(input[[paste0(panel, "-ssAmountType")]] == "Constant" && input[[paste0(panel, "-ssIIType")]] == "Column"){
            if(input[[paste0(panel, "-ssDurationType")]] == "Constant"){
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountConstant")]], II = input[[paste0(panel, "-ssIIColumn")]], duration = input[[paste0(panel, "-ssDurationConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountConstant")]], II = input[[paste0(panel, "-ssIIColumn")]], duration = input[[paste0(panel, "-ssDurationColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else if(input[[paste0(panel, "-ssAmountType")]] == "Column" && input[[paste0(panel, "-ssIIType")]] == "Constant"){
            if(input[[paste0(panel, "-ssDurationType")]] == "Constant"){
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountColumn")]], II = input[[paste0(panel, "-ssIIConstant")]], duration = input[[paste0(panel, "-ssDurationConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountColumn")]], II = input[[paste0(panel, "-ssIIConstant")]], duration = input[[paste0(panel, "-ssDurationColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else {
            if(input[[paste0(panel, "-ssDurationType")]] == "Constant"){
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountColumn")]], II = input[[paste0(panel, "-ssIIColumn")]], duration = input[[paste0(panel, "-ssDurationConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountColumn")]], II = input[[paste0(panel, "-ssIIColumn")]], duration = input[[paste0(panel, "-ssDurationColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          }
        } else {
          if(input[[paste0(panel, "-ssAmountType")]] == "Constant" && input[[paste0(panel, "-ssIIType")]] == "Constant"){
            if(input[[paste0(panel, "-ssRateType")]] == "Constant"){
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountConstant")]], II = input[[paste0(panel, "-ssIIConstant")]], rate = input[[paste0(panel, "-ssRateConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountConstant")]], II = input[[paste0(panel, "-ssIIConstant")]], rate = input[[paste0(panel, "-ssRateColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else if(input[[paste0(panel, "-ssAmountType")]] == "Constant" && input[[paste0(panel, "-ssIIType")]] == "Column"){
            if(input[[paste0(panel, "-ssRateType")]] == "Constant"){
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountConstant")]], II = input[[paste0(panel, "-ssIIColumn")]], rate = input[[paste0(panel, "-ssRateConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountConstant")]], II = input[[paste0(panel, "-ssIIColumn")]], rate = input[[paste0(panel, "-ssRateColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else if(input[[paste0(panel, "-ssAmountType")]] == "Column" && input[[paste0(panel, "-ssIIType")]] == "Constant"){
            if(input[[paste0(panel, "-ssRateType")]] == "Constant"){
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountColumn")]], II = input[[paste0(panel, "-ssIIConstant")]],  rate = input[[paste0(panel, "-ssRateConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountColumn")]], II = input[[paste0(panel, "-ssIIConstant")]],  rate = input[[paste0(panel, "-ssRateColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            }
          } else {
            if(input[[paste0(panel, "-ssRateType")]] == "Constant"){
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountColumn")]], II = input[[paste0(panel, "-ssIIColumn")]],  rate = input[[paste0(panel, "-ssRateConstant")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
              })
            } else {
              modelUser <- tryCatch({
                modelUser %>%
                  addDoseCycle(type = "SteadyState", name = input[[paste0(panel, "-ssDoseSelect")]], administration = "Infusion", amount = input[[paste0(panel, "-ssAmountColumn")]], II = input[[paste0(panel, "-ssIIColumn")]],  rate = input[[paste0(panel, "-ssRateColumn")]], isSecondDose = ifelse(input[[paste0(panel, "-ssDoseOrder")]] == "dosepoint2", TRUE, FALSE))
              },
              error = function(e) {
                return(modelUser)
                
              })
            }
            
          }
        }
      }
    }
  }
  return(modelUser)
}



addTextualLabel <- function(model, input){
  colnames <- model@columnMapping@mapping
  tryCatch({
  for(i in seq_along(colnames)){
    if(input[[paste0("col", i)]] != " " && colnames[[i]]@variableType$type == "covariate"){
          model <- addLabel(model,
                            covariate = input[[paste0("col", i)]],
                            levels = as.numeric(unlist(strsplit(input[[paste0("covLevels", i)]], split = ","))),
                            labels = trimws(unlist(strsplit(input[[paste0("covLabels", i)]], split = ","))))
      }
  }
  }, error = function(e){
    return(model)
  })
  return(model)
}



getUserDefMDV <- function(userDefinedExtraDefs) {
  if (length(userDefinedExtraDefs) == 0)
    return(c())
  
  posMDV <- grep("^mdv", userDefinedExtraDefs)
  
  if (length(posMDV) > 0) {
    mdvline <- userDefinedExtraDefs[[posMDV]]
    mdvcol <- gsub(".*\\(\"(.*)\"\\).*", "\\1", mdvline)
  } else {
    mdvcol <- NULL
  }
  
  mdvcol
}

splitDefs <- function(userDefinedExtraDefs) {
  if (length(userDefinedExtraDefs) == 0)
    return(c())
  
  userDefinedExtraDefs <-
    unlist(strsplit(unlist(userDefinedExtraDefs), split = "\n"))
  userDefinedExtraDefs <-
    userDefinedExtraDefs[userDefinedExtraDefs != ""]
  
  userDefinedExtraDefs
}


getUserDefSS <- function(userDefinedExtraDefs) {
  if (length(userDefinedExtraDefs) == 0)
    return(c())
  
  posSS <- grep("^sscol", userDefinedExtraDefs)
  
  if (length(posSS) > 0) {
    ssline <- userDefinedExtraDefs[[posSS]]
    sscol <- gsub(".*\\((.*)\\).*", "\\1", ssline)
  } else {
    sscol <- NULL
  }
  
  sscol
}

getUserDefADDL <- function(userDefinedExtraDefs) {
  if (length(userDefinedExtraDefs) == 0)
    return(c())
  
  posADDL <- grep("^addlcol", userDefinedExtraDefs)
  
  if (length(posADDL) > 0) {
    addline <- userDefinedExtraDefs[[posADDL]]
    addlcol <- gsub(".*\\((.*)\\).*", "\\1", addline)
  } else {
    addlcol <- NULL
  }
  
  addlcol
}

getUserDefII <- function(userDefinedExtraDefs) {
  if (length(userDefinedExtraDefs) == 0)
    return(c())
  
  posII <- grep("^iicol", userDefinedExtraDefs)
  
  if (length(posII) > 0) {
    iiline <- userDefinedExtraDefs[[posII]]
    iicol <- gsub(".*\\((.*)\\).*", "\\1", iiline)
  } else {
    iicol <- NULL
  }
  
  iicol
}

getUserDefSSOff <- function(userDefinedExtraDefs) {
  if (length(userDefinedExtraDefs) == 0)
    return(c())
  
  posSSOff <- grep("^ssoffcol", userDefinedExtraDefs)
  
  if (length(posSSOff) > 0) {
    ssoffline <- userDefinedExtraDefs[[posSSOff]]
    ssoffcol <- gsub(".*\\((.*)\\).*", "\\1", ssoffline)
  } else {
    ssoffcol <- NULL
  }
  
  ssoffcol
}

getUserDefReset <- function(model, userDefinedExtraDefs) {
  if (length(userDefinedExtraDefs) == 0)
    return(c())
  
  posReset <- grep("^reset", userDefinedExtraDefs)
  
  if (length(posReset) > 0) {
    resetline <- userDefinedExtraDefs[[posReset]]
    resetvector <- unlist(strsplit(resetline, split = "\""))
    resetcol <- resetvector[[2]]
    resetLow <-
      as.numeric(gsub(".*\\((.*)\\,.*", "\\1", resetvector[[3]]))
    resetHi <- gsub(".*\\,", "\\1", resetvector[[3]])
    resetHi <- as.numeric(gsub("[^[:digit:].]", "\\1",  resetHi))
    resetinfo <- list(col = resetcol,
                      low = resetLow,
                      hi = resetHi)
  } else {
    resetinfo <- NULL
  }
  # } else {
  #   if(model@hasResetInfo){
  #     resetcol <- lookupColname(model@columnMapping@mapping, "Reset")
  #     resetLow <- model@resetInfo@low
  #     resetHi <- model@resetInfo@hi
  #     resetinfo <- list(col = resetcol,
  #                       low = resetLow,
  #                       hi = resetHi)
  #
  #   } else {
  #     resetinfo <- NULL
  #   }
  # }

  resetinfo
}

getUserDefDoseCycle <- function(userDefinedExtraDefs) {
  if (length(userDefinedExtraDefs) == 0)
    return(c())
  
  posSSDose <- grep("^ss\\(", userDefinedExtraDefs)
  
  if (length(posSSDose) > 0) {
    ssdoseline <- userDefinedExtraDefs[[posSSDose]]
  } else {
    ssdoseline <- NULL
  }
  
  posADDLDose <- grep("^addl\\(", userDefinedExtraDefs)
  
  if (length(posADDLDose) > 0) {
    addldoseline <- userDefinedExtraDefs[[posADDLDose]]
  } else {
    addldoseline <- NULL
  }
  
  doselines <- c(ssdoseline, addldoseline)
  
  doselines
}

rmTablesFromColDef <- function(userDefinedExtraDefs) {
  posTable <- grep("^table|^simtable", userDefinedExtraDefs)
  
  if (length(posTable) > 0) {
    userDefinedExtraDefs <- userDefinedExtraDefs[-posTable]
  }
  
  userDefinedExtraDefs
}

