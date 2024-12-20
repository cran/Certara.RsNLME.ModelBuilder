
extradef_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("extraDefModule"))
  )
}

extradef_panel <- function(id, value = NULL) {
  moduleServer(id,
               function(input, output, session) {
                 # generate the UI on the server side
                 ns <- session$ns
                 panel_number <- regmatches(id,
                                            regexpr("[0-9]+", id))
                 output$extraDefModule <- renderUI({
                   # render_material_from_server(
                   tags$div(id = id,
                     fluidRow(
                       column(width = 10,
                              textInput(ns("extraDef"), label = "",  value = value, placeholder = "Enter Column Definition")
                       ),
                       column(width = 2,
                              shinyWidgets::actionBttn(ns("removeExtraDefBtn"), label = NULL, icon = shiny::icon("minus"), style = "material-circle", size = "xs")
                       )
                     ),
                     div(style = "padding:10px;")
                   )
                   # )
                 })
                 
                 outputOptions(output, "extraDefModule", suspendWhenHidden = FALSE)  
                 
                 
                 # remove the element
                 observeEvent(input$removeExtraDefBtn, {
                   removeUI(selector = paste0("div#", id))
                   runjs(paste0("Shiny.onInputChange('", id, "-extraDef', null)"))
                 })
               })
  
  return(list(
    dropdown = reactive(input$extraDef)
  ))
}



renderMetaExtraDef <- function(modelUser, input){
  posExraDefSelect <- grep(pattern = "extraDef", names(input))
  
  for(i in posExraDefSelect){
    n <- names(input)[[i]]
    if(!is.null(input[[n]])){
      panel <- sub("-.*", "", n)
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                addExtraDef(value = ..(input[[paste0(panel, "-extraDef")]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
        }
  }
  return(modelUser)
  
}
# col_panel_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     uiOutput(ns("col"))
#   )
# }
# 
# col_panel <- function(id, choicesAvail, modelVariableNames) {
#   moduleServer(id,
#                function(input, output, session) {
#                  # generate the UI on the server side
#                  ns <- session$ns
#                  
# 
#                  output$col <- renderUI({
#                    if(length(modelVariableNames) == 4){
#                      uiColDef <- tags$div(id = id,
#                                           material_row(
#                                             material_column(width = 8,
#                                                             div(style = "padding-top:3px;"),
#                                                             material_dropdown(input_id = ns("colSelect1"), label = modelVariableNames[[1]], choices = choicesAvail, selected = NULL),
#                                                             div(style = "padding-top:3px;"),
#                                                             material_dropdown(input_id = ns("colSelect2"), label = modelVariableNames[[2]], choices = choicesAvail, selected = NULL),
#                                                             div(style = "padding-top:3px;"),
#                                                             material_dropdown(input_id = ns("colSelect3"), label = modelVariableNames[[3]], choices = choicesAvail, selected = NULL),
#                                                             div(style = "padding-top:3px;"),
#                                                             material_dropdown(input_id = ns("colSelect4"), label = modelVariableNames[[4]], choices = choicesAvail, selected = NULL)
#                                             )
#                                           )
#                      )
#                    } else if(length(modelVariableNames) == 5){
#                      uiColDef <- tags$div(id = id,
#                                           material_row(
#                                             material_column(width = 8,
#                                                             div(style = "padding-top:3px;"),
#                                                             material_dropdown(input_id = ns("colSelect1"), label = modelVariableNames[[1]], choices = choicesAvail, selected = NULL),
#                                                             div(style = "padding-top:3px;"),
#                                                             material_dropdown(input_id = ns("colSelect2"), label = modelVariableNames[[2]], choices = choicesAvail, selected = NULL),
#                                                             div(style = "padding-top:3px;"),
#                                                             material_dropdown(input_id = ns("colSelect3"), label = modelVariableNames[[3]], choices = choicesAvail, selected = NULL),
#                                                             div(style = "padding-top:3px;"),
#                                                             material_dropdown(input_id = ns("colSelect4"), label = modelVariableNames[[4]], choices = choicesAvail, selected = NULL),
#                                                             div(style = "padding-top:3px;"),
#                                                             material_dropdown(input_id = ns("colSelect5"), label = modelVariableNames[[5]], choices = choicesAvail, selected = NULL)
#                                             )
#                                           )
#                      ) 
#                      } else {
#                      return()
#                    }
#                    
#                    render_material_from_server(
#                      uiColDef
#                    )
#                    
#                   })
#                  
#                  outputOptions(output, "col", suspendWhenHidden = TRUE)  
# 
#                  # remove the element
#                  # observeEvent(input$removeCovPkBtn, {
#                  #   removeUI(selector = paste0("div#", id))
#                  #   runjs(paste0("Shiny.onInputChange('", id, "-covSelect', null)"))
#                  # })
#                })
#   
#   return(list(
#     dropdown = reactive(input$colSelect)
#   ))
# }


# # #Column mapping
# # renderMetaColMap <- function(modelUser, input, modelVarNames){
# #   
# #   lenModelVar <- length(modelVarNames)
# #   
# #     modelUser <- metaExpr({
# #       ..(modelUser) %>%
# #         colMapping(c(..(modelVarNames$values[[1]]) = ..(input$col1_1), 
# #                      ..(modelVarNames$values[[2]]) = ..(input$col2_1),
# #                      ..(modelVarNames$values[[3]]) = ..(input$col3_1),
# #                      ..(modelVarNames$values[[4]]) = ..(input$col4_1))
# #         )
# #     })
# #    return(modelUser)
# # }
# 
# 
# 
# renderColumnMapping <- function(id,model, data, colMapInputs) {
#   
#   modelVariables <- unlist(modelVariableNames(model))
#   
#   modelVariablesLength <- length(modelVariables)
#   
#   availChoices <- c("None", names(data))
#   
#   #availChoices <- setdiff(availChoices, unlist(colMapInputs$values))
#   
#   if(modelVariablesLength == 0){
#     return()
#   }
#   
#   if(modelVariablesLength == 1){
#     uiColMap <-  tagList(
#       material_row(
#         material_column(width = 12,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                         material_dropdown(input_id = paste0("col1_", id), label = modelVariables[[1]], choices = availChoices, selected = colMapInputs$values$col1)
#         )
#       )
#     )
#   } else if(modelVariablesLength == 2) {  
#     uiColMap <-  tagList(
#       material_row(
#         material_column(width = 12,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                         material_dropdown(input_id = paste0("col1_", id), label = modelVariables[[1]], choices = availChoices, selected = colMapInputs$values$col1, multiple = TRUE),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col2_", id), label = modelVariables[[2]], choices = availChoices, selected = colMapInputs$values$col2)
#         )
#       )
#     )
#   } else if(modelVariablesLength == 3) {  
#     uiColMap <-  tagList(
#       material_row(
#         material_column(width = 12,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                         material_dropdown(input_id = paste0("col1_", id), label = modelVariables[[1]], choices = availChoices, selected = colMapInputs$values$col1, multiple = TRUE),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col2_", id), label = modelVariables[[2]], choices = availChoices, selected = colMapInputs$values$col2),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col3_", id), label = modelVariables[[3]], choices = availChoices, selected = colMapInputs$values$col3)
#         )
#       )
#     )
#   } else if(modelVariablesLength == 4) {  
#     uiColMap <-  tagList(
#       material_row(
#         material_column(width = 12,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                         material_dropdown(input_id = paste0("col1_", id), label = modelVariables[[1]], choices = availChoices, selected = colMapInputs$values$col1, multiple = TRUE),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col2_", id), label = modelVariables[[2]], choices = availChoices, selected = colMapInputs$values$col2),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col3_", id), label = modelVariables[[3]], choices = availChoices, selected = colMapInputs$values$col3),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col4_", id), label = modelVariables[[4]], choices = availChoices, selected = colMapInputs$values$col4)
#                         
#         )
#       )
#     )
#   } else if(modelVariablesLength == 5) {  
#     uiColMap <-  tagList(
#       material_row(
#         material_column(width = 12,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                         material_dropdown(input_id = paste0("col1_", id), label = modelVariables[[1]], choices = availChoices, selected = colMapInputs$values$col1, multiple = TRUE),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col2_", id), label = modelVariables[[2]], choices = availChoices, selected = colMapInputs$values$col2),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col3_", id), label = modelVariables[[3]], choices = availChoices, selected = colMapInputs$values$col3),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col4_", id), label = modelVariables[[4]], choices = availChoices, selected = colMapInputs$values$col4),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col5_", id), label = modelVariables[[5]], choices = availChoices, selected = colMapInputs$values$col5)
#                         
#                         
#         )
#       )
#     )
#   } else if(modelVariablesLength == 6) {  
#     uiColMap <-  tagList(
#       material_row(
#         material_column(width = 12,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                         material_dropdown(input_id = paste0("col1_", id), label = modelVariables[[1]], choices = availChoices, selected = colMapInputs$values$col1, multiple = TRUE),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col2_", id), label = modelVariables[[2]], choices = availChoices, selected = colMapInputs$values$col2),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col3_", id), label = modelVariables[[3]], choices = availChoices, selected = colMapInputs$values$col3),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col4_", id), label = modelVariables[[4]], choices = availChoices, selected = colMapInputs$values$col4),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col5_", id), label = modelVariables[[5]], choices = availChoices, selected = colMapInputs$values$col5),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col6_", id), label = modelVariables[[6]], choices = availChoices, selected = colMapInputs$values$col6)
#                         
#                         
#         )
#       )
#     )
#   
#   } else if(modelVariablesLength == 7) {  
#     uiColMap <-  tagList(
#       material_row(
#         material_column(width = 12,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                         material_dropdown(input_id = paste0("col1_", id), label = modelVariables[[1]], choices = availChoices, selected = colMapInputs$values$col1, multiple = TRUE),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col2_", id), label = modelVariables[[2]], choices = availChoices, selected = colMapInputs$values$col2),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col3_", id), label = modelVariables[[3]], choices = availChoices, selected = colMapInputs$values$col3),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col4_", id), label = modelVariables[[4]], choices = availChoices, selected = colMapInputs$values$col4),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col5_", id), label = modelVariables[[5]], choices = availChoices, selected = colMapInputs$values$col5),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col6_", id), label = modelVariables[[6]], choices = availChoices, selected = colMapInputs$values$col6),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col7_", id), label = modelVariables[[7]], choices = availChoices, selected = colMapInputs$values$col7)
#                         
#                         
#         )
#       )
#     ) 
#   
#   } else if(modelVariablesLength == 8) {  
#     uiColMap <-  tagList(
#       material_row(
#         material_column(width = 12,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                         material_dropdown(input_id = paste0("col1_", id), label = modelVariables[[1]], choices = availChoices, selected = colMapInputs$values$col1, multiple = TRUE),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col2_", id), label = modelVariables[[2]], choices = availChoices, selected = colMapInputs$values$col2),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col3_", id), label = modelVariables[[3]], choices = availChoices, selected = colMapInputs$values$col3),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col4_", id), label = modelVariables[[4]], choices = availChoices, selected = colMapInputs$values$col4),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col5_", id), label = modelVariables[[5]], choices = availChoices, selected = colMapInputs$values$col5),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col6_", id), label = modelVariables[[6]], choices = availChoices, selected = colMapInputs$values$col6),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col7_", id), label = modelVariables[[7]], choices = availChoices, selected = colMapInputs$values$col7),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col8_", id), label = modelVariables[[8]], choices = availChoices, selected = colMapInputs$values$col8)
#                         
#                         
#         )
#       )
#     )
#   } else if(modelVariablesLength == 9) {  
#     uiColMap <-  tagList(
#       material_row(
#         material_column(width = 12,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                         material_dropdown(input_id = paste0("col1_", id), label = modelVariables[[1]], choices = availChoices, selected = colMapInputs$values$col1, multiple = TRUE),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col2_", id), label = modelVariables[[2]], choices = availChoices, selected = colMapInputs$values$col2),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col3_", id), label = modelVariables[[3]], choices = availChoices, selected = colMapInputs$values$col3),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col4_", id), label = modelVariables[[4]], choices = availChoices, selected = colMapInputs$values$col4),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col5_", id), label = modelVariables[[5]], choices = availChoices, selected = colMapInputs$values$col5),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col6_", id), label = modelVariables[[6]], choices = availChoices, selected = colMapInputs$values$col6),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col7_", id), label = modelVariables[[7]], choices = availChoices, selected = colMapInputs$values$col7),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col8_", id), label = modelVariables[[8]], choices = availChoices, selected = colMapInputs$values$col8),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col9_", id), label = modelVariables[[9]], choices = availChoices, selected = colMapInputs$values$col9)
#                         
#                         
#         )
#       )
#     )
#     
#   } else if(modelVariablesLength == 10) {  
#     uiColMap <-  tagList(
#       material_row(
#         material_column(width = 12,  style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px',
#                         material_dropdown(input_id = paste0("col1_", id), label = modelVariables[[1]], choices = availChoices, selected = colMapInputs$values$col1, multiple = TRUE),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col2_", id), label = modelVariables[[2]], choices = availChoices, selected = colMapInputs$values$col2),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col3_", id), label = modelVariables[[3]], choices = availChoices, selected = colMapInputs$values$col3),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col4_", id), label = modelVariables[[4]], choices = availChoices, selected = colMapInputs$values$col4),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col5_", id), label = modelVariables[[5]], choices = availChoices, selected = colMapInputs$values$col5),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col6_", id), label = modelVariables[[6]], choices = availChoices, selected = colMapInputs$values$col6),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col7_", id), label = modelVariables[[7]], choices = availChoices, selected = colMapInputs$values$col7),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col8_", id), label = modelVariables[[8]], choices = availChoices, selected = colMapInputs$values$col8),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col9_", id), label = modelVariables[[9]], choices = availChoices, selected = colMapInputs$values$col9),
#                         div(style = "padding:2px;"),
#                         material_dropdown(input_id = paste0("col10_", id), label = modelVariables[[10]], choices = availChoices, selected = colMapInputs$values$col10)
# 
#         )
#       )
#     )
#   
#   } else {
#     return()
#   }
#   uiColMap
# }


writeColsOut <- function(model, sortColumns = NULL) {
  

  colMap <- model@columnMapping@mapping
  modelType <- model@modelType@modelType
  on <- Certara.RsNLME::observationNames(model)
  # not typical observations are not included as LL count etc.
  # need to add them here
  for(mappedTerm in colMap) {
    if(.hasSlot(mappedTerm, "variableType") &&
       !is.null(mappedTerm@variableType$type) &&
       mappedTerm@variableType$type == "observation" &&
       !mappedTerm@variableName %in% on) {
      on <- c(on, mappedTerm@variableName)
    }
  }
  
  en <- observationExtraNames(model)
  cn <- Certara.RsNLME::covariateNames(model)
  dn <- Certara.RsNLME::doseNames(model)
  
  # RandomParam
  isSequential <- model@pkModelAttrs@isSequential
  if (isSequential) {
    rn <- randParameterNames(model)
    rcolMap <- model@randParamsMapping@mapping
  }
  eDoseLines <- Certara.RsNLME::extraDoseLines(model)
  
  sspos <- grep("ss\\(", eDoseLines)
  ss_eDoseLines <- eDoseLines[sspos]
  
  addlpos <- grep("addl\\(", eDoseLines)
  addl_eDoseLines <- eDoseLines[addlpos]
  
  if (length(ss_eDoseLines) > 1) {
    s1 <- gsub(pattern = "\\)$", "", ss_eDoseLines[[1]])
    s2 <- vector(mode = "character", length = length(ss_eDoseLines) - 1)
    for (i in 2:length(ss_eDoseLines)) {
      s2[[i]] <- ss_eDoseLines[[i]]
      s2[[i]] <- gsub("\\)$", "", s2[[i]])
      s2[[i]] <- gsub(".*?,", "", s2[[i]])
    }
    s2 <- paste0(s2, collapse = " ")
    
    ss_eDoseLines <- paste0(s1, s2, ")")
  }
  
  if (length(addl_eDoseLines) > 1) {
    a1 <- gsub(pattern = "\\)$", "", addl_eDoseLines[[1]])
    a2 <- vector(mode = "character", length = length(addl_eDoseLines) - 1)
    for (i in 2:length(addl_eDoseLines)) {
      a2[[i]] <- addl_eDoseLines[[i]]
      a2[[i]] <- gsub("\\)$", "", a2[[i]])
      a2[[i]] <- gsub(".*?,", "", a2[[i]])
    }
    a2 <- paste0(a2, collapse = " ")
    
    addl_eDoseLines <- paste0(a1, a2, ")")
  }
  
  eDoseLines <- c(ss_eDoseLines, addl_eDoseLines)
  
  vars <- c()
  lines <- c()
  if (model@isPopulation) {
    idcolNames <- strsplit(lookupColname(colMap, "id"), split = ",")[[1]]
    # type gives doublequotes
    idcolNamesquo <- paste(shQuote(trimws(idcolNames), type = "cmd"), collapse = ", ")
    lines <- c(lines, paste0("id(", idcolNamesquo, ")"))
  } else {
    if(missing(sortColumns)|
       is.null(sortColumns)|
       !.hasSlot(sortColumns, "sortColumnList") ) {
      # nothing to put into id
      lines <- c(lines, paste0("id(\"zzzDummyId\")"))
    } else if(sortColumns@numSortColumns == 0){
      # same as above; just cannot do all checks in one line
      lines <- c(lines, paste0("id(\"zzzDummyId\")"))
    } else if(sortColumns@numSortColumns > 5) {
      stop("Number of input data columns supplied ",
           "to 'SortColumns' cannot exceed 5 for individual models:\n",
           paste(sortColumns@sortColumnList, collapse = ", "),
           call. = FALSE)
    } else {
      idcolNamesquo <- paste0(shQuote(trimws(sortColumns@sortColumnList), type = "cmd"),
                              collapse = ", ")
      lines <- c(lines, paste0("id(", idcolNamesquo, ")"))
    }
  }
  
  if (model@isTimeBased) {
    timeCol <- lookupColname(colMap, "time")
    lines <- c(lines,
               paste0("time(\"", timeCol, "\")"))
  }
  
  for (doseName in dn) {
    colName <- lookupColname(colMap, doseName)
    if(colName %in% c("", "?")) {
      next()
    }
    
    extraDoseInfo <- ""
    
    if (!model@isTextual) {
      if (model@pkModelAttrs@infusionAllowed) {
        if (model@pkModelAttrs@isDuration) {
          infusionName <- paste0(doseName, "_Duration")
          dcolName <- lookupColname(colMap, infusionName)
          if(!dcolName %in% c("", "?")) {
            extraDoseInfo <- paste0(", duration=\"", dcolName, "\"")
          }
        } else {
          infusionName <- paste0(doseName, "_Rate")
          dcolName <- lookupColname(colMap, infusionName)
          if(!dcolName %in% c("", "?")) {
            extraDoseInfo <- paste0(", \"", dcolName, "\"")
          }
        }
      }
    } else if (.hasSlot(colMap[[doseName]], "variableType") &&
               !is.null(colMap[[doseName]]@variableType$Infusion)){
      Infusion <- colMap[[doseName]]@variableType$Infusion
      if(!is.na(Infusion)) {
        infusionName <- paste0(doseName, "_", Infusion)
        dcolName <- lookupColname(colMap, infusionName)
        if(!dcolName %in% c("", "?")) {
          if (Infusion == "Duration") {
            DurSuffix <- "duration="
          } else {
            DurSuffix <- ""
          }
          
          extraDoseInfo <- paste0(", ", DurSuffix, "\"", dcolName, "\"")
        }
      }
    }
    
    doseN <- ifelse((colMap[[doseName]]@variableType$DosepointN == 1), "dose", "dose2")
    # if the dosepoint was doubled, need to get rid of index
    doseName <- ifelse(colMap[[doseName]]@variableType$DosepointDouble,
                       gsub("_\\d$", "", doseName),
                       doseName)
    
    lines <- c(lines,
               paste0(doseN, "(", doseName, "<-\"", colName, "\"", extraDoseInfo, ")"))
    
  }
  
  lines <- c(lines, eDoseLines)
  
  if(lookupColname(colMap, "MDV") != "?" && lookupColname(colMap, "MDV") != ""){
    lines <- c(lines, paste0("mdv(\"", lookupColname(colMap, "MDV"), "\")"))
  }
  
  covarList <- model@covariateList
  for (covariate in cn) {
    colName <- lookupColname(colMap, covariate)
    covObject <- covarList[sapply(covarList,
                                  function(x, covariate){x@name == covariate},
                                  covariate)][[1]]
    lines <- c(lines,
               paste0("covr(", covariate, "<-\"", colName, "\"", covariatePartsString(covObject), ")"))
  }
  
  # are there some special covariates not in the list?
  for(mappedTerm in colMap) {
    if(.hasSlot(mappedTerm, "variableType") &&
       !is.null(mappedTerm@variableType$type) &&
       mappedTerm@variableType$type == "covariate" &&
       !mappedTerm@variableName %in% cn) {
      
      # we need cn to create posthoc file later
      cn <- c(cn, mappedTerm@variableName)
      colName <- mappedTerm@columnName
      if(mappedTerm@variableType$covType == COVAR_CATEGORY) {
        brackets <- "()"
      } else {
        brackets <- ""
      }
      lines <- c(lines,
                 paste0("covr(", mappedTerm@variableName, "<-\"", colName, "\"", brackets, ")"))
    }
  }
  
  if (length(en) == 0) {
    for (obsName in on) {
      colName <- lookupColname(colMap, obsName)
      if (colName != "?") {
        lines <- c(lines,
                   sprintf("obs(%s<-\"%s\")", obsName, colName))
      }
    }
  } else {
    # there are BQLs
    statementsWOComments <- .remove_comments(model@statements)
    
    for (obsName in on) {
      colName <- lookupColname(colMap, obsName)
      if (colName != "?") {
        bqlName <- paste0(obsName, "BQL")
        if (any(sapply(en, grepl, bqlName)) &&
            bqlName %in% names(colMap) &&
            lookupColname(colMap, bqlName) != "?") {
          colBQL <- lookupColname(colMap, bqlName)
          # use BQL column only if it is mapped
          s <- sprintf("obs(%s<-\"%s\", bql <- \"%s\") ", obsName, colName, colBQL)
          # throw a warning if static LLOQ is used AND BQL is mapped
          greplStaticLLOQString <- paste0("(?<=observe\\(", obsName, "=)[^,]+,bql=")
          if(any(grepl(greplStaticLLOQString, statementsWOComments, perl = TRUE))) {
            warning("For observation ", obsName, " a static LLOQ value requested,",
                    "\nbut ", bqlName, " mapped; static LLOQ value will be ignored.", call. = FALSE)
          }
          
        } else {
          s <- sprintf("obs(%s<-\"%s\")", obsName, colName)
        }
        lines <- c(lines, s)
      }
    }
  }
  
  # Check if A1strip Variable is mapped
  if ("A1Strip" %in% names(colMap)) {
    colName <- lookupColname(colMap, "A1Strip")
    if(colName != "?")
      lines <- c(lines, paste0("covr(A1Strip", "<-\"", colName, "\"", ")"))
  }
  
  if (isSequential) {
    for (r in rn) {
      colName <- lookupColname(rcolMap, r)
      lines <- c(lines, paste0("covr(", r, "<-\"", colName, "\"", ")"))
    }
  }
  
  # if(model@isPopulation) {
  #   if(length(cn) > 0) {
  #     indVar <- paste0(", covr(", paste(cn, collapse = ", "), "), ")
  #   } else {
  #     # no covariates
  #     indVar <- ", "
  #   }
  #   
  #   stparms <- paste(Certara.RsNLME::structuralParameterNames(model), collapse = ", ")
  #   
  #   if(length(stparms) != 0) {
  #     table <- paste0("table(file=\"posthoc.csv\"",
  #                     indVar,
  #                     stparms, ", mode=keep)")
  #     
  #     lines <- c(lines, table)
  #   }
  # }
  
  
  if (length(model@userDefinedExtraDefs) != 0) {
    for (l in model@userDefinedExtraDefs) {
      lines <- c(lines, l)
    }
  }
  
  return(lines)
}
