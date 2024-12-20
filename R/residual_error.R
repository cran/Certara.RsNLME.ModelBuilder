

renderResidEffect <- function(id, namesResidEffect){


  if(is.null(namesResidEffect)){
    return()
  }
                                                                                                                                                                                      #namesResidEffect <-  residualEffectNames(model)
  if(length(namesResidEffect) == 0){
    return()
  }

  nResEff <- length(namesResidEffect)
  
  if(nResEff == 1){
    # ResEff = 1 ----
    uiResEff <-  tagList(
      ## Effect 1 ----
      ### Row 1 ----
      fluidRow(class = "multi-input-with-checkbox",
        column(
          width = 2, 
          textInput(inputId = paste0("errorPredName1_", id), label = paste0(namesResidEffect), value = paste0(namesResidEffect, "Obs")) %>% shinyjs::disabled()
        ),
        column(
          width = 3,
          selectInput(inputId = paste0("errorType1_", id), label = "Type", choices = c("Additive", "Multiplicative", "AdditiveMultiplicative", "MixRatio", "Power", "LogAdditive"), selected = "Multiplicative")
        ),
        column(class = "col-checkbox",
          width = 2,
          checkboxInput(inputId = paste0("errorBQL1_", id), label = "BQL", value = FALSE)
        ),
        column(class = "col-checkbox",
          width = 3,
          conditionalPanel(paste0("input.errorBQL1_", id," == true"),
                           checkboxInput(inputId = paste0("errorStaticLLOQ1_", id), label = "Static LLOQ", value = FALSE)
          )
        ),
        column(
          style = "align-self: flex-end;",
          width = 2, 
          conditionalPanel(condition = paste0("input.errorStaticLLOQ1_", id," == true && ", "input.errorBQL1_", id, " == true"),
                           numericInput(inputId = paste0("errorStaticVal1_", id), label = NULL, min = 0.001, max = 1000, step = 0.1, value = .1)
          )
        )
      ),
      ### Row 2 ----
      fluidRow(class = "multi-input-with-checkbox",
        column(style = "align-self: flex-end !important;", 
          width = 2,
          textInput(inputId = paste0("errorEpsName1_", id), label = NULL, value = paste0(namesResidEffect, "Eps")) %>% shinyjs::disabled()
        ),
        column(style = "align-self: flex-end;",
          width = 3,
          numericInput(inputId = paste0("errorStDev1_", id), label = "StDev", min = 0.001, max = 1000, step = 0.1, value = .1)
        ),
        column(class = "col-checkbox",
          width = 2,
          checkboxInput(inputId = paste0("errorFreeze1_", id), label = "Freeze", value = FALSE)
        )
      ),
      ### Power (Conditional) ----
      fluidRow(
        column(
          width = 3, offset = 2,
          conditionalPanel(condition = paste0("input.errorType1_", id, " == 'Power'"),
                           column(width = 12,
                                  numericInput(inputId = paste0("errorPower1_", id), label = "Exponent", min = 1, max = 1000, step = 0.1, value = 2)
                           )
          )
        )
      )
    )
  } else if (nResEff == 2){
    # ResEff = 2 ----
    uiResEff <-  tagList(
      ## Effect 1 ----
      ### Row 1 ----
      fluidRow(class = "multi-input-with-checkbox",
        column(
          width = 2,
          textInput(inputId = paste0("errorPredName1_", id), label = paste0(namesResidEffect[[1]]), value = paste0(namesResidEffect[[1]], "Obs")) %>% shinyjs::disabled()
        ),
        column(
          width = 3,
          selectInput(inputId = paste0("errorType1_", id), label = "Type", choices = c("Additive", "Multiplicative", "AdditiveMultiplicative", "MixRatio", "Power", "LogAdditive"), selected = "Multiplicative")
        ),
        column(class = "col-checkbox",
          width = 2,
          checkboxInput(inputId = paste0("errorBQL1_", id), label = "BQL", value = FALSE)
        ),
        column(class = "col-checkbox",
          width = 3,
          conditionalPanel(paste0("input.errorBQL1_", id," == true"),
                           checkboxInput(inputId = paste0("errorStaticLLOQ1_", id), label = "Static LLOQ", value = FALSE)
          )
        ),
        column(
          style = "align-self: flex-end;",
          width = 2,
          conditionalPanel(condition = paste0("input.errorStaticLLOQ1_", id," == true && ", "input.errorBQL1_", id, " == true"),
                           numericInput(inputId = paste0("errorStaticVal1_", id), label = NULL, min = 0.001, max = 1000, step = 0.1, value = .1)
          )
        )
      ),
      ### Row 2 ----
      fluidRow(class = "multi-input-with-checkbox",
        column(style = "align-self: flex-end !important;",
          width = 2,
          textInput(inputId = paste0("errorEpsName1_", id), label = NULL, value = paste0(namesResidEffect[[1]], "Eps")) %>% shinyjs::disabled()
        ),
        column(
          style = "align-self: flex-end;",
          width = 3,
          numericInput(inputId = paste0("errorStDev1_", id), label = "StDev", min = 0.001, max = 1000, step = 0.1, value = .1)
        ),
        column(class = "col-checkbox",
          width = 2,
          checkboxInput(inputId = paste0("errorFreeze1_", id), label = "Freeze", value = FALSE)
        )
      ),
      ### Power (Conditional) ----
      fluidRow(
        column(
          width = 3, offset = 2,
          conditionalPanel(condition = paste0("input.errorType1_", id, " == 'Power'"),
                           column(width = 12,
                                  numericInput(inputId = paste0("errorPower1_", id), label = "Exponent", min = 1, max = 1000, step = 0.1, value = 2)
                           )
          )
        )
      ),
      ## Effect 2 ----
      ### Row 1 ----
      fluidRow(class = "multi-input-with-checkbox",
        column(
          width = 2,
          textInput(inputId = paste0("errorPredName2_", id), label = paste0(namesResidEffect[[2]]), value = paste0(namesResidEffect[[2]], "Obs")) %>% shinyjs::disabled()
        ),
        column(
          width = 3,
          selectInput(inputId = paste0("errorType2_", id), label = "Type", choices = c("Additive", "Multiplicative", "AdditiveMultiplicative", "MixRatio", "Power", "LogAdditive"), selected = "Multiplicative")
        ),
        column(class = "col-checkbox",
          width = 2,
          checkboxInput(inputId = paste0("errorBQL2_", id), label = "BQL", value = FALSE)
        ),
        column(class = "col-checkbox",
          width = 3,
          conditionalPanel(paste0("input.errorBQL2_", id," == true"),
                           checkboxInput(inputId = paste0("errorStaticLLOQ2_", id), label = "Static LLOQ", value = FALSE)
          )
        ),
        column(
          style = "align-self: flex-end;",
          width = 2,
          conditionalPanel(condition = paste0("input.errorStaticLLOQ2_", id," == true && ", "input.errorBQL2_", id, " == true"),
                           numericInput(inputId = paste0("errorStaticVal2_", id), label = NULL, min = 0.001, max = 1000, step = 0.1, value = .1)
          )
        )
      ),
      ### Row 2 ----
      fluidRow(class = "multi-input-with-checkbox",
        column(style = "align-self: flex-end !important;",
          width = 2,
          textInput(inputId = paste0("errorEpsName2_", id), label = NULL, value = paste0(namesResidEffect[[2]], "Eps")) %>% shinyjs::disabled()
        ),
        column(
          style = "align-self: flex-end;",
          width = 3,
          numericInput(inputId = paste0("errorStDev2_", id), label = "StDev", min = 0.001, max = 1000, step = 0.1, value = .1)
        ),
        column(class = "col-checkbox",
          width = 2,
          checkboxInput(inputId = paste0("errorFreeze2_", id), label = "Freeze", value = FALSE)
        )
      ),
      ### Power (Conditional) ----
      fluidRow(
        column(
          width = 3, offset = 2,
          conditionalPanel(condition = paste0("input.errorType2_", id, " == 'Power'"),
                           column(width = 12, 
                                  numericInput(inputId = paste0("errorPower2_", id), label = "Exponent", min = 1, max = 1000, step = 0.1, value = 2)
                           )
          )
        )
      )
    )
  } else if(nResEff == 3){
    # ResEff = 3 ----
    uiResEff <-  tagList(
      ## Effect 1 ----
      ### Row 1 ----
      fluidRow(class = "multi-input-with-checkbox",
        column(
          width = 2,
          textInput(inputId = paste0("errorPredName1_", id), label = paste0(namesResidEffect[[1]]), value = paste0(namesResidEffect[[1]], "Obs")) %>% shinyjs::disabled()
        ),
        column(
          width = 3,
          selectInput(inputId = paste0("errorType1_", id), label = "Type", choices = c("Additive", "Multiplicative", "AdditiveMultiplicative", "MixRatio", "Power", "LogAdditive"), selected = "Multiplicative")
        ),
        column(class = "col-checkbox",
          width = 2,
          checkboxInput(inputId = paste0("errorBQL1_", id), label = "BQL", value = FALSE)
        ),
        column(class = "col-checkbox",
          width = 3,
          conditionalPanel(paste0("input.errorBQL1_", id," == true"),
                           checkboxInput(inputId = paste0("errorStaticLLOQ1_", id), label = "Static LLOQ", value = FALSE)
          )
        ),
        column(
          style = "align-self: flex-end;",
          width = 2,
          conditionalPanel(condition = paste0("input.errorStaticLLOQ1_", id," == true && ", "input.errorBQL1_", id, " == true"),
                           numericInput(inputId = paste0("errorStaticVal1_", id), label = NULL, min = 0.001, max = 1000, step = 0.1, value = .1)
          )
        )
      ),
      ### Row 2 ----
      fluidRow(class = "multi-input-with-checkbox",
        column(style = "align-self: flex-end !important;",
          width = 2,
          textInput(inputId = paste0("errorEpsName1_", id), label = NULL, value = paste0(namesResidEffect[[1]], "Eps")) %>% shinyjs::disabled()
        ),
        column(
          style = "align-self: flex-end;",
          width = 3,
          numericInput(inputId = paste0("errorStDev1_", id), label = "StDev", min = 0.001, max = 1000, step = 0.1, value = .1)
        ),
        column(class = "col-checkbox",
          width = 2,
          checkboxInput(inputId = paste0("errorFreeze1_", id), label = "Freeze", value = FALSE)
        )
      ),
      ### Power (Conditional) ----
      fluidRow(
        column(
          width = 3, offset = 2,
          conditionalPanel(condition = paste0("input.errorType1_", id, " == 'Power'"),
                           column(width = 12, 
                                  numericInput(inputId = paste0("errorPower1_", id), label = "Exponent", min = 1, max = 1000, step = 0.1, value = 2)
                           )
          )
        )
      ),
      ## Effect 2 ----
      ### Row 1 ----
      fluidRow(class = "multi-input-with-checkbox",
        column(
          width = 2,
          textInput(inputId = paste0("errorPredName2_", id), label = paste0(namesResidEffect[[2]]), value = paste0(namesResidEffect[[2]], "Obs")) %>% shinyjs::disabled()
        ),
        column(
          width = 3,
          selectInput(inputId = paste0("errorType2_", id), label = "Type", choices = c("Additive", "Multiplicative", "AdditiveMultiplicative", "MixRatio", "Power", "LogAdditive"), selected = "Multiplicative")
        ),
        column(class = "col-checkbox",
          width = 2,
          checkboxInput(inputId = paste0("errorBQL2_", id), label = "BQL", value = FALSE)
        ),
        column(class = "col-checkbox",
          width = 3,
          conditionalPanel(paste0("input.errorBQL2_", id," == true"),
                           checkboxInput(inputId = paste0("errorStaticLLOQ2_", id), label = "Static LLOQ", value = FALSE)
          )
        ),
        column(
          style = "align-self: flex-end;",
          width = 2,
          conditionalPanel(condition = paste0("input.errorStaticLLOQ2_", id," == true && ", "input.errorBQL2_", id, " == true"),
                           numericInput(inputId = paste0("errorStaticVal2_", id), label = NULL, min = 0.001, max = 1000, step = 0.1, value = .1)
          )
        )
      ),
      ### Row 2 ----
      fluidRow(class = "multi-input-with-checkbox",
        column(style = "align-self: flex-end !important;",
          width = 2,
          textInput(inputId = paste0("errorEpsName2_", id), label = NULL, value = paste0(namesResidEffect[[2]], "Eps")) %>% shinyjs::disabled()
        ),
        column(
          style = "align-self: flex-end;",
          width = 3,
          numericInput(inputId = paste0("errorStDev2_", id), label = "StDev", min = 0.001, max = 1000, step = 0.1, value = .1)
        ),
        column(class = "col-checkbox",
          width = 2,
          checkboxInput(inputId = paste0("errorFreeze2_", id), label = "Freeze", value = FALSE)
        )
      ),
      ### Power (Conditional) ----
      fluidRow(
        column(
          width = 3, offset = 2,
          conditionalPanel(condition = paste0("input.errorType2_", id, " == 'Power'"),
                           column(width = 12, 
                                  numericInput(inputId = paste0("errorPower2_", id), label = "Exponent", min = 1, max = 1000, step = 0.1, value = 2)
                           )
          )
        )
      ),
      ## Effect 3 ----
      ### Row 1 ----
      fluidRow(class = "multi-input-with-checkbox",
        column(
          width = 2,
          textInput(inputId = paste0("errorPredName3_", id), label = paste0(namesResidEffect[[3]]), value = paste0(namesResidEffect[[3]], "Obs")) %>% shinyjs::disabled()
        ),
        column(
          width = 3,
          selectInput(inputId = paste0("errorType3_", id), label = "Type", choices = c("Additive", "Multiplicative", "AdditiveMultiplicative", "MixRatio", "Power", "LogAdditive"), selected = "Multiplicative")
        ),
        column(class = "col-checkbox",
          width = 2,
          checkboxInput(inputId = paste0("errorBQL3_", id), label = "BQL", value = FALSE)
        ),
        column(class = "col-checkbox",
          width = 3,
          conditionalPanel(paste0("input.errorBQL3_", id," == true"),
                           checkboxInput(inputId = paste0("errorStaticLLOQ3_", id), label = "Static LLOQ", value = FALSE)
          )
        ),
        column(
          style = "align-self: flex-end;",
          width = 2,
          conditionalPanel(condition = paste0("input.errorStaticLLOQ3_", id," == true && ", "input.errorBQL3_", id, " == true"),
                           numericInput(inputId = paste0("errorStaticVal3_", id), label = NULL, min = 0.001, max = 1000, step = 0.1, value = .1)
          )
        )
      ),
      ### Row 2 ----
      fluidRow(class = "multi-input-with-checkbox",
        column(style = "align-self: flex-end !important;",
          width = 2,
          textInput(inputId = paste0("errorEpsName3_", id), label = NULL, value = paste0(namesResidEffect[[3]], "Eps")) %>% shinyjs::disabled()
        ),
        column(
          style = "align-self: flex-end;",
          width = 3,
          numericInput(inputId = paste0("errorStDev3_", id), label = "StDev", min = 0.001, max = 1000, step = 0.1, value = .1)
        ),
        column(class = "col-checkbox",
          width = 2,
          checkboxInput(inputId = paste0("errorFreeze3_", id), label = "Freeze", value = FALSE)
        )
      ),
      ### Power (Conditional) ----
      fluidRow(
        column(
          width = 3, offset = 2,
          conditionalPanel(condition = paste0("input.errorType3_", id, " == 'Power'"),
                           column(width = 12, 
                                  numericInput(inputId = paste0("errorPower3_", id), label = "Exponent", min = 1, max = 1000, step = 0.1, value = 2)
                           )
          )
        )
      )
    )
  } else {
    return()
  }

  uiResEff
}

