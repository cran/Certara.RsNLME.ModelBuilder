
renderStrRanEffSelections <- function(id, effects){
  
  if(is.null(effects) || length(effects) == 0){
    return()
  }
  
  uiRanEffSelections <- 
    tagList(
      fluidRow(
        column(width = 12,
               fluidRow(class = "multi-input-with-checkbox", style = "padding-bottom: 50px;",
                 column(width = 5,
                        selectInput(inputId = paste0("ranEffSelections", id), label = paste0("Random Effects: Covariance Matrix ", id), choices = effects, selected = NULL, multiple = TRUE)
                        ),
                 column(class = "col-checkbox",
                        width = 3, offset = 1, 
                        checkboxInput(inputId = paste0("diagonalRanEff", id), label = "Diagonal", value = TRUE),
                        ),
                 column(class = "col-checkbox",
                        width = 3, 
                        checkboxInput(inputId = paste0("freezeRanEff", id), label = "Freeze", value = FALSE),
                        )
                 )
        )
      )
    )
}


space_cols <- function(n) {
  col <- column(width = 12)
  
  lapply(1:n, function(i) col)
}


renderStrRanEffMatrix <- function(id, effects, isDiagonal){
  
  
  if(is.null(effects) || length(effects) == 0){
    return()
  }
  
  nRanEff <- length(effects)
  
  
  
  if(nRanEff == 1){
    # RanEff = 1 ----
    uiRanEff <-  tagList(
      fluidRow( 
        numericInput(inputId = paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 1000, step = 1)
      )
    )
  } else if(nRanEff == 2) {
    # RanEff = 2 ----
    if(isDiagonal == FALSE){
      uiRanEff <- tagList(
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/2,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/2,
            numericInput(paste0("strRan11_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    } else {
      uiRanEff <- tagList(
        fluidRow(
          bslib::layout_column_wrap(width = 1/2,
            numericInput(inputId = paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/2,
            !!!space_cols(1),
            numericInput(inputId = paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    }
  } else if(nRanEff == 3) {
    # RanEff = 3 ----
    if(isDiagonal == FALSE){
      uiRanEff <- tagList(
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/3,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/3,
            numericInput(paste0("strRan11_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/3,
            numericInput(paste0("strRan111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    } else {
      uiRanEff <- tagList(
            fluidRow(
              bslib::layout_column_wrap(width = 1/3,
              numericInput(inputId = paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
              )
            ),
            fluidRow(
              bslib::layout_column_wrap(width = 1/3,
                !!!space_cols(1),
                numericInput(inputId = paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)  
              )
            ),
            fluidRow(
              bslib::layout_column_wrap(width = 1/3,
                !!!space_cols(2),
                numericInput(inputId = paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
              )
            )
      )
    }
  } else if(nRanEff == 4) {
    # RanEff = 4 ----
    if(isDiagonal == FALSE){
      uiRanEff <- tagList(
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/4,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/4,
            numericInput(paste0("strRan11_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/4,
            numericInput(paste0("strRan111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/4,
            numericInput(paste0("strRan1111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan33_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    } else {
      uiRanEff <- tagList(
        fluidRow(
          bslib::layout_column_wrap(width = 1/4, 
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/4,
            !!!space_cols(1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/4,
            !!!space_cols(2),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/4,
            !!!space_cols(3),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    }
    
  } else if(nRanEff == 5) {
    # RanEff = 5 ----
    if(isDiagonal == FALSE){
      uiRanEff <- tagList(
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/5,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/5,
            numericInput(paste0("strRan11_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/5,
            numericInput(paste0("strRan111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/5,
            numericInput(paste0("strRan1111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan33_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/5,
            numericInput(paste0("strRan11111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan44_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan5_", id), label = effects[[5]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    } else {
      uiRanEff <- tagList(
        fluidRow(
          bslib::layout_column_wrap(width = 1/5,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/5,
            !!!space_cols(1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/5,
            !!!space_cols(2),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/5,
            !!!space_cols(3),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/5,
            !!!space_cols(4),
            numericInput(paste0("strRan5_", id), label = effects[[5]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    }
    
  } else if(nRanEff == 6) {
    # RanEff = 6 ----
    if(isDiagonal == FALSE){
      uiRanEff <- tagList(
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/6,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/6,
            numericInput(paste0("strRan11_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/6,
            numericInput(paste0("strRan111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/6,
            numericInput(paste0("strRan1111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan33_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/6,
            numericInput(paste0("strRan11111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan44_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan5_", id), label = effects[[5]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/6,
            numericInput(paste0("strRan111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan55_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan6_", id), label = effects[[6]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    } else {
      uiRanEff <- tagList(
        fluidRow(
          bslib::layout_column_wrap(width = 1/6,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/6,
            !!!space_cols(1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/6,
            !!!space_cols(2),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/6,
            !!!space_cols(3),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/6,
            !!!space_cols(4),
            numericInput(paste0("strRan5_", id), label = effects[[5]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/6,
            !!!space_cols(5),
            numericInput(paste0("strRan6_", id), label = effects[[6]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    }
    
  } else if(nRanEff == 7) {
    # RanEff = 7 ----
    if(isDiagonal == FALSE){
      uiRanEff <- tagList(
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/7,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/7,
            numericInput(paste0("strRan11_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/7,
            numericInput(paste0("strRan111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/7,
            numericInput(paste0("strRan1111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan33_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/7,
            numericInput(paste0("strRan11111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan44_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan5_", id), label = effects[[5]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/7,
            numericInput(paste0("strRan111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan55_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan6_", id), label = effects[[6]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/7,
            numericInput(paste0("strRan1111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan222222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan33333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan4444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan555_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan66_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan7_", id), label = effects[[7]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    } else {
      uiRanEff <- tagList(
        fluidRow(
          bslib::layout_column_wrap(width = 1/7,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/7,
            !!!space_cols(1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/7,
            !!!space_cols(2),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/7,
            !!!space_cols(3),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/7,
            !!!space_cols(4),
            numericInput(paste0("strRan5_", id), label = effects[[5]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/7,
            !!!space_cols(5),
            numericInput(paste0("strRan6_", id), label = effects[[6]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/7,
            !!!space_cols(6),
            numericInput(paste0("strRan7_", id), label = effects[[7]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    }
    
  } else if(nRanEff == 8) {
    # RanEff = 8 ----
    if(isDiagonal == FALSE){
      uiRanEff <- tagList(
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/8,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/8,
            numericInput(paste0("strRan11_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/8,
            numericInput(paste0("strRan111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/8,
            numericInput(paste0("strRan1111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan33_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/8,
            numericInput(paste0("strRan11111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan44_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan5_", id), label = effects[[5]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/8,
            numericInput(paste0("strRan111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan55_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan6_", id), label = effects[[6]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/8,
            numericInput(paste0("strRan1111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan222222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan33333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan4444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan555_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan66_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan7_", id), label = effects[[7]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/8,
            numericInput(paste0("strRan11111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2222222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan333333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan44444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan5555_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan666_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan77_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan8_", id), label = effects[[8]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    } else {
      uiRanEff <- tagList(
        fluidRow(
          bslib::layout_column_wrap(width = 1/8,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/8,
            !!!space_cols(1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/8,
            !!!space_cols(2),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/8,
            !!!space_cols(3),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/8,
            !!!space_cols(4),
            numericInput(paste0("strRan5_", id), label = effects[[5]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/8,
            !!!space_cols(5),
            numericInput(paste0("strRan6_", id), label = effects[[6]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/8,
            !!!space_cols(6),
            numericInput(paste0("strRan7_", id), label = effects[[7]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/8,
            !!!space_cols(7),
            numericInput(paste0("strRan8_", id), label = effects[[8]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    }
    
  } else if(nRanEff == 9) {
    # RanEff = 9 ----
    if(isDiagonal == FALSE){
      uiRanEff <- tagList(
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/9,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/9,
            numericInput(paste0("strRan11_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/9,
            numericInput(paste0("strRan111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/9,
            numericInput(paste0("strRan1111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan33_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/9,
            numericInput(paste0("strRan11111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan44_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan5_", id), label = effects[[5]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/9,
            numericInput(paste0("strRan111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan55_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan6_", id), label = effects[[6]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/9,
            numericInput(paste0("strRan1111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan222222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan33333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan4444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan555_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan66_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan7_", id), label = effects[[7]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/9,
            numericInput(paste0("strRan11111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2222222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan333333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan44444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan5555_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan666_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan77_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan8_", id), label = effects[[8]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/9,
            numericInput(paste0("strRan111111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22222222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3333333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan444444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan55555_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan6666_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan777_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan88_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan9_", id), label = effects[[9]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    } else {
      uiRanEff <- tagList(
        fluidRow(
          bslib::layout_column_wrap(width = 1/9,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )        
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/9,
            !!!space_cols(1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )        
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/9,
            !!!space_cols(2),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )        
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/9,
            !!!space_cols(3),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )        
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/9,
            !!!space_cols(4),
            numericInput(paste0("strRan5_", id), label = effects[[5]], value = 1, min = 0, max = 100, step = 1)
          )        
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/9,
            !!!space_cols(5),
            numericInput(paste0("strRan6_", id), label = effects[[6]], value = 1, min = 0, max = 100, step = 1)
          )        
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/9,
            !!!space_cols(6),
            numericInput(paste0("strRan7_", id), label = effects[[7]], value = 1, min = 0, max = 100, step = 1)
          )        
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/9,
            !!!space_cols(7),
            numericInput(paste0("strRan8_", id), label = effects[[8]], value = 1, min = 0, max = 100, step = 1)
          )        
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/9,
            !!!space_cols(8),
            numericInput(paste0("strRan9_", id), label = effects[[9]], value = 1, min = 0, max = 100, step = 1)
          )        
        )
      )
    }
    
  } else if(nRanEff == 10) {
    # RanEff = 10 ----
    if(isDiagonal == FALSE){
      uiRanEff <- tagList(
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/10,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/10,
            numericInput(paste0("strRan11_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/10,
            numericInput(paste0("strRan111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/10,
            numericInput(paste0("strRan1111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan33_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/10,
            numericInput(paste0("strRan11111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan44_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan5_", id), label = effects[[5]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/10,
            numericInput(paste0("strRan111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan55_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan6_", id), label = effects[[6]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/10,
            numericInput(paste0("strRan1111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan222222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan33333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan4444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan555_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan66_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan7_", id), label = effects[[7]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/10,
            numericInput(paste0("strRan11111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan2222222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan333333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan44444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan5555_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan666_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan77_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan8_", id), label = effects[[8]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/10,
            numericInput(paste0("strRan111111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan22222222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan3333333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan444444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan55555_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan6666_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan777_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan88_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan9_", id), label = effects[[9]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(class = "matrix-row",
          bslib::layout_column_wrap(width = 1/10,
            numericInput(paste0("strRan1111111111_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan222222222_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan33333333_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan4444444_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan555555_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan66666_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan7777_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan888_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan99_", id), label = NULL, value = 0, min = 0, max = 100, step = 1),
            numericInput(paste0("strRan10_", id), label = effects[[10]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    } else {
      uiRanEff <- tagList(
        fluidRow(
          bslib::layout_column_wrap(width = 1/10,
            numericInput(paste0("strRan1_", id), label = effects[[1]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/10,
            !!!space_cols(1),
            numericInput(paste0("strRan2_", id), label = effects[[2]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/10,
            !!!space_cols(2),
            numericInput(paste0("strRan3_", id), label = effects[[3]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/10,
            !!!space_cols(3),
            numericInput(paste0("strRan4_", id), label = effects[[4]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/10,
            !!!space_cols(4),
            numericInput(paste0("strRan5_", id), label = effects[[5]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/10,
            !!!space_cols(5),
            numericInput(paste0("strRan6_", id), label = effects[[6]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/10,
            !!!space_cols(6),
            numericInput(paste0("strRan7_", id), label = effects[[7]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/10,
            !!!space_cols(7),
            numericInput(paste0("strRan8_", id), label = effects[[8]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/10,
            !!!space_cols(8),
            numericInput(paste0("strRan9_", id), label = effects[[9]], value = 1, min = 0, max = 100, step = 1)
          )
        ),
        fluidRow(
          bslib::layout_column_wrap(width = 1/10,
            !!!space_cols(9),
            numericInput(paste0("strRan10_", id), label = effects[[10]], value = 1, min = 0, max = 100, step = 1)
          )
        )
      )
    }
    
  } else {
    return()
  }
  uiRanEff
}


renderMetaRanEff <- function(modelUser, effects, values, isDiagonal, isFrozen){
  
  modelUser <- tryCatch({
    metaExpr({
    ..(modelUser) %>%
      randomEffect(effect = ..(effects), value = ..(values), isDiagonal = ..(isDiagonal), isFrozen = ..(isFrozen))
  })
  },
  error = function(e) {
    if(isDiagonal == FALSE){
      checkPositiveDefinite(effects, as.numeric(values))
      return(modelUser)
    }
    return(modelUser)
  })
  
  return(modelUser)
}

checkPositiveDefinite <- function(effect, values){
  
  tryCatch({
    if(reqBlockNum(length(effect)) != length(values)){
      return()
    }
  covMatrix  <- matrix(NA, length(effect), length(effect))
  covMatrix[upper.tri(covMatrix, diag = TRUE)] <- values
  covMatrix[lower.tri(covMatrix)] <- t(covMatrix)[lower.tri(covMatrix)]
  if(.is.positive.definite(covMatrix)){
    return()
  } else {
    showNotification(
      "Block matrix defined is not positive definite. Please change input value",
      duration = 5,
      closeButton = TRUE,
      type =  "error"
    )
  }
  }, error = function(e){
    return()
  }
  )
}


.is.positive.definite <- function( x, tol=1e-8 ){
  ###
  ### this function determines if the given real symmetric matrix is positive definite
  ###
  ### parameters
  ### x = a square numeric matrix object
  ### tol = tolerance level for zero
  ###
  tryCatch({
  if ( !.is.square.matrix( x ) )
    stop( "argument x is not a square matrix" )
  if ( !.is.symmetric.matrix( x ) )
    stop( "argument x is not a symmetric matrix" )
  if ( !is.numeric( x ) )
    stop( "argument x is not a numeric matrix" )
  eigenvalues <- eigen(x, only.values = TRUE)$values
  n <- nrow( x )
  for ( i in 1: n ) {
    if ( abs( eigenvalues[i] ) < tol ) {
      eigenvalues[i] <- 0
    }
  }
  if ( any( eigenvalues <= 0 ) ) {
    return( FALSE )
  } else {
  return( TRUE )
  }
}, error = function(e){
  return(TRUE)
})
  }

.is.square.matrix <- function( x )
{
  ###
  ### determines if the given matrix is a square matrix
  ###
  ### arguments
  ### x = a matrix object
  ###
  tryCatch({
  if ( !is.matrix( x ) )
    stop( "argument x is not a matrix" )
  return( nrow(x) == ncol(x) )
  }, error = function(e){
    return()
  })
}

.is.symmetric.matrix <- function( x )
{
  ###
  ### this function determines if the matrix is symmetric
  ###
  ### argument
  ### x = a numeric matrix object
  ###
  tryCatch({
  if ( !is.matrix( x ) ) {
    stop( "argument x is not a matrix" )
  }
  if ( !is.numeric( x ) ) {
    stop( "argument x is not a numeric matrix" )
  }
  if ( !.is.square.matrix( x ) )
    stop( "argument x is not a square numeric matrix" )
  return( sum( x == t(x) ) == ( nrow(x) ^ 2 ) )
  }, error = function(e){
    return()
  })
}



reqBlockNum <- function(x){
  if(x == 1 ){
    y <- 1
  } else if(x==2){
    y <- 3
  } else if(x == 3){
    y <- 6
  } else if(x == 4){
    y <- 10
  } else if(x == 5){
    y <- 15
  } else if(x == 6){
    y <- 21
  } else if(x == 7){
    y <- 28
  } else if(x == 8){
    y <- 36
  } else if(x == 9){
    y <- 45
  } else if(x == 10){
    y <- 55
  } else if(x == 11){
    y <- 66
  } else {
    return()
  }
  return(y)
}

getDiagonalRanEff1 <-  function(input){
    ret <-c(input[["strRan1_1"]], input[["strRan2_1"]], input[["strRan3_1"]], input[["strRan4_1"]], input[["strRan5_1"]], input[["strRan6_1"]], input[["strRan7_1"]], input[["strRan8_1"]], input[["strRan9_1"]], input[["strRan10_1"]],
            input[["strRan11_1"]], input[["strRan12_1"]], input[["strRan13_1"]], input[["strRan14_1"]], input[["strRan15_1"]], input[["strRan16_1"]], input[["strRan17_1"]], input[["strRan18_1"]], input[["strRan19_1"]], input[["strRan20_1"]])

    return(ret)
}

getBlockRanEff1 <-  function(input){
  ret <- c(input[["strRan1_1"]],
    input[["strRan11_1"]], input[["strRan2_1"]],
    input[["strRan111_1"]], input[["strRan22_1"]], input[["strRan3_1"]],
    input[["strRan1111_1"]], input[["strRan222_1"]], input[["strRan33_1"]], input[["strRan4_1"]],
    input[["strRan11111_1"]], input[["strRan2222_1"]], input[["strRan333_1"]], input[["strRan44_1"]], input[["strRan5_1"]],
    input[["strRan111111_1"]], input[["strRan22222_1"]], input[["strRan3333_1"]], input[["strRan444_1"]], input[["strRan55_1"]], input[["strRan6_1"]],
    input[["strRan1111111_1"]], input[["strRan222222_1"]], input[["strRan33333_1"]], input[["strRan4444_1"]], input[["strRan555_1"]], input[["strRan66_1"]], input[["strRan7_1"]],
    input[["strRan11111111_1"]], input[["strRan2222222_1"]], input[["strRan333333_1"]], input[["strRan44444_1"]], input[["strRan5555_1"]], input[["strRan666_1"]], input[["strRan77_1"]], input[["strRan8_1"]],
    input[["strRan111111111_1"]], input[["strRan22222222_1"]], input[["strRan3333333_1"]], input[["strRan444444_1"]], input[["strRan55555_1"]], input[["strRan6666_1"]], input[["strRan777_1"]], input[["strRan88_1"]], input[["strRan9_1"]],
    input[["strRan1111111111_1"]], input[["strRan222222222_1"]], input[["strRan33333333_1"]], input[["strRan4444444_1"]], input[["strRan555555_1"]], input[["strRan66666_1"]], input[["strRan7777_1"]], input[["strRan888_1"]], input[["strRan99_1"]], input[["strRan10_1"]])
  
  return(ret)
}


getDiagonalRanEff2 <-  function(input){
  ret <-c(input[["strRan1_2"]], input[["strRan2_2"]], input[["strRan3_2"]], input[["strRan4_2"]], input[["strRan5_2"]], input[["strRan6_2"]], input[["strRan7_2"]], input[["strRan8_2"]], input[["strRan9_2"]], input[["strRan10_2"]],
          input[["strRan11_2"]], input[["strRan12_2"]], input[["strRan13_2"]], input[["strRan14_2"]], input[["strRan15_2"]], input[["strRan16_2"]], input[["strRan17_2"]], input[["strRan18_2"]], input[["strRan19_2"]], input[["strRan20_2"]])
  
  return(ret)
}

getBlockRanEff2 <-  function(input){
  ret <- c(input[["strRan1_2"]],
           input[["strRan11_2"]], input[["strRan2_2"]],
           input[["strRan111_2"]], input[["strRan22_2"]], input[["strRan3_2"]],
           input[["strRan1111_2"]], input[["strRan222_2"]], input[["strRan33_2"]], input[["strRan4_2"]],
           input[["strRan11111_2"]], input[["strRan2222_2"]], input[["strRan333_2"]], input[["strRan44_2"]], input[["strRan5_2"]],
           input[["strRan111111_2"]], input[["strRan22222_2"]], input[["strRan3333_2"]], input[["strRan444_2"]], input[["strRan55_2"]], input[["strRan6_2"]],
           input[["strRan1111111_2"]], input[["strRan222222_2"]], input[["strRan33333_2"]], input[["strRan4444_2"]], input[["strRan555_2"]], input[["strRan66_2"]], input[["strRan7_2"]],
           input[["strRan11111111_2"]], input[["strRan2222222_2"]], input[["strRan333333_2"]], input[["strRan44444_2"]], input[["strRan5555_2"]], input[["strRan666_2"]], input[["strRan77_2"]], input[["strRan8_2"]],
           input[["strRan111111111_2"]], input[["strRan22222222_2"]], input[["strRan3333333_2"]], input[["strRan444444_2"]], input[["strRan55555_2"]], input[["strRan6666_2"]], input[["strRan777_2"]], input[["strRan88_2"]], input[["strRan9_2"]],
           input[["strRan1111111111_2"]], input[["strRan222222222_2"]], input[["strRan33333333_2"]], input[["strRan4444444_2"]], input[["strRan555555_2"]], input[["strRan66666_2"]], input[["strRan7777_2"]], input[["strRan888_2"]], input[["strRan99_2"]], input[["strRan10_2"]])
  
  return(ret)
}


getDiagonalRanEff3 <-  function(input){
  ret <-c(input[["strRan1_3"]], input[["strRan2_3"]], input[["strRan3_3"]], input[["strRan4_3"]], input[["strRan5_3"]], input[["strRan6_3"]], input[["strRan7_3"]], input[["strRan8_3"]], input[["strRan9_3"]], input[["strRan10_3"]],
          input[["strRan11_3"]], input[["strRan12_3"]], input[["strRan13_3"]], input[["strRan14_3"]], input[["strRan15_3"]], input[["strRan16_3"]], input[["strRan17_3"]], input[["strRan18_3"]], input[["strRan19_3"]], input[["strRan20_3"]])
  
  return(ret)
}

getBlockRanEff3 <-  function(input){
  ret <- c(input[["strRan1_3"]],
           input[["strRan11_3"]], input[["strRan2_3"]],
           input[["strRan111_3"]], input[["strRan22_3"]], input[["strRan3_3"]],
           input[["strRan1111_3"]], input[["strRan222_3"]], input[["strRan33_3"]], input[["strRan4_3"]],
           input[["strRan11111_3"]], input[["strRan2222_3"]], input[["strRan333_3"]], input[["strRan44_3"]], input[["strRan5_3"]],
           input[["strRan111111_3"]], input[["strRan22222_3"]], input[["strRan3333_3"]], input[["strRan444_3"]], input[["strRan55_3"]], input[["strRan6_3"]],
           input[["strRan1111111_3"]], input[["strRan222222_3"]], input[["strRan33333_3"]], input[["strRan4444_3"]], input[["strRan555_3"]], input[["strRan66_3"]], input[["strRan7_3"]],
           input[["strRan11111111_3"]], input[["strRan2222222_3"]], input[["strRan333333_3"]], input[["strRan44444_3"]], input[["strRan5555_3"]], input[["strRan666_3"]], input[["strRan77_3"]], input[["strRan8_3"]],
           input[["strRan111111111_3"]], input[["strRan22222222_3"]], input[["strRan3333333_3"]], input[["strRan444444_3"]], input[["strRan55555_3"]], input[["strRan6666_3"]], input[["strRan777_3"]], input[["strRan88_3"]], input[["strRan9_3"]],
           input[["strRan1111111111_3"]], input[["strRan222222222_3"]], input[["strRan33333333_3"]], input[["strRan4444444_3"]], input[["strRan555555_3"]], input[["strRan66666_3"]], input[["strRan7777_3"]], input[["strRan888_3"]], input[["strRan99_3"]], input[["strRan10_3"]])
  
  return(ret)
}

getDiagonalRanEff4 <-  function(input){
  ret <-c(input[["strRan1_4"]], input[["strRan2_4"]], input[["strRan3_4"]], input[["strRan4_4"]], input[["strRan5_4"]], input[["strRan6_4"]], input[["strRan7_4"]], input[["strRan8_4"]], input[["strRan9_4"]], input[["strRan10_4"]],
          input[["strRan11_4"]], input[["strRan12_4"]], input[["strRan13_4"]], input[["strRan14_4"]], input[["strRan15_4"]], input[["strRan16_4"]], input[["strRan17_4"]], input[["strRan18_4"]], input[["strRan19_4"]], input[["strRan20_4"]])
  
  return(ret)
}

getBlockRanEff4 <-  function(input){
  ret <- c(input[["strRan1_4"]],
           input[["strRan11_4"]], input[["strRan2_4"]],
           input[["strRan111_4"]], input[["strRan22_4"]], input[["strRan3_4"]],
           input[["strRan1111_4"]], input[["strRan222_4"]], input[["strRan33_4"]], input[["strRan4_4"]],
           input[["strRan11111_4"]], input[["strRan2222_4"]], input[["strRan333_4"]], input[["strRan44_4"]], input[["strRan5_4"]],
           input[["strRan111111_4"]], input[["strRan22222_4"]], input[["strRan3333_4"]], input[["strRan444_4"]], input[["strRan55_4"]], input[["strRan6_4"]],
           input[["strRan1111111_4"]], input[["strRan222222_4"]], input[["strRan33333_4"]], input[["strRan4444_4"]], input[["strRan555_4"]], input[["strRan66_4"]], input[["strRan7_4"]],
           input[["strRan11111111_4"]], input[["strRan2222222_4"]], input[["strRan333333_4"]], input[["strRan44444_4"]], input[["strRan5555_4"]], input[["strRan666_4"]], input[["strRan77_4"]], input[["strRan8_4"]],
           input[["strRan111111111_4"]], input[["strRan22222222_4"]], input[["strRan3333333_4"]], input[["strRan444444_4"]], input[["strRan55555_4"]], input[["strRan6666_4"]], input[["strRan777_4"]], input[["strRan88_4"]], input[["strRan9_4"]],
           input[["strRan1111111111_4"]], input[["strRan222222222_4"]], input[["strRan33333333_4"]], input[["strRan4444444_4"]], input[["strRan555555_4"]], input[["strRan66666_4"]], input[["strRan7777_4"]], input[["strRan888_4"]], input[["strRan99_4"]], input[["strRan10_4"]])
  
  return(ret)
}

getDiagonalRanEff5 <-  function(input){
  ret <-c(input[["strRan1_5"]], input[["strRan2_5"]], input[["strRan3_5"]], input[["strRan4_5"]], input[["strRan5_5"]], input[["strRan6_5"]], input[["strRan7_5"]], input[["strRan8_5"]], input[["strRan9_5"]], input[["strRan10_5"]],
          input[["strRan11_5"]], input[["strRan12_5"]], input[["strRan13_5"]], input[["strRan14_5"]], input[["strRan15_5"]], input[["strRan16_5"]], input[["strRan17_5"]], input[["strRan18_5"]], input[["strRan19_5"]], input[["strRan20_5"]])
  
  return(ret)
}

getBlockRanEff5 <-  function(input){
  ret <- c(input[["strRan1_5"]],
           input[["strRan11_5"]], input[["strRan2_5"]],
           input[["strRan111_5"]], input[["strRan22_5"]], input[["strRan3_5"]],
           input[["strRan1111_5"]], input[["strRan222_5"]], input[["strRan33_5"]], input[["strRan4_5"]],
           input[["strRan11111_5"]], input[["strRan2222_5"]], input[["strRan333_5"]], input[["strRan44_5"]], input[["strRan5_5"]],
           input[["strRan111111_5"]], input[["strRan22222_5"]], input[["strRan3333_5"]], input[["strRan444_5"]], input[["strRan55_5"]], input[["strRan6_5"]],
           input[["strRan1111111_5"]], input[["strRan222222_5"]], input[["strRan33333_5"]], input[["strRan4444_5"]], input[["strRan555_5"]], input[["strRan66_5"]], input[["strRan7_5"]],
           input[["strRan11111111_5"]], input[["strRan2222222_5"]], input[["strRan333333_5"]], input[["strRan44444_5"]], input[["strRan5555_5"]], input[["strRan666_5"]], input[["strRan77_5"]], input[["strRan8_5"]],
           input[["strRan111111111_5"]], input[["strRan22222222_5"]], input[["strRan3333333_5"]], input[["strRan444444_5"]], input[["strRan55555_5"]], input[["strRan6666_5"]], input[["strRan777_5"]], input[["strRan88_5"]], input[["strRan9_5"]],
           input[["strRan1111111111_5"]], input[["strRan222222222_5"]], input[["strRan33333333_5"]], input[["strRan4444444_5"]], input[["strRan555555_5"]], input[["strRan66666_5"]], input[["strRan7777_5"]], input[["strRan888_5"]], input[["strRan99_5"]], input[["strRan10_5"]])
  
  return(ret)
}

getDiagonalRanEff6 <-  function(input){
  ret <-c(input[["strRan1_6"]], input[["strRan2_6"]], input[["strRan3_6"]], input[["strRan4_6"]], input[["strRan5_6"]], input[["strRan6_6"]], input[["strRan7_6"]], input[["strRan8_6"]], input[["strRan9_6"]], input[["strRan10_6"]],
          input[["strRan11_6"]], input[["strRan12_6"]], input[["strRan13_6"]], input[["strRan14_6"]], input[["strRan15_6"]], input[["strRan16_6"]], input[["strRan17_6"]], input[["strRan18_6"]], input[["strRan19_6"]], input[["strRan20_6"]])
  
  return(ret)
}

getBlockRanEff6 <-  function(input){
  ret <- c(input[["strRan1_6"]],
           input[["strRan11_6"]], input[["strRan2_6"]],
           input[["strRan111_6"]], input[["strRan22_6"]], input[["strRan3_6"]],
           input[["strRan1111_6"]], input[["strRan222_6"]], input[["strRan33_6"]], input[["strRan4_6"]],
           input[["strRan11111_6"]], input[["strRan2222_6"]], input[["strRan333_6"]], input[["strRan44_6"]], input[["strRan5_6"]],
           input[["strRan111111_6"]], input[["strRan22222_6"]], input[["strRan3333_6"]], input[["strRan444_6"]], input[["strRan55_6"]], input[["strRan6_6"]],
           input[["strRan1111111_6"]], input[["strRan222222_6"]], input[["strRan33333_6"]], input[["strRan4444_6"]], input[["strRan555_6"]], input[["strRan66_6"]], input[["strRan7_6"]],
           input[["strRan11111111_6"]], input[["strRan2222222_6"]], input[["strRan333333_6"]], input[["strRan44444_6"]], input[["strRan5555_6"]], input[["strRan666_6"]], input[["strRan77_6"]], input[["strRan8_6"]],
           input[["strRan111111111_6"]], input[["strRan22222222_6"]], input[["strRan3333333_6"]], input[["strRan444444_6"]], input[["strRan55555_6"]], input[["strRan6666_6"]], input[["strRan777_6"]], input[["strRan88_6"]], input[["strRan9_6"]],
           input[["strRan1111111111_6"]], input[["strRan222222222_6"]], input[["strRan33333333_6"]], input[["strRan4444444_6"]], input[["strRan555555_6"]], input[["strRan66666_6"]], input[["strRan7777_6"]], input[["strRan888_6"]], input[["strRan99_6"]], input[["strRan10_6"]])
  
  return(ret)
}


getDiagonalRanEff7 <-  function(input){
  ret <-c(input[["strRan1_7"]], input[["strRan2_7"]], input[["strRan3_7"]], input[["strRan4_7"]], input[["strRan5_7"]], input[["strRan6_7"]], input[["strRan7_7"]], input[["strRan8_7"]], input[["strRan9_7"]], input[["strRan10_7"]],
          input[["strRan11_7"]], input[["strRan12_7"]], input[["strRan13_7"]], input[["strRan14_7"]], input[["strRan15_7"]], input[["strRan16_7"]], input[["strRan17_7"]], input[["strRan18_7"]], input[["strRan19_7"]], input[["strRan20_7"]])
  
  return(ret)
}

getBlockRanEff7 <-  function(input){
  ret <- c(input[["strRan1_7"]],
           input[["strRan11_7"]], input[["strRan2_7"]],
           input[["strRan111_7"]], input[["strRan22_7"]], input[["strRan3_7"]],
           input[["strRan1111_7"]], input[["strRan222_7"]], input[["strRan33_7"]], input[["strRan4_7"]],
           input[["strRan11111_7"]], input[["strRan2222_7"]], input[["strRan333_7"]], input[["strRan44_7"]], input[["strRan5_7"]],
           input[["strRan111111_7"]], input[["strRan22222_7"]], input[["strRan3333_7"]], input[["strRan444_7"]], input[["strRan55_7"]], input[["strRan6_7"]],
           input[["strRan1111111_7"]], input[["strRan222222_7"]], input[["strRan33333_7"]], input[["strRan4444_7"]], input[["strRan555_7"]], input[["strRan66_7"]], input[["strRan7_7"]],
           input[["strRan11111111_7"]], input[["strRan2222222_7"]], input[["strRan333333_7"]], input[["strRan44444_7"]], input[["strRan5555_7"]], input[["strRan666_7"]], input[["strRan77_7"]], input[["strRan8_7"]],
           input[["strRan111111111_7"]], input[["strRan22222222_7"]], input[["strRan3333333_7"]], input[["strRan444444_7"]], input[["strRan55555_7"]], input[["strRan6666_7"]], input[["strRan777_7"]], input[["strRan88_7"]], input[["strRan9_7"]],
           input[["strRan1111111111_7"]], input[["strRan222222222_7"]], input[["strRan33333333_7"]], input[["strRan4444444_7"]], input[["strRan555555_7"]], input[["strRan66666_7"]], input[["strRan7777_7"]], input[["strRan888_7"]], input[["strRan99_7"]], input[["strRan10_7"]])
  
  return(ret)
}


getDiagonalRanEff8 <-  function(input){
  ret <-c(input[["strRan1_8"]], input[["strRan2_8"]], input[["strRan3_8"]], input[["strRan4_8"]], input[["strRan5_8"]], input[["strRan6_8"]], input[["strRan7_8"]], input[["strRan8_8"]], input[["strRan9_8"]], input[["strRan10_8"]],
          input[["strRan11_8"]], input[["strRan12_8"]], input[["strRan13_8"]], input[["strRan14_8"]], input[["strRan15_8"]], input[["strRan16_8"]], input[["strRan17_8"]], input[["strRan18_8"]], input[["strRan19_8"]], input[["strRan20_8"]])
  
  return(ret)
}

getBlockRanEff8 <-  function(input){
  ret <- c(input[["strRan1_8"]],
           input[["strRan11_8"]], input[["strRan2_8"]],
           input[["strRan111_8"]], input[["strRan22_8"]], input[["strRan3_8"]],
           input[["strRan1111_8"]], input[["strRan222_8"]], input[["strRan33_8"]], input[["strRan4_8"]],
           input[["strRan11111_8"]], input[["strRan2222_8"]], input[["strRan333_8"]], input[["strRan44_8"]], input[["strRan5_8"]],
           input[["strRan111111_8"]], input[["strRan22222_8"]], input[["strRan3333_8"]], input[["strRan444_8"]], input[["strRan55_8"]], input[["strRan6_8"]],
           input[["strRan1111111_8"]], input[["strRan222222_8"]], input[["strRan33333_8"]], input[["strRan4444_8"]], input[["strRan555_8"]], input[["strRan66_8"]], input[["strRan7_8"]],
           input[["strRan11111111_8"]], input[["strRan2222222_8"]], input[["strRan333333_8"]], input[["strRan44444_8"]], input[["strRan5555_8"]], input[["strRan666_8"]], input[["strRan77_8"]], input[["strRan8_8"]],
           input[["strRan111111111_8"]], input[["strRan22222222_8"]], input[["strRan3333333_8"]], input[["strRan444444_8"]], input[["strRan55555_8"]], input[["strRan6666_8"]], input[["strRan777_8"]], input[["strRan88_8"]], input[["strRan9_8"]],
           input[["strRan1111111111_8"]], input[["strRan222222222_8"]], input[["strRan33333333_8"]], input[["strRan4444444_8"]], input[["strRan555555_8"]], input[["strRan66666_8"]], input[["strRan7777_8"]], input[["strRan888_8"]], input[["strRan99_8"]], input[["strRan10_8"]])
  
  return(ret)
}

getDiagonalRanEff9 <-  function(input){
  ret <-c(input[["strRan1_9"]], input[["strRan2_9"]], input[["strRan3_9"]], input[["strRan4_9"]], input[["strRan5_9"]], input[["strRan6_9"]], input[["strRan7_9"]], input[["strRan8_9"]], input[["strRan9_9"]], input[["strRan10_9"]],
          input[["strRan11_9"]], input[["strRan12_9"]], input[["strRan13_9"]], input[["strRan14_9"]], input[["strRan15_9"]], input[["strRan16_9"]], input[["strRan17_9"]], input[["strRan18_9"]], input[["strRan19_9"]], input[["strRan20_9"]])
  
  return(ret)
}

getBlockRanEff9 <-  function(input){
  ret <- c(input[["strRan1_9"]],
           input[["strRan11_9"]], input[["strRan2_9"]],
           input[["strRan111_9"]], input[["strRan22_9"]], input[["strRan3_9"]],
           input[["strRan1111_9"]], input[["strRan222_9"]], input[["strRan33_9"]], input[["strRan4_9"]],
           input[["strRan11111_9"]], input[["strRan2222_9"]], input[["strRan333_9"]], input[["strRan44_9"]], input[["strRan5_9"]],
           input[["strRan111111_9"]], input[["strRan22222_9"]], input[["strRan3333_9"]], input[["strRan444_9"]], input[["strRan55_9"]], input[["strRan6_9"]],
           input[["strRan1111111_9"]], input[["strRan222222_9"]], input[["strRan33333_9"]], input[["strRan4444_9"]], input[["strRan555_9"]], input[["strRan66_9"]], input[["strRan7_9"]],
           input[["strRan11111111_9"]], input[["strRan2222222_9"]], input[["strRan333333_9"]], input[["strRan44444_9"]], input[["strRan5555_9"]], input[["strRan666_9"]], input[["strRan77_9"]], input[["strRan8_9"]],
           input[["strRan111111111_9"]], input[["strRan22222222_9"]], input[["strRan3333333_9"]], input[["strRan444444_9"]], input[["strRan55555_9"]], input[["strRan6666_9"]], input[["strRan777_9"]], input[["strRan88_9"]], input[["strRan9_9"]],
           input[["strRan1111111111_9"]], input[["strRan222222222_9"]], input[["strRan33333333_9"]], input[["strRan4444444_9"]], input[["strRan555555_9"]], input[["strRan66666_9"]], input[["strRan7777_9"]], input[["strRan888_9"]], input[["strRan99_9"]], input[["strRan10_9"]])
  
  return(ret)
}

getDiagonalRanEff10 <-  function(input){
  ret <-c(input[["strRan1_10"]], input[["strRan2_10"]], input[["strRan3_10"]], input[["strRan4_10"]], input[["strRan5_10"]], input[["strRan6_10"]], input[["strRan7_10"]], input[["strRan8_10"]], input[["strRan9_10"]], input[["strRan10_10"]],
          input[["strRan11_10"]], input[["strRan12_10"]], input[["strRan13_10"]], input[["strRan14_10"]], input[["strRan15_10"]], input[["strRan16_10"]], input[["strRan17_10"]], input[["strRan18_10"]], input[["strRan19_10"]], input[["strRan20_10"]])
  
  return(ret)
}

getBlockRanEff10 <-  function(input){
  ret <- c(input[["strRan1_10"]],
           input[["strRan11_10"]], input[["strRan2_10"]],
           input[["strRan111_10"]], input[["strRan22_10"]], input[["strRan3_10"]],
           input[["strRan1111_10"]], input[["strRan222_10"]], input[["strRan33_10"]], input[["strRan4_10"]],
           input[["strRan11111_10"]], input[["strRan2222_10"]], input[["strRan333_10"]], input[["strRan44_10"]], input[["strRan5_10"]],
           input[["strRan111111_10"]], input[["strRan22222_10"]], input[["strRan3333_10"]], input[["strRan444_10"]], input[["strRan55_10"]], input[["strRan6_10"]],
           input[["strRan1111111_10"]], input[["strRan222222_10"]], input[["strRan33333_10"]], input[["strRan4444_10"]], input[["strRan555_10"]], input[["strRan66_10"]], input[["strRan7_10"]],
           input[["strRan11111111_10"]], input[["strRan2222222_10"]], input[["strRan333333_10"]], input[["strRan44444_10"]], input[["strRan5555_10"]], input[["strRan666_10"]], input[["strRan77_10"]], input[["strRan8_10"]],
           input[["strRan111111111_10"]], input[["strRan22222222_10"]], input[["strRan3333333_10"]], input[["strRan444444_10"]], input[["strRan55555_10"]], input[["strRan6666_10"]], input[["strRan777_10"]], input[["strRan88_10"]], input[["strRan9_10"]],
           input[["strRan1111111111_10"]], input[["strRan222222222_10"]], input[["strRan33333333_10"]], input[["strRan4444444_10"]], input[["strRan555555_10"]], input[["strRan66666_10"]], input[["strRan7777_10"]], input[["strRan888_10"]], input[["strRan99_10"]], input[["strRan10_10"]])
  
  return(ret)
}




