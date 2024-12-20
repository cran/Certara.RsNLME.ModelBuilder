#' Returns values from initial estimates shiny App
#'
#' Use to return values from initial estimates shiny App
#'
#'
#' @param model Model object
#'
#' @return Returns a list of initial parameter estimates from the supplied model. 
#' @noRd
#' @keywords internal
getInitialEstimates <- function(model) {
  wd <- tempdir(TRUE)
  modelName <- model@modelInfo@modelName
  modelDir <- file.path(wd, modelName)
  # Read latest parameters
  lines <- readLines(file.path(modelDir, "params.txt"))
  effects <- list()
  numEffects <- as.numeric(lines[[1]])
  for (i in 1:numEffects) {
    name <- trimws(lines[[(i - 1) * 2 + 2]], "both")
    val <- as.numeric(lines[[(i - 1) * 2 + 3]])
    effects[[name]] <- val
  }
  
  effects
}

findDecimalDigits <- function(current_value) {
  if ((current_value %% 1) != 0) {
    temp <-
      unlist(strsplit(format(current_value, scientific = F), ".", fixed = TRUE))
    if (length(temp) > 1) {
      n_digit <- nchar(temp[[2]])
    } else {
      n_digit <- 0
    }
  } else {
    n_digit <- 0
  }
  n_digit
}

.get_sliderMax <- function(defStartTime, defEndTime) {
  if (defEndTime < 1) {
    maxLimit <- 1
  } else if (defStartTime == defEndTime) {
    # rare situation of identical values
    # increment the 1st register
    signifStart <- signif(defStartTime, 2)
    signifEnd <- signif(defEndTime, 2)
    defEndTime <- signifEnd + 10 ^ (trunc(log10(signifStart)))
    maxLimit <- defEndTime
  } else {
    signifEnd <- signif(defEndTime, 2)
    if (signifEnd < defEndTime) {
      while (signifEnd < defEndTime) {
        # increment by 1 the 2nd decimal register until we get the higher value
        signifEnd <- signifEnd + 10 ^ (trunc(log10(signifEnd)) - 1)
      }
    }
    maxLimit <- signifEnd
  }
  
  maxLimit
}

.step_sliders <- function(current_value, Lower, Upper) {
  step <- 1
  
  if (current_value %% 1 != 0) {
    n_digit <- findDecimalDigits(abs(current_value))
    step <- 10 ^ -n_digit
  } else {
    n_digit <- floor(log10(abs(current_value))) - 1
    step <- 10 ^ n_digit
  }
  
  i <- 1
  while (current_value - step <= Lower ||
         current_value + step >= Upper)  {
    step <- 10 ^ -(n_digit + i)
    i <- i + 1
    if (i > 10) break
  }
  
  step
}

.get_rowValuesModelInfo <-
  function(statement_string,
           lookupString,
           lookupPattern = "[a-zA-Z\\d\\._ ]+(?=\\))") {
    pattern <-
      paste0("(?<=", lookupString, " )", lookupPattern)
    rowValuesString <- regmatches(statement_string,
                                  gregexpr(pattern, statement_string,  perl = TRUE))
    if (length(rowValuesString) > 0) {
      rowValues <- unlist(strsplit(unlist(rowValuesString), " "))
    } else {
      rowValues <- character(0)
    }
    
    rowValues
  }

output_plot <- function(out, input) {
  sIndx <- input$Subject
  
  plot <-
    ggplot2::ggplot() +
    ggplot2::xlab("") +
    ggplot2::ylab("")
  
  if (!input$overlay && !input$facet)
  {
    if (input$independent_var == "time")
    {
      dfGivenDataCurSubj <- .get_dfGivenDataCurSubjTime(out, sIndx)
      dfSimDataCurSubj <- .get_dfSimDataCurSubjTime(out, sIndx)
    } else  {
      # one variable vs another
      dfGivenDataCurSubj <- .get_dfGivenDataCurSubj2Vars(out, sIndx)
      dfSimDataCurSubj <- .get_dfSimDataCurSubj2Vars(out, sIndx)
    }
    
    xlim <- c(min(dfSimDataCurSubj$x), max(dfSimDataCurSubj$x))
    ylim <-
      c(min(c(
        dfSimDataCurSubj$y, dfGivenDataCurSubj$py
      )), max(c(
        dfSimDataCurSubj$y, dfGivenDataCurSubj$py
      )))
    
    plot <-
      plot +
      ggplot2::geom_line(
        data = dfSimDataCurSubj,
        ggplot2::aes(x, y),
        colour = "blue",
        linetype = "solid",
        size = 1
      ) +
      ggplot2::geom_point(
        data = dfGivenDataCurSubj,
        ggplot2::aes(px, py),
        colour = 'red',
        size = 3
      ) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::ggtitle(paste0("ID ", input$Subject)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  } else {
    dfGivenAllSubs <- data.frame()
    dfSimDataAllSubjs <- data.frame()
    
    # all subjects plots
    for (indx in 1:out$nSubjects)
    {
      sIndx <- names(out$subjects)[[indx]]
      
      if (input$independent_var == "time") {
        dfGivenDataCurSubj <- .get_dfGivenDataCurSubjTime(out, sIndx)
        dfSimDataCurSubj <- .get_dfSimDataCurSubjTime(out, sIndx)
      } else {
        dfGivenDataCurSubj <- .get_dfGivenDataCurSubj2Vars(out, sIndx)
        dfSimDataCurSubj <- .get_dfSimDataCurSubj2Vars(out, sIndx)
      }
      
      dfGivenAllSubs <-
        rbind(dfGivenAllSubs,
              dfGivenDataCurSubj)
      
      dfSimDataAllSubjs <-
        rbind(dfSimDataAllSubjs,
              dfSimDataCurSubj)
    }
    
    xlim <-
      c(min(dfSimDataAllSubjs$x), max(dfSimDataAllSubjs$x))
    
    if (!nrow(dfGivenAllSubs)) {
      ylim <-
        c(min(dfSimDataAllSubjs$y),
          max(dfSimDataAllSubjs$y))
    } else {
      ylim <-
        c(
          min(dfSimDataAllSubjs$y, dfGivenAllSubs$py),
          max(dfSimDataAllSubjs$y, dfGivenAllSubs$py)
        )
    }
    
    plot <-
      plot +
      ggplot2::geom_line(
        data = dfSimDataAllSubjs,
        ggplot2::aes(x, y, group = subject),
        colour  =  "blue",
        linetype  =  "solid",
        size  =  1
      ) +
      ggplot2::geom_point(
        data = dfGivenAllSubs,
        ggplot2::aes(px, py, group = subject),
        colour = 'red',
        size = 3
      ) +
      ggplot2::ggtitle("All") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    
    if (input$facet) {
      plot <-
        plot +
        ggforce::facet_wrap_paginate(
          ~  subject,
          ncol = input$ncol,
          nrow = input$nrow,
          page = input$page
        )
    }
  }
  
  if (input$log) {
    xlim[1] <- 0.0001
    ylim[1] <- 0.0001
  }
  
  plot <-
    plot +
    ggplot2::xlim(xlim) +
    ggplot2::ylim(ylim)
  
  if (input$log) {
    plot <-
      plot +
      ggplot2::scale_y_log10()
  }
  
  plot + theme_certara()
}

.get_dfGivenDataCurSubjTime <- function(out, sIndx) {
  # given data for the current subject
  px <-
    as.numeric(out$subjects[[sIndx]]$observations[[1]]$obsTimes)
  py <-
    as.numeric(out$subjects[[sIndx]]$observations[[1]]$obsValues)
  if (length(!is.na(px)) != 0 &&
      length(!is.na(py)) != 0)
  {
    dfGivenDataCurSubj <-
      na.omit(data.frame(
        px = px,
        py = py,
        subject = sIndx
      ))
  } else {
    dfGivenDataCurSubj <-
      na.omit(data.frame(
        px = numeric(0),
        py = numeric(0),
        subject = character(0)
      ))
  }
  
  dfGivenDataCurSubj
}

.get_dfSimDataCurSubjTime <- function(out, sIndx) {
  # simulated data for the current subject
  x1 <- out$subjects[[sIndx]]$observations[[1]]$x
  y1 <- out$subjects[[sIndx]]$observations[[1]]$y
  
  # prepare sim data frame for one subject plots
  if (length(!is.na(x1)) != 0 && length(!is.na(y1)) != 0)
  {
    dfSimDataCurSubj <-
      na.omit(data.frame(x = x1, y = y1, subject = sIndx))
  } else {
    dfSimDataCurSubj <-
      na.omit(data.frame(
        x = x1,
        y = y1,
        subject = character(0)
      ))
  }
  
  dfSimDataCurSubj
}

.get_dfGivenDataCurSubj2Vars <- function(out, sIndx) {
  # given data for the current subject
  pxx <-
    as.numeric(out$subjects[[sIndx]]$observations[[1]]$obsTimes)
  px <-
    as.numeric(out$subjects[[sIndx]]$observations[[1]]$obsValues)
  
  pyx <-
    as.numeric(out$subjects[[sIndx]]$observations[[2]]$obsTimes)
  py <-
    as.numeric(out$subjects[[sIndx]]$observations[[2]]$obsValues)
  
  if (out$tBased == 0)
  {
    # non-time based
    dfGivenDataCurSubj <-
      na.omit(data.frame(px = pyx, py = py))
    # if (length(!is.na(px))  == length(!is.na(py)))
    # {
    #   dfGivenDataCurSubj <- na.omit(data.frame(px = px, py = pyx))
    # } else
    # {
    #   dfGivenDataCurSubj <-
    #     na.omit(data.frame(px = pyx, py = py))
    # }
  } else
  {
    if (length(px) == 1 | length(py) == 1) {
      dfGivenDataCurSubj <- na.omit(data.frame(px = px, py = py))
    } else {
      # if there is observed data, merge observation sequences
      dfGivenDataCurSubjX <- data.frame(time = pxx, px = px)
      dfGivenDataCurSubjY <-
        data.frame(time = pyx, py = py)
      dfGivenDataCurSubj <-
        na.omit(merge(dfGivenDataCurSubjX, dfGivenDataCurSubjY))
    }
  }
  
  if (nrow(dfGivenDataCurSubj) > 0) {
    dfGivenDataCurSubj <-
      cbind.data.frame(dfGivenDataCurSubj, subject = sIndx)
  } else {
    dfGivenDataCurSubj <-
      cbind.data.frame(dfGivenDataCurSubj, subject = character(0))
  }
  
  dfGivenDataCurSubj
}

.get_dfSimDataCurSubj2Vars <- function(out, sIndx) {
  # simulated data for the current subject
  x1 <- out$subjects[[sIndx]]$observations[[1]]$x
  y1 <- out$subjects[[sIndx]]$observations[[1]]$y
  x2 <- out$subjects[[sIndx]]$observations[[2]]$x
  y2 <- out$subjects[[sIndx]]$observations[[2]]$y
  
  if (out$tBased == 0)
  {
    # non-time based
    if (length(!is.na(y1))  == length(!is.na(y2)))
    {
      dfSimDataCurSubj <- na.omit(data.frame(x = y1, y = y2))
    } else
    {
      dfSimDataCurSubj <-
        na.omit(data.frame(x = x2, y = y2))
    }
  } else
  {
    # if there is observed data, merge observation sequences
    dfSimDataCurSubjX <- data.frame(time = x1, x = y1)
    dfSimDataCurSubjY <-
      data.frame(time = x2, y = y2)
    dfSimDataCurSubj <-
      na.omit(merge(dfSimDataCurSubjX, dfSimDataCurSubjY))
  }
  
  if (nrow(dfSimDataCurSubj) > 0) {
    dfSimDataCurSubj <-
      cbind.data.frame(dfSimDataCurSubj, subject = sIndx)
  } else {
    dfSimDataCurSubj <-
      cbind.data.frame(dfSimDataCurSubj, subject = character(0))
  }
  
  dfSimDataCurSubj
}

.update_ThetaNumericValue <- function(input, thetaIndex, session, FixefValuesBounds) {
  numThetaIndexName <-
    paste0("num_theta", thetaIndex)
  
  if (is.null(input[[numThetaIndexName]]) ||
      is.na(input[[numThetaIndexName]]) ||
      !is.numeric(input[[numThetaIndexName]])) {
    return(FALSE)
  }
  
  numThetaVal <- isolate({
    as.numeric(input[[numThetaIndexName]])
  })
  
  checkThetaVal <- isolate({
    as.logical(input[[paste0("check_theta", thetaIndex)]])
  })
  
  if (checkThetaVal) {
    Lower <- 0
  } else {
    Lower <- FixefValuesBounds["Lower", thetaIndex]
  }
  
  Upper <- FixefValuesBounds["Upper", thetaIndex]
  
  if (numThetaVal <= Lower) {
    if (checkThetaVal) {
      warning("Theta ",
              colnames(FixefValuesBounds)[thetaIndex],
              " should be positive if checkbox 'Positive' is set.",
              call. = FALSE)
    } else {
      warning(
        "Theta ",
        colnames(FixefValuesBounds)[thetaIndex],
        " has an embedded lower bound set as ",
        Lower,
        call. = FALSE
      )
    }
    
    if (Upper < Inf) {
      numThetaVal <- Lower + abs(Upper - Lower) * 0.1
    } else {
      numThetaVal <- Lower + abs(Lower) * 0.1 + 0.1
    }
    
    message("Theta ",
            colnames(FixefValuesBounds)[thetaIndex],
            " is set to ",
            numThetaVal)
  } else if (numThetaVal >= Upper) {
    warning(
      "Theta ",
      colnames(FixefValuesBounds)[thetaIndex],
      " has an embedded upper bound set as ",
      Upper,
      call. = FALSE
    )
    
    if (Lower > -Inf) {
      numThetaVal <- Lower + abs(Upper - Lower) * 0.9
    } else {
      # no lower bound
      numThetaVal <- Upper - abs(Upper) * 0.1 - 0.1
    }
    
    message("Theta ",
            colnames(FixefValuesBounds)[thetaIndex],
            " is set to ",
            numThetaVal)
  }
  
  step <- .step_sliders(numThetaVal, Lower, Upper)
  
  if (is.infinite(step)) {
    step <- numThetaVal / 10
  }

  updateNumericInput(
    session,
    inputId = numThetaIndexName,
    value = numThetaVal,
    step = step,
    min = Lower,
    max = Upper
  )
  
  TRUE
}

.clean_PML <-
  function(StatementsLines) {
    if (is.list(StatementsLines)) {
      StatementsLines <- unlist(StatementsLines)
    }
    
    Statements <- paste(StatementsLines, collapse = "\n")
    Statements <- gsub("\\r", "", Statements)
    OneLine2Slash <- "(?:\\/\\/(?:\\\\\\n|[^\\n])*(?=$|\\n))"
    OneLineSharp <- "(?:#(?:\\\\\\n|[^\\n])*(?=$|\\n))"
    Asterisk <- "(?:\\/\\*[\\s\\S]*?\\*\\/)"
    
    Pattern <- paste(OneLine2Slash,
                     OneLineSharp,
                     Asterisk,
                     sep = "|",
                     collapse = "|")
    
    StatementsWOComm <-
      gsub(Pattern, "\n", Statements, perl = TRUE)
    
    # we cannot handle multiple capturing groups at once
    Pattern <- "(\\((?:[^()]++|(?-1))*+\\))"
    #"(?=\\()(?:(?=.*?\\((?!.*?\\1)(.*\\)(?!.*\\2).*))(?=.*?\\)(?!.*?\\2)(.*)).)+?.*?(?=\\1)[^(]*(?=\\2$)"
    for (iChar in 1:nchar(StatementsWOComm)) {
      ParenthesisRegexpr <- regexpr(Pattern,
                                    StatementsWOComm,
                                    perl = TRUE)
      
      if (ParenthesisRegexpr[1] == -1)
        break
      FirstInParent <-
        unlist(regmatches(StatementsWOComm,
                          ParenthesisRegexpr))
      
      # the PML code does not have any `[` or `]`
      # substituting the opening `(`
      FirstInParentModified <-
        sub("\\(", "[", FirstInParent, perl = TRUE)
      # substituting the closing `)`
      FirstInParentModified <-
        sub("\\)$", "]", FirstInParentModified, perl = TRUE)
      # substituting all paragraph marks to ' '
      FirstInParentModified <-
        gsub("\\s+", " ", FirstInParentModified, perl = TRUE)
      
      regmatches(StatementsWOComm,
                 ParenthesisRegexpr) <- FirstInParentModified
    }
    
    StatementsWOComm <- gsub("\\[", "\\(", StatementsWOComm)
    StatementsWOComm <- gsub("\\]", "\\)", StatementsWOComm)
    
    for (Sign in c("+", "-", "*", "/", "(", "=")) {
      StatementsWOComm <-
        gsub(paste0("\\s*\\", Sign, "\\s*"),
             Sign,
             StatementsWOComm,
             perl = TRUE)
    }
    
    StatementsWOComm <- gsub("\\t", " ", StatementsWOComm)
    StatementsWOComm
  }


.parse_Fixefs <- function(CustomCodeToSearch, Statement = "fixef") {
  Pattern <-
    paste0("(?<=", Statement, ")(\\((?:[^()]++|(?-1))*+\\))")
  
  # get rid of enable
  CustomCodeToSearch <-
    gsub("\\(\\W*enable\\W*\\=\\W*c\\(\\W*\\d+\\W*\\)\\W*\\)", "", CustomCodeToSearch)
  
  # get rid of freeze
  CustomCodeToSearch <- gsub("\\(\\W*freeze\\W*\\)", "", CustomCodeToSearch)
  
  ParenthesisRegexpr <-
    gregexpr(Pattern, CustomCodeToSearch, perl = TRUE)
  if (ParenthesisRegexpr[[1]][1] == -1)
    return(list())
  
  Fixefs <- list()
  
  InParents <-
    unlist(regmatches(CustomCodeToSearch, ParenthesisRegexpr))
  for (InParent in InParents) {
    InParentSplitted <- strsplit(InParent, "[^a-zA-Z0-9_\\-\\.]", perl = TRUE)[[1]]
    FixefNames <-
      unique(InParentSplitted[nchar(InParentSplitted) > 0])
    # removing special words
    FixefNames <- FixefNames[!FixefNames %in% "c"]
    # removing numbers
    FixefNames <- 
      FixefNames[!grepl("^([-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][-]?[0-9]+)$", FixefNames)]
    FixefNamesPattern <-
      paste0("(", FixefNames, ")", collapse = "|")
    FixefNamesPattern <-
      paste0("(?<=\\W)(", FixefNamesPattern, ")\\W")
    FixefStatements <-
      strsplit(InParent, FixefNamesPattern, perl = TRUE)[[1]]
    # the first one is a parenthesis
    FixefStatements <- FixefStatements[FixefStatements != "("]
    # removing the last parenthesis
    FixefStatements[length(FixefStatements)] <-
      gsub(")$", "", FixefStatements[length(FixefStatements)])
    
    for (FixefNameIndex in seq_along(FixefNames)) {
      Upper <- Inf
      Lower <- -Inf
      if (grepl("c\\(.+\\)", FixefStatements[[FixefNameIndex]])) {
        # c(,,) format
        FixefValuesParsed <-
          strsplit(FixefStatements[[FixefNameIndex]], split = "(c\\(|\\,|\\))")[[1]]
        FixefValuesParsed <- trimws(FixefValuesParsed)
        if (FixefValuesParsed[2] != "") {
          Lower <- as.numeric(FixefValuesParsed[2])
        }
        
        Value <- as.numeric(FixefValuesParsed[3])
        
        if (FixefValuesParsed[4] != "") {
          Upper <- as.numeric(FixefValuesParsed[4])
        }
        
      } else {
        # value format
        Value <- gsub("(\\=|\\ )", "", FixefStatements[[FixefNameIndex]])
        Value <- as.numeric(Value)
      }
      
      Fixefs[[FixefNames[FixefNameIndex]]] <- c(Lower = Lower,
                                                Value = Value,
                                                Upper = Upper)
    }
  }
  
  Fixefs
}
