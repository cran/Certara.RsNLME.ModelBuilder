COVAR_CONTINUOUS <- 1
COVAR_CATEGORY <- 2
COVAR_OCCASION <- 3
CovarNumber <- 1
CovarMean <- 2
CovarMedian <- 3
Backward <- 1
Interpolate <- 2
Forward <- 3
Continuous <- 1
SteadyStateDose <- 1
BolusDose <- 1
ColumnType <- 2
Additive <- 1
ERR_ADDITIVE <- 1
ERR_LOG_ADDITIVE <- 2
ERR_MULTIPLICATIVE <- 3
ERR_ADD_MULT <- 4
ERR_MIX_RATIO <- 5
ERR_POWER <- 6
ERR_CUSTOM <-  7
STP_PRODUCT <- 1

observationExtraNames <- function(model) {
  ExtraNames <- sapply(
    model@errorModel@effectsList,
    function(x) {
      if (x@isBQL) {
        return(paste0(x@observeName, "BQL"))
      } else {
        return("")
      }
    }
  )
  
  ExtraNames <- ExtraNames[ExtraNames != ""]
  return(ExtraNames)
}

randParameterNames <- function(model) {
  if (model@pkModelAttrs@isSequential) {
    sps <- attr(model, "structuralParams")
    names <- c()
    for (s in sps) {
      if (s@isSequential) {
        name <- attr(s, "name")
        names <- c(names, paste0("n", name))
      }
    }
  } else {
    sps <- attr(model, "structuralParams")
    names <- c()
    for (s in sps) {
      name <- attr(s, "name")
      names <- c(names, paste0("n", name))
    }
  }
  return(names)
}

lookupColname <- function(mapping, vname) {
  for (m in mapping) {
    varName <- attr(m, "variableName")
    colName <- attr(m, "columnName")
    if (vname == varName) {
      return(colName)
    }
  }
  return("")
}

covariatePartsString <- function(obj) {
  str <- ""
  type <- obj@type
  if (type == COVAR_CONTINUOUS) {
    str <- ""
  } else if (type %in% c(COVAR_CATEGORY, COVAR_OCCASION)) {
    str <- "("
    l <- obj@covarItems
    sep <- ""
    for (c in l) {
      if (c@name != "") {
        str <- paste0(str,
                      sprintf("%s\"%s\"=%d", sep, c@name, c@value))
        sep <- ", "
      }
    }
    str <- paste0(str, ")")
  }
  
  return(str)
}

.remove_comments <- function(statementsLines, type = "All", removeWhites = TRUE) {
  if (is.list(statementsLines)) {
    statementsLines <- unlist(statementsLines)
  }
  
  if (length(statementsLines) > 1) {
    statementsLines <- paste(statementsLines, collapse = "\n")
  }
  OneLine2Slash <- "(?:\\/\\/(?:\\\\\\n|[^\\n])*(?=$|\\n))"
  OneLineSharp <- "(?:#(?:\\\\\\n|[^\\n])*(?=$|\\n))"
  Asterisk <- "(?:\\/\\*[\\s\\S]*?\\*\\/)"
  
  if (type == "All") {
    pattern <- paste(OneLine2Slash,
                     OneLineSharp,
                     Asterisk,
                     sep = "|", collapse = "|"
    )
  } else if (type == "Asterisk") {
    pattern <- paste(Asterisk,
                     sep = "|", collapse = "|"
    )
  } else if (type == "Sharp") {
    pattern <- paste(OneLineSharp,
                     sep = "|", collapse = "|"
    )
  } else if (type == "OneLine") {
    pattern <- paste(OneLine2Slash,
                     OneLineSharp,
                     sep = "|", collapse = "|"
    )
  } else if (type == "C") {
    pattern <- paste(OneLine2Slash,
                     Asterisk,
                     sep = "|", collapse = "|"
    )
  } else {
    warning("type not recognized: ", type)
  }
  
  statementsLineswoComm <-
    gsub(pattern, "\n", statementsLines, perl = TRUE)
  
  if (removeWhites) {
    statementsLineswoComm <- gsub(
      "[\r\n \t]|\\(\\)",
      "",
      paste0(statementsLineswoComm, collapse = "")
    )
  }
  statementsLineswoComm
}

.restore_model <- function(metaModelFile) {
  prevModel <- NULL
  
  metaModelRaw <- readBin(metaModelFile, "raw", n = 30000)
  metamodelPart <- rawToChar(metaModelRaw)
  
  return(list(metamodelPart = metamodelPart, prevModel = prevModel))
}

.prepare_wd <- function(workingDir, showWarnings = FALSE) {
  workingDir <- trimws(workingDir)
  if (workingDir == "")
    return(normalizePath(".", winslash = "/", mustWork = FALSE))
  
  workingDir <-
    normalizePath(workingDir, winslash = "/", mustWork = FALSE)
  if (dir.exists(workingDir))
    return(workingDir)
  
  dirCreated <-
    dir.create(workingDir,
               recursive = TRUE,
               showWarnings = showWarnings)
  if (!dir.exists(workingDir)) {
    errMsg <- paste("Cannot create the directory to run:\n",
                    workingDir)
    if (.Platform$OS.type == "windows" &&
        nchar(workingDir) > 250) {
      errMsg <-
        paste(
          errMsg,
          "\nPossible reason: Current directory path is too long.",
          "Consider to shrink the path for execution."
        )
    }
    
    stop(errMsg, call. = FALSE)
  }
  
  workingDir
}

writeInputData <- function(model, datafileName, workingDir) {
  inputData <- model@inputData
  if (!model@isPopulation) {
    inputData <- cbind(zzzDummyId = 0, inputData)
  }
  
  if (missing(workingDir)) {
    workingDir <- model@modelInfo@workingDir
  }
  
  if (!dir.exists(workingDir)) {
    workingDir <- .prepare_wd(workingDir)
  }
  
  fullPath <- file.path(workingDir, datafileName)
  colnames <- colnames(inputData)
  header <- paste0("##", paste(colnames, collapse = ","))
  cat(header, file = fullPath, append = FALSE, sep = "\n")
  
  # optional units in attributes
  attrUnits <- attr(inputData, "Units")
  if (is.null(attrUnits)) attrUnits <- attr(inputData, "units")
  if (!is.null(attrUnits)) {
    if (length(attrUnits) != ncol(inputData)) {
      stop("inputData slot of the model contains units row. ",
           "The length of the units row is not the same as the data columns length: ",
           length(attrUnits), " != ", ncol(inputData))
    }
    
    units <- paste0("#@", paste(attrUnits, collapse = ","))
    cat(units, file = fullPath, append = TRUE, sep = "\n")
  }
  
  commaCheck <- sapply(inputData, function(x) {any(grepl(",", x))})
  if (any(commaCheck)) {
    stop("Cannot proceed with the data given due to commas inside the column(s): ",
         paste(names(which(commaCheck)), collapse = ", "))
  }
  write.table(inputData, fullPath,
              row.names = FALSE, col.names = FALSE, sep = ",",
              quote = FALSE, append = TRUE
  )
}

writeColumnMapping <- function(model, filename, workingDir, sortColumns = NULL) {
  if (missing(workingDir)) {
    workingDir <- model@modelInfo@workingDir
  }
  
  workingDir <- .prepare_wd(workingDir)
  
  fullPath <- file.path(workingDir, filename)
  
  colMap <- model@columnMapping@mapping
  modelType <- model@modelType@modelType
  on <- Certara.RsNLME::observationNames(model)
  # not typical observations are not included as LL count etc.
  # need to add them here
  for (mappedTerm in colMap) {
    if (.hasSlot(mappedTerm, "variableType") &&
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
  eDoseLines <- extraDoseLines(model)
  
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
    if (missing(sortColumns) |
        is.null(sortColumns) |
        !.hasSlot(sortColumns, "sortColumnList")) {
      # nothing to put into id
      lines <- c(lines, paste0("id(\"zzzDummyId\")"))
    } else if (sortColumns@numSortColumns <= 5) {
      # same as above; just cannot do all checks in one line
      lines <- c(lines, paste0("id(\"zzzDummyId\")"))
    } else if (sortColumns@numSortColumns > 5) {
      stop("Number of input data columns supplied ",
           "to 'SortColumns' cannot exceed 5 for individual models:\n",
           paste(sortColumns@sortColumnList, collapse = ", "),
           call. = FALSE
      )
    } else {
      idcolNamesquo <- paste0(shQuote(trimws(sortColumns@sortColumnList), type = "cmd"),
                              collapse = ", "
      )
      lines <- c(lines, paste0("id(", idcolNamesquo, ")"))
    }
  }
  
  if (model@isTimeBased) {
    timeCol <- lookupColname(colMap, "time")
    lines <- c(
      lines,
      paste0("time(\"", timeCol, "\")")
    )
  }
  
  for (doseName in dn) {
    colName <- lookupColname(colMap, doseName)
    if (colName %in% c("", "?")) {
      next()
    }
    
    extraDoseInfo <- ""
    
    if (!model@isTextual) {
      if (model@pkModelAttrs@infusionAllowed) {
        if (model@pkModelAttrs@isDuration) {
          infusionName <- paste0(doseName, "_Duration")
          dcolName <- lookupColname(colMap, infusionName)
          if (!dcolName %in% c("", "?")) {
            extraDoseInfo <- paste0(", duration=\"", dcolName, "\"")
          }
        } else {
          infusionName <- paste0(doseName, "_Rate")
          dcolName <- lookupColname(colMap, infusionName)
          if (!dcolName %in% c("", "?")) {
            extraDoseInfo <- paste0(", \"", dcolName, "\"")
          }
        }
      }
    } else if (.hasSlot(colMap[[doseName]], "variableType") &&
               !is.null(colMap[[doseName]]@variableType$Infusion)) {
      Infusion <- colMap[[doseName]]@variableType$Infusion
      if (!is.na(Infusion)) {
        infusionName <- paste0(doseName, "_", Infusion)
        dcolName <- lookupColname(colMap, infusionName)
        if (!dcolName %in% c("", "?")) {
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
                       doseName
    )
    
    lines <- c(
      lines,
      paste0(doseN, "(", doseName, "<-\"", colName, "\"", extraDoseInfo, ")")
    )
  }
  
  lines <- c(lines, eDoseLines)
  
  if (lookupColname(colMap, "MDV") != "?" && lookupColname(colMap, "MDV") != "") {
    lines <- c(lines, paste0("mdv(\"", lookupColname(colMap, "MDV"), "\")"))
  }
  
  covarList <- model@covariateList
  for (covariate in cn) {
    colName <- lookupColname(colMap, covariate)
    covObject <- covarList[sapply(
      covarList,
      function(x, covariate) {
        x@name == covariate
      },
      covariate
    )][[1]]
    lines <- c(
      lines,
      paste0("covr(", covariate, "<-\"", colName, "\"", covariatePartsString(covObject), ")")
    )
  }
  
  # are there some special covariates not in the list?
  for (mappedTerm in colMap) {
    if (.hasSlot(mappedTerm, "variableType") &&
        !is.null(mappedTerm@variableType$type) &&
        mappedTerm@variableType$type == "covariate" &&
        !mappedTerm@variableName %in% cn) {
      
      # we need cn to create posthoc file later
      cn <- c(cn, mappedTerm@variableName)
      colName <- mappedTerm@columnName
      if (mappedTerm@variableType$covType == COVAR_CATEGORY) {
        brackets <- "()"
      } else {
        brackets <- ""
      }
      lines <- c(
        lines,
        paste0("covr(", mappedTerm@variableName, "<-\"", colName, "\"", brackets, ")")
      )
    }
  }
  
  if (length(en) == 0) {
    for (obsName in on) {
      colName <- lookupColname(colMap, obsName)
      if (colName != "?") {
        lines <- c(
          lines,
          sprintf("obs(%s<-\"%s\")", obsName, colName)
        )
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
          if (any(grepl(greplStaticLLOQString, statementsWOComments, perl = TRUE))) {
            warning("For observation ", obsName, " a static LLOQ value requested,",
                    "\nbut ", bqlName, " mapped; static LLOQ value will be ignored.",
                    call. = FALSE
            )
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
    if (colName != "?") {
      lines <- c(lines, paste0("covr(A1Strip", "<-\"", colName, "\"", ")"))
    }
  }
  
  if (isSequential) {
    for (r in rn) {
      colName <- lookupColname(rcolMap, r)
      lines <- c(lines, paste0("covr(", r, "<-\"", colName, "\"", ")"))
    }
  }
  
  if (model@isPopulation) {
    if (length(cn) > 0) {
      indVar <- paste0(", covr(", paste(cn, collapse = ", "), "), ")
    } else {
      # no covariates
      indVar <- ", "
    }
    
    stparms <- paste(Certara.RsNLME::structuralParameterNames(model), collapse = ", ")
    
    if (length(stparms) != 0) {
      table <- paste0(
        "table(file=\"posthoc.csv\"",
        indVar,
        stparms, ", mode=keep)"
      )
      
      lines <- c(lines, table)
    }
  }
  
  
  if (length(model@userDefinedExtraDefs) != 0) {
    for (l in model@userDefinedExtraDefs) {
      lines <- c(lines, l)
    }
  }
  
  writeLines(lines, fullPath)
  
  return(lines)
}

observationExtraNames <- function(model) {
  ExtraNames <- sapply(
    model@errorModel@effectsList,
    function(x) {
      if (x@isBQL) {
        return(paste0(x@observeName, "BQL"))
      } else {
        return("")
      }
    }
  )
  
  ExtraNames <- ExtraNames[ExtraNames != ""]
  return(ExtraNames)
}

extraDoseLines <- function(model) {
  lines <- c()
  doseList <- attr(model, "extraDoses")
  colMap <- model@columnMapping@mapping
  if (length(doseList) > 0) {
    for (c in doseList) {
      name <- c@name
      doseType <- c@doseType
      if (doseType == SteadyStateDose) {
        cmd <- "ss"
        colName <- lookupColname(colMap, "SteadyState")
      } else {
        cmd <- "addl"
        colName <- lookupColname(colMap, "ADDL")
      }
      
      line <- paste0(cmd, "(\"", colName, "\", ")
      
      for (d in c@doses) {
        if (d@amount@type == ColumnType) {
          amount <- paste0("\"", d@amount@column, "\"")
        } else {
          amount <- d@amount@value
        }
        if (d@rate@type == ColumnType) {
          rate <- paste0("\"", d@rate@column, "\"")
        } else {
          rate <- d@rate@value
          if (rate == 0) {
            rate <- NULL
          }
        }
        if (d@duration@type == ColumnType) {
          duration <- paste0("\"", d@duration@column, "\"")
        } else {
          duration <- d@duration@value
          if (duration == 0) {
            duration <- NULL
          }
        }
        if (d@deltaTime@type == ColumnType) {
          deltaTime <- paste0("\"", d@deltaTime@column, "\"")
        } else {
          deltaTime <- d@deltaTime@value
        }
        if (d@isSecondDose) {
          second <- "2"
        } else {
          second <- ""
        }
        
        if (is.null(duration)) {
          dur_rate <- rate
        }
        if (is.null(rate)) {
          dur_rate <- paste0("dup / ", duration)
        }
        if (is.null(rate) && is.null(duration)) {
          dur_rate <- NULL
        }
        
        if (d@type == BolusDose) {
          if (doseType == SteadyStateDose) {
            line <- paste0(line, amount, dur_rate, " bolus", second, "(", name, ") ", deltaTime, " dt ")
          } else { # For ADDL
            line <- paste0(line, deltaTime, " dt ", amount, dur_rate, " bolus", second, "(", name, ") ")
          }
        } else {
          if (doseType == SteadyStateDose) {
            line <- paste0(line, amount, " ", dur_rate, " inf", second, "(", name, ") ", deltaTime, " dt")
          } else { # For ADDL
            line <- paste0(line, deltaTime, " dt ", amount, " ", dur_rate, " inf", second, "(", name, ") ")
          }
        }
      }
      line <- paste0(line, ")")
      lines <- c(lines, line)
    }
  }
  return(lines)
}

writeModelStatements <- function(model, filename, workingDir) {
  if (missing(workingDir)) {
    workingDir <- model@modelInfo@workingDir
  }
  
  workingDir <- .prepare_wd(workingDir)
  
  fullPath <- file.path(workingDir, filename)
  
  modelStatements <- gsub("\r\n", "\n", unlist(model@statements))
  cat(paste(unlist(modelStatements), collapse = "\n"), file = fullPath)
}

.get_fixefStrings <- function(model, initialcf) {
  if (missing(initialcf)) {
    initialcf <- Certara.RsNLME::createModelInfo(model, ForceRun = TRUE)
  }
  
  fixefLine <-
    initialcf[grepl("^\\(fixedeffects ", initialcf)]
  fixefStrings <-
    unlist(strsplit(fixefLine, split = "(\\(fixedeffects\\W*\\()|(\\)\\W*\\()|(\\)\\))"))
  fixefStrings <- fixefStrings[fixefStrings != ""]
  splittedFixefsStrings <-
    strsplit(fixefStrings, split = " ")
  splittedFixefsStrings
}

.transform_FixefsStringsToThetas <- function(splittedFixefsStrings, Part = "Value") {
  Part <- match.arg(Part, c("Value", "Eanble", "Freeze"))
  if (Part == "Value") {
    Index <- 4
  } else if (Part == "Enable") {
    Index <- 2
  } else {
    # Freeze
    Index <- 3
  }
  
  thetas <- list()
  for (fixefString in splittedFixefsStrings) {
    if (length(fixefString) < 4) {
      warning(
        "Cannot parse the following modelInfo string:\n",
        paste(fixefString)
      )
      next()
    }
    name <- fixefString[1]
    value <- fixefString[Index]
    thetas[[name]] <- value
  }
  
  thetas
}

compileModel <- function(model, host, workingDir) {
  if (missing(workingDir)) {
    workingDir <- model@modelInfo@workingDir
  }
  
  if (!host@isLocal) {
    warning("Cannot compile model on remote host given")
    return(FALSE)
  }
  
  installDir <- path.expand(host@installationDirectory)
  Sys.setenv("INSTALLDIR" = installDir)
  
  if (.Platform$OS.type == "windows") {
    fsep <- "\\"
  } else {
    fsep <- "/"
  }
  NLME7exe <- file.path(workingDir, "NLME7.exe", fsep = fsep)
  if (file.exists(NLME7exe)) {
    if (unlink(NLME7exe)) {
      warning("Cannot delete NLME7.exe from previous runs")
      return(FALSE)
    }
  }
  
  scriptargs <- paste("COMPILE test.mdl", workingDir, "MPINO NO 1")
  
  if (.Platform$OS.type == "windows") {
    script <-
      shQuote(file.path(installDir, "execNlmeCmd.ps1", fsep = fsep),
              type = "cmd")
    system2(
      "powershell",
      args = c(
        "-noninteractive",
        "-executionpolicy",
        "remotesigned",
        "-File",
        script,
        scriptargs
      ),
      wait = TRUE,
      stdout = NULL
    )
  } else {
    script <-
      shQuote(file.path(installDir, "execNLMECmd.sh", fsep = fsep))
    system(paste(script, scriptargs),
           wait = TRUE,
           ignore.stdout = TRUE)
    
  }
  
  if (!file.exists(NLME7exe)) {
    warning("NLME executable was not compiled. See the output")
    return(FALSE)
  }
  
  return(TRUE)
}

.extract_ResponsesObservations <- function(statement_string, ResponsesObservations) {
  regexprString <- paste0("(?<=", ResponsesObservations, " )[ a-zA-Z\\d_\\(\\)]+(?=\\)\\|)")
  observationsString <- regmatches(
    statement_string,
    regexpr(
      regexprString,
      statement_string,
      perl = TRUE
    )
  )
  
  observationswithBQL <-
    unlist(strsplit(observationsString, "(\\) \\()|(\\()|(\\))", perl = TRUE))
  observationswithBQL <-
    observationswithBQL[observationswithBQL != ""]
  observations <-
    unlist(lapply(observationswithBQL,
                  function(x) {
                    observation <- strsplit(x, " ")[[1]][1]
                    observation
                  }))
  
  observations
}


#' Class represents an NLME Covariate parameter
#'
#' Class represents an NLME Covariate parameter
#'
#' @param name      Name of covariate
#' @param value     Covariate value
#' @keywords internal
NlmeCovarItem <- setClass(
  "NlmeCovarItem",
  slots = c(name = "character",
            value = "numeric"),
  prototype = list(name = "",
                   value = 0)
)


setMethod("initialize", "NlmeCovarItem",
          function(.Object,
                   name = "",
                   value = 0) {
            .Object@name <- name
            .Object@value <- as.integer(value)
            .Object
          })


