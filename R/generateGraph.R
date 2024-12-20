generate_NLMEResults <- function(workingDir,
                                 ColumnMapping1,
                                 inputFile,
                                 outputBinFile,
                                 outputTxtFile,
                                 thetas,
                                 thetasPositive,
                                 FixefValuesBounds,
                                 DoseTable,
                                 useDoseTable,
                                 ODE,
                                 Optimization = FALSE) {
  tryCatch({
        if (.Platform$OS.type == "windows") {
      fsep <- "\\"
    } else {
      fsep <- "/"
    }
    
    fullCols1FilePath <-
      file.path(workingDir, "cols1.txt", fsep = fsep)
    fullData1FilePath <-
      file.path(workingDir, "data1.txt", fsep = fsep)
    
    NLME7exe <- file.path(workingDir, "NLME7.exe", fsep = fsep)
    
    if (.Platform$OS.type != "windows" && Sys.getenv("BASH_VERSION") == "") {
      # Ubuntu, bash could be not there
      NLME7exeWithSetWD <-
        paste("( cd", shQuote(workingDir), ";", shQuote(NLME7exe))
    } else {
      NLME7exeWithSetWD <-
        paste0("pushd ", shQuote(workingDir), "&&", shQuote(NLME7exe))
    }
    
    if (ODE == "Stiff") {
      ODEnum <- 1
    } else if (ODE == "Auto-detect") {
      ODEnum <- 4
    } else if (ODE == "Matrix Exponent") {
      ODEnum <- 6
    } else if (ODE == "Non-stiff DOPRI5") {
      ODEnum <- 7
    } else if (ODE == "Exponent Higham") {
      ODEnum <- 8
    } else {
      # "DVERK"
      ODEnum <- 3
    }
    
    args <- paste("-m 6",
                  "-n 10",
                  "-e -1",
                  paste("-o", ODEnum),
                  "-xstderr 0",
                  "-csv")
    
    if (!Optimization) {
      args <- paste("-plotinascii",
                    shQuote(inputFile),
                    "-plotout",
                    shQuote(outputBinFile),
                    args)
    }
    
    if (Optimization &&
        !missing(thetas) &&
        all(!is.na(thetas)) &&
        !missing(thetasPositive)) {
      # lower bound should be given through cols3
      cols3.txt <- c('param("Parameter")',
                     'high("Upper")',
                     'init("Initial")',
                     'low("Lower")')
      # map thetas
      cols3.txt <- c(cols3.txt,
                     sapply(names(thetas), function(x) {
                       paste0('map("', x, '" <- "', x, '")')
                     }))
      
      Lower <- FixefValuesBounds[rownames(FixefValuesBounds) == "Lower",]
      Upper <- FixefValuesBounds[rownames(FixefValuesBounds) == "Upper",]
      ValidBounds <- (Lower < unlist(thetas)) & (unlist(thetas) < Upper)
      if (any(!ValidBounds)) {
        warning(paste("Bounds for", names(thetas)[!ValidBounds], "are not valid:", 
                      paste(Lower[!ValidBounds], Upper[!ValidBounds]), "\n"))
      }
      
      Lower <- ifelse(Lower == -Inf, NA, Lower)
      Lower <- ifelse(thetasPositive, 0, Lower)
      Upper <- ifelse(Upper == Inf, NA, Upper)
      
      data3.txt <- data.frame(
        `##Parameter` = names(thetas),
        Initial = unlist(thetas),
        Lower = unlist(Lower),
        Upper = unlist(Upper),
        check.names = FALSE
      )
      
      fullCols3FilePath <-
        file.path(workingDir, "cols3.txt", fsep = fsep)
      writeLines(cols3.txt, fullCols3FilePath)
      
      fullData3FilePath <-
        file.path(workingDir, "data3.txt", fsep = fsep)
      
      utils::write.csv(
        data3.txt,
        fullData3FilePath,
        row.names = FALSE,
        quote = FALSE,
        na = ""
      )
      
      args <- paste(args,
                    "-d3",
                    shQuote(fullCols3FilePath),
                    shQuote(fullData3FilePath))
    }
    
    ColumnMapping1 <-
      ColumnMapping1[!grepl("table(file=", ColumnMapping1, fixed = TRUE)]
    
    if (useDoseTable) {
      args <- .prepare_DoseTableArgs(
        workingDir = workingDir,
        args = args,
        ColumnMapping1 = ColumnMapping1,
        fullCols1FilePath = fullCols1FilePath,
        fullData1FilePath = fullData1FilePath,
        outputTxtFile = outputTxtFile,
        DoseTable = DoseTable,
        fsep = fsep
      )
    } else {
      writeLines(ColumnMapping1, fullCols1FilePath)
      args <- paste(
        args,
        "-d1",
        shQuote(fullCols1FilePath),
        shQuote(fullData1FilePath),
        "-out_file",
        shQuote(outputTxtFile)
      )
    }
    
    fort27TxtFile <- file.path(workingDir, "fort.27")
    outputBinFile <-
      file.path(workingDir, "output.bin", fsep = fsep)
    
    for (File in c(fort27TxtFile, outputBinFile, outputTxtFile)) {
      if (file.exists(File)) {
        file.remove(File)
      }
    }      
    
    if (.Platform$OS.type == "windows") {
      NLMEOutput <-
        system(
          paste(Sys.getenv("COMSPEC"), "/c", NLME7exeWithSetWD, args),
          wait = TRUE,
          intern = TRUE
        )
    } else {
      if (Sys.getenv("BASH_VERSION") == "") {
        ClosingParenthesis <- ")"
      } else {
        ClosingParenthesis <- ""
      }
        
      NLMEOutput <-
        system(
          paste(NLME7exeWithSetWD, args, ClosingParenthesis),
          wait = TRUE,
          ignore.stdout = TRUE,
          intern = TRUE
        )
    }
    
    if (length(NLMEOutput) > 0) {
      message("NLME output: ", paste(NLMEOutput, collapse = "\n"))
    }
    
    if (file.exists(fort27TxtFile)) {
      message(
        paste0(
          "Integration errors occured during ",
          ifelse(Optimization, "parameters fitting", "simulation"),
          ". The results should be reviewed with caution."
        )
      )
      IntegrationErrors <- TRUE
    } else {
      IntegrationErrors <- FALSE
    }
    
    if (Optimization &&
        !file.exists(outputTxtFile)) {
      WarningMessage <-
        "Failed to fit the model. Please check console output."
      if (IntegrationErrors) {
        WarningMessage <-
          paste(WarningMessage,
                "Possible reason is integration errors.",
                collapse = "\n")
      }
      
      warning(WarningMessage)
    } else if (!Optimization &&
               !file.exists(outputBinFile)) {
      warning(.gen_SimODEFailedMessage(IntegrationErrors))
    }
  }, error = function(ex) {
    warning("The following error was reported in generate_NLMEResults:",
            ex)
  })
}

.prepare_DoseTableArgs <- function(workingDir,
                                   args,
                                   ColumnMapping1,
                                   fullCols1FilePath,
                                   fullData1FilePath,
                                   outputTxtFile,
                                   DoseTable,
                                   fsep) {
  # removing dosing info from cols1
  cols1.txt <- ColumnMapping1
  DoseAddlSsStatements <- c("dose",
                            "sscol",
                            "iicol",
                            "ssoffcol",
                            "addlcol",
                            "addl",
                            "ss")
  RowsToRemoveStartWith <-
    paste0("(", DoseAddlSsStatements, "\\()", collapse = "|")
  cols1.txt <-
    cols1.txt[!grepl(RowsToRemoveStartWith, cols1.txt)]
  writeLines(cols1.txt, fullCols1FilePath)
  
  # adding dosing info in cols2
  cols2.txt <- 'time("Time")'
  if (any(DoseTable$Time < 0)) {
    message("Please check the dosing table, negative time will be treated as 0.")
    DoseTable$Time[DoseTable$Time < 0] <- 0 
  }
  
  if (any(unlist(DoseTable) < 0)) {
    message("All negative values in the dosing table will be treated as 0.")
    DoseTable <- replace(DoseTable, DoseTable < 0, 0)
  }
  
  if (is.unsorted(DoseTable$Time)) {
    message("Time in the dosing table is unsorted. It will be sorted to make NLME work.")
    DoseTable <- DoseTable[order(DoseTable$Time),]
  }
  
  DoseTableColNames <- gsub("\\n", " ", colnames(DoseTable))
  colnames(DoseTable) <- DoseTableColNames
  
  Dosepoints <-
    DoseTableColNames[-c(1, (length(DoseTableColNames) - 2):(length(DoseTableColNames)))]
  # even are dosepoints, odd are durations
  Durations <- Dosepoints[seq(2, length(Dosepoints), 2)]
  Dosepoints <- Dosepoints[seq(1, length(Dosepoints), 2)]
  
  for (DosepointIndex in seq_along(Dosepoints)) {
    if (any(DoseTable[, Durations[DosepointIndex]] > 0)) {
      # Duration is given
      DurationText <-
        paste0(', duration="', Durations[DosepointIndex], '"')
    } else {
      DurationText <- ""
    }
    
    cols2.txt <- c(
      cols2.txt,
      paste0(
        'dose(',
        Dosepoints[DosepointIndex],
        '<-"',
        Dosepoints[DosepointIndex],
        '"',
        DurationText,
        ')'
      )
    )
  }
  
  IIAdded <- FALSE
  if (any(DoseTable$SS > 0)) {
    if (any(DoseTable$SS > 0 & DoseTable$II > 0)) {
      cols2.txt <- c(cols2.txt,
                     paste0("sscol(SS)"),
                     paste0("iicol(II)"))
      IIAdded <- TRUE
      
      # impute 1 for II where it is 0 since otherwise NLME does not work
      # note that we do not change the rows where ADDL is given since ADDL > 0 and II =0 should not work
      DoseTable$II <-
        ifelse(DoseTable$II <= 0 &
                 DoseTable$ADDL <= 0, 1, DoseTable$II)
    } else {
      stop("II should be given if SS is enabled (>0).")
    }
  }
  
  if (any(DoseTable$ADDL > 0)) {
    if (sum(DoseTable$ADDL > 0) == sum(DoseTable$ADDL > 0 &
                                       DoseTable$II > 0)) {
      cols2.txt <- c(cols2.txt, paste0("addlcol(ADDL)"))
      if (!IIAdded) {
        cols2.txt <- c(cols2.txt, paste0("iicol(II)"))
      }
      
      # impute 1 for II where it is 0 since otherwise NLME does not work
      DoseTable$II <- ifelse(DoseTable$II <= 0, 1, DoseTable$II)
    } else {
      stop("II should be given if ADDL is enabled (>0).")
    }
  }
  
  fullCols2FilePath <-
    file.path(workingDir, "cols2.txt", fsep = fsep)
  writeLines(cols2.txt, fullCols2FilePath)
  
  fullData2FilePath <-
    file.path(workingDir, "data2.txt", fsep = fsep)
  DoseTableToOutput <- DoseTable
  colnames(DoseTableToOutput)[1] <-
    paste0("##", colnames(DoseTableToOutput)[1])
  utils::write.csv(DoseTableToOutput,
            fullData2FilePath,
            row.names = FALSE,
            quote = FALSE)
  
  args <- paste(
    args,
    "-d2",
    shQuote(fullCols2FilePath),
    shQuote(fullData2FilePath),
    "-d1",
    shQuote(fullCols1FilePath),
    shQuote(fullData1FilePath),
    "-out_file",
    shQuote(outputTxtFile)
  )
  
  args
}

.gen_SimODEFailedMessage <- function(IntegrationErrors) {
  WarningMessage <-
    "Simulation failed for given model/data/parameters."
  if (IntegrationErrors) {
    WarningMessage <-
      paste(WarningMessage,
            "Possible reason is integration errors.",
            collapse = "\n")
  }
  
  WarningMessage
}