get_NLMEIEstimatesData <-
  function(modelDir,
           ColumnMapping1,
           thetas,
           plotVariables,
           sweepStart,
           sweepLength,
           numSweepSteps,
           DoseTable,
           useDoseTable,
           ODE,
           fsep) {
    paramsFilePath <-
      file.path(modelDir, "params.txt", fsep = fsep)
    outputBinFile <-
      file.path(modelDir, "output.bin", fsep = fsep)
    outputTxtFile <-
      file.path(modelDir, "out.txt", fsep = fsep)
    
    generateInitialEstimatesInputAscii(paramsFilePath,
                                       thetas,
                                       plotVariables,
                                       sweepStart,
                                       sweepLength,
                                       numSweepSteps)
    
    generate_NLMEResults(workingDir = modelDir,
                         ColumnMapping1 = ColumnMapping1,
                         inputFile = paramsFilePath,
                         outputBinFile = outputBinFile,
                         outputTxtFile = outputTxtFile,
                         # thetas = NA, not used
                         # thetasPositive = NA, not used
                         # FixefValuesBounds = NA, not used
                         DoseTable = DoseTable,
                         useDoseTable = useDoseTable,
                         ODE = ODE,
                         Optimization = FALSE)
    
    if (!file.exists(outputBinFile)) {
      return(NA)
    }
    
    out <- FALSE

    tryCatch({
      out <- readModelData(outputBinFile)
    },
    error = function(cond) {
      # output.bin exists but does not have info => failed
      warning(.gen_SimODEFailedMessage(file.exists(file.path(dirname(outputBinFile), "fort.27"))))
    })
    
    out
  }
