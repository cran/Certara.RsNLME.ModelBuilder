optimize_Model <- function(modelDir,
                           ColumnMapping1,
                           thetas,
                           thetasPositive,
                           FixefValuesBounds,
                           DoseTable,
                           useDoseTable,
                           ODE,
                           fsep) {
  paramsFilePath <- "" # not required
  outputBinFile <- ""  # not required
  outputTxtFile <-
    file.path(modelDir, "out.txt", fsep = fsep)
  if (file.exists(outputTxtFile))
    fs::file_delete(outputTxtFile)
  
  generate_NLMEResults(
    workingDir = modelDir,
    ColumnMapping1 = ColumnMapping1,
    inputFile = paramsFilePath,
    outputBinFile = outputBinFile,
    outputTxtFile = outputTxtFile,
    thetas = thetas,
    thetasPositive = thetasPositive,
    FixefValuesBounds = FixefValuesBounds,
    DoseTable = DoseTable,
    useDoseTable = useDoseTable,
    ODE = ODE,
    Optimization = TRUE
  )
  
  if (!file.exists(outputTxtFile)) {
    return(thetas)
  }
  
  UpdatedThetas <- .parse_OutFileStructure(outfile = outputTxtFile,
                                           ToCapture = "fixed effects")
  
  if (!is.list(UpdatedThetas) ||
      length(UpdatedThetas) < length(thetas)) {
    warning("Cannot optimize thetas, returned thetas array is not valid.")
  } else {
    NewThetas <- thetas
    UpdatedThetasNames <- names(UpdatedThetas)
    for (ThetaName in names(NewThetas)) {
      if (!ThetaName %in% UpdatedThetasNames) {
        warning("Current theta is not found in ", outputTxtFile)
      } else {
        NewThetas[[ThetaName]] <- UpdatedThetas[[ThetaName]]
      }
    }
    
    NewThetas
  }
}

# parse fixef structure of consequent rows in the out file
.parse_OutFileStructure <-
  function(outfile = "out.txt",
           ToCapture = "fixed effects") {
    if (!file.exists(outfile)) {
      warning("File ", outfile, " was not found after optimization.")
      return("")
    }
    
    outText <-  readLines(outfile, warn = FALSE)
    
    StructureStart <- grep(paste0("^", ToCapture, "$"), outText)
    
    if (length(StructureStart) == 0) {
      warning("Fixed effects section was not found in ", outfile)
      return("")
    }
    
    if (length(StructureStart) > 1) {
      warning(
        paste0(
          "More than one ",
          ToCapture,
          " record was found in \n",
          outfile,
          "\nOnly the first occurence will be used."
        )
      )
      StructureStart <- StructureStart[1]
    }
    
    # Structure is starting at the next row
    StructureStart <- StructureStart + 1
    # from StructureStart to the end of the file
    outTextStart <- outText[StructureStart:length(outText)]
    # entities are separated with empty rows
    StructureLength <- match("", outTextStart) - 1
    if (is.na(StructureLength) || StructureLength < 1) {
      warning(
        "Current ouptut file was not parsed correctly \n",
        outfile,
        "\nUnable to read Structures."
      )
      return("")
    }
    
    StructureList <- strsplit(outTextStart[1:StructureLength], " ")
    suppressWarnings(UpdatedThetas <- lapply(StructureList,
                                             function(Row)
                                               as.numeric(Row[3])))
    
    StructuresNames <- sapply(StructureList,
                              function(Row)
                                Row[5])
    
    names(UpdatedThetas) <- StructuresNames
    
    UpdatedThetas
  }