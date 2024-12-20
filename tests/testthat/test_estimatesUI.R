test_that("internal function used by EstimatesUI ", {
  testthat::skip_if(Sys.getenv("INSTALLDIR") == "",
                    message = "cannot start the test, INSTALLDIR variable is not specified.")
  
  workingDir <- file.path(tempdir(TRUE), "EstimatesUI")
  dir.create(workingDir, recursive = TRUE)
  
  model <- Certara.RsNLME::pkindirectmodel(
    parameterization = "Micro",
    data = Certara.RsNLME::pkpdData,
    ID = "ID",
    Time = "Time",
    A1 = "Dose",
    CObs = "CObs",
    EObs = "EObs",
    workingDir = workingDir
  )
  
  dataset <- model@dataset
  mf <- dataset@modelFile
  cf <- dataset@colDefFile
  df <- dataset@dataFile
  
  Certara.RsNLME.ModelBuilder:::writeInputData(model, df, workingDir)
  ColumnMapping1 <-
    Certara.RsNLME.ModelBuilder:::writeColumnMapping(model = model,
                                                     filename = cf,
                                                     workingDir = workingDir)
  Certara.RsNLME.ModelBuilder:::writeModelStatements(model, mf, workingDir)
  
  host <- Certara.RsNLME::NlmeParallelHost(
    sharedDirectory = workingDir,
    installationDirectory = Sys.getenv("INSTALLDIR"),
    parallelMethod = NlmeParallelMethod("None"),
    hostName = "Local",
    numCores = 1
  )
  
  thetas <- Certara.RsNLME::getThetas(model)
  plotVariables <- character(0)
  sweepStart <- 0
  sweepLength <- 10
  numSweepSteps <- 500
  fsep <- ifelse(.Platform$OS.type == "unix", "/", "\\")
  Certara.RsNLME.ModelBuilder:::compileModel(model, host, workingDir)
  
  out <-
    Certara.RsNLME.ModelBuilder:::get_NLMEIEstimatesData(
      modelDir = workingDir,
      ColumnMapping1 = ColumnMapping1, 
        thetas = thetas,
      plotVariables = plotVariables,
      sweepStart = sweepStart,
      sweepLength = sweepLength,
      numSweepSteps = numSweepSteps,
      DoseTable = data.frame(),
      useDoseTable = FALSE,
      ODE = "DVERK",
      fsep = fsep
    )
  
  testthat::local_edition(3)
  testthat::expect_snapshot_value(out,
                                  style = "json2")
})
