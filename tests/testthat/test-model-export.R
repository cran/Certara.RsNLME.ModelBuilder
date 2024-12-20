testthat::test_that("required files can be written", {
  
  model <- Certara.RsNLME::pkmodel(
    parameterization = "Clearance",
    numCompartments = 2,
    data = Certara.RsNLME::pkData,
    ID = "Subject",
    Time = "Act_Time",
    A1 = "Amount",
    CObs = "Conc"
  )
  
  mf <- model@dataset@modelFile
  cf <- model@dataset@colDefFile
  df <- model@dataset@dataFile
  
  tmpdir <- tempdir()
  tmpmf <- file.path(tmpdir, mf)
  tmpcf <- file.path(tmpdir, cf)
  tmpdf <- file.path(tmpdir, df)
  
  writeInputData(model, df, tmpdir)
  writeColumnMapping(model,cf,tmpdir)
  writeModelStatements(model, mf, tmpdir)
  
  testthat::expect_true(file.exists(tmpmf))
  testthat::expect_true(file.exists(tmpcf))
  testthat::expect_true(file.exists(tmpdf))
  
  unlink(tmpmf)
  unlink(tmpcf)
  unlink(tmpdf)
  
})
