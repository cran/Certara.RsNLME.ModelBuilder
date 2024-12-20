readModelData <- function(fileName) {
  out <- list()
  inputFile <- file(fileName, open = "rb")
  on.exit(close(inputFile))
  sig <- readBin(inputFile, integer(), n = 1)
  tBased <- readBin(inputFile, "integer", n = 1)
  nThetas <- readBin(inputFile, "integer", n = 1)
  out$tBased <- tBased
  out$nThetas <- nThetas
  out$thetas <- list()
  for (ThetaIndex in 1:nThetas) {
    ThetaNameLength <- readBin(inputFile, "raw", size = 1)
    ThetaName <-
      readChar(inputFile, nchars = as.integer(ThetaNameLength))
    ThetaValue <- readBin(inputFile, "double")
    out$thetas[[ThetaIndex]] <-
      c(name = ThetaName, value = ThetaValue)
  }

  nVars <- readBin(inputFile, "integer", n = 1)
  out$nVars <- nVars
  out$vars <- list()
  for (n in 1:nVars) {
    varnameLength <- readBin(inputFile, "raw", size = 1)
    var <- readChar(inputFile, nchars = as.integer(varnameLength))
    flag <- readBin(inputFile, "integer", n = 1)
    out$vars <- c(out$vars, list(name = var, flag = flag))
  }
  nSubjects <- readBin(inputFile, "integer", n = 1)
  out$nSubjects <- nSubjects

  nVarsReturned <- readBin(inputFile, "integer", n = 1)
  out$nVarsReturned <- nVarsReturned
  out$varsReturned <- list()
  out$subjects <- list()
  if (nVarsReturned > 0) {
    for (n in 1:nVarsReturned) {
      l <- readBin(inputFile, "raw", size = 1)
      varName <- readChar(inputFile, nchars = as.integer(l))
      isAvail <- readBin(inputFile, "integer", n = 1)
      sweep <- readBin(inputFile, "integer", n = 1)
      out$varsReturned[[varName]] <-
          list(
            isAvail = isAvail,
            sweep = sweep
          )
    }
  }


  for (SubjIndex in 1:nSubjects) {
    subject <- list()

    subject$id <- ""
    for (nID in 1:5) {
      idLength <- readBin(inputFile, "raw", size = 1)
      if (idLength == 0) next

      id <- readChar(inputFile, nchars = as.integer(idLength))
      if (nchar(subject$id) > 0) {
        subject$id <- paste(subject$id, id, sep = ", ")
      } else {
        subject$id <- id
      }
    }

    subject$observations <- list()
    if (nVarsReturned > 0) {
      for (ObsIndex in 1:out$nVarsReturned) {
        obsValues <- .readBin_obsValues(inputFile)

        subject$observations[[ObsIndex]] <- obsValues
      }
    }

    end <- readBin(inputFile, "integer", n = 1)
    stopifnot(end == 9999)

    out$subjects[[subject$id]] <- subject
  }

  out
}

.readBin_obsValues <- function(inputFile) {
  obsValues <- list()

  nObs <- readBin(inputFile, "integer", n = 1)
  obsValues$nObs <- nObs
  obsValues$obsTimes <- list()
  obsValues$obsValues <- list()

  if (nObs > 0) {
    TimesValues <- readBin(inputFile, "double", n = nObs*2)
    obsValues$obsTimes <- TimesValues[2*(1:nObs) - 1]
    obsValues$obsValues <- TimesValues[2*(1:nObs)]
  }

  nVal <- readBin(inputFile, "integer", n = 1)
  obsValues$nVal <- nVal

  if (nVal > 0) {
    num <- as.integer(2 * nVal)
    vals <- readBin(inputFile, "double", n = num)
    obsValues$x <- vals[2*(1:num) - 1]
    obsValues$y <- vals[2*(1:num)]
  } else {
    vals <- c()
    obsValues$x <- numeric(0)
    obsValues$y <- numeric(0)
  }

  obsValues
}
