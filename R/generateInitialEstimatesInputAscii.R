generateInitialEstimatesInputAscii <-
  function(outFile,
           thetas,
           variables,
           sweepStart,
           sweepLength,
           numSweepSteps) {
    cat(length(thetas),
        file = outFile,
        append = FALSE,
        sep = "\n")

    for (ThetaName in names(thetas)) {
      cat(ThetaName,
          file = outFile,
          append = TRUE,
          sep = "\n")
      cat(thetas[[ThetaName]],
          file = outFile,
          append = TRUE,
          sep = "\n")
    }
    cat(length(variables),
        file = outFile,
        append = TRUE,
        sep = "\n")

    for (var in variables) {
      cat(var,
          file = outFile,
          append = TRUE,
          sep = "\n")
    }

    num <- numSweepSteps + 1
    cat(num,
        file = outFile,
        append = TRUE,
        sep = "\n")

    sweepStep <- sweepLength / numSweepSteps

    for (inx in 0:numSweepSteps) {
      cat(
        sweepStart + inx * sweepStep,
        file = outFile,
        append = TRUE,
        sep = "\n"
      )
    }

    cat(999999L,
        file = outFile,
        append = TRUE,
        sep = "\n")
  }
