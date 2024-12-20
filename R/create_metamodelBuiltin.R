#' Run \code{modelBuilderUI()} and create resulting metamodel
#' 
#' Used by Pirana to run \code{modelBuilderUI()}, saving the
#' resulting metamodel e.g., .mmmdl file given model building operations performed in GUI.
#'
#' @param metamodelFile File where the metamodel should be created.
#' @param datafile File with input data.
#' @param author Optional character string to specify the author in the metamodel.
#'
#' @return NLME PML model S4 class instance
#' 
#' @examples
#' if (interactive()) {
#' tmp_data <- tempfile(fileext = ".csv")
#' write.csv(Certara.RsNLME::pkData, tmp_data, row.names = FALSE)
#' 
#' create_metamodelBuiltin(
#'   "run1.mmdl",
#'   tmp_data
#'   )
#' }
#' 
#' @export
#'
create_metamodelBuiltin <- function(metamodelFile, datafile, author = "") {
  # to normalizePath we should have one type of slashes
  metamodelFile <- gsub("\\", "/", metamodelFile, fixed = TRUE)
  if(file.exists(metamodelFile)){
    warning("The specified file\n", metamodelFile, 
            "\nwas found and will be substituted")
    if(!file.remove(metamodelFile)) {
      stop("Cannot substitute the file ", metamodelFile, " specified as metamodel output.")
    }
  }
  
  modelName <- tools::file_path_sans_ext(basename(metamodelFile))
  
  if(!file.exists(datafile)) {
    stop("Cannot find the file specified for loading into metamodel:\n",
         datafile)
  }
  
  if(requireNamespace("data.table", quietly = TRUE)) {
    inputdata <- data.table::fread(file = datafile, data.table = FALSE)
  } else {
    # figure out the end of comments line
    commentsEnded <- 0L
    while (grepl("^#",
                 scan(datafile,
                      what="character",
                      sep='\n',
                      quiet = T,
                      skip = commentsEnded,
                      nlines = 1))) {
      commentsEnded <- commentsEnded + 1L
    }
    
    firstDataLine <- scan(datafile,
                          what="character",
                          sep='\n',
                          quiet = T,
                          skip = commentsEnded,
                          nlines = 1)
    if(isTRUE(grep(",", firstDataLine, fixed = TRUE))) {
      sep = ","
    } else {
      sep = " "
    }
    
    inputdata <- read.table(datafile,
                            comment.char = '#',
                            sep = sep)
  }
  
  resmodel <- modelBuilderUI(data = inputdata, modelName = modelName)
  stopifnot(class(resmodel)[1] == "NlmePmlModel")

  cat("## Description: ", file = metamodelFile, append = FALSE)
  cat("\n## Author:", author, file = metamodelFile, append = TRUE)
  cat("\n## DATA", datafile, file = metamodelFile, append = TRUE)
  
  covariateList <- resmodel@covariateList
  covnames <- lapply(covariateList, function(x) x@name)
  names(covariateList) <- unlist(covnames)
  
  mapping <- resmodel@columnMapping@mapping
  finalMapText <- ""
  for(mapTerm in mapping) {
    columnName <- mapTerm@columnName
    variableType <- mapTerm@variableType
    if(variableType$type == "extraDoses") next
    if(columnName != "?") {
      variableName <- mapTerm@variableName
      currentMapText <- paste0(variableName, "=", columnName)
      if(variableType$type == "covariate"){
        if(variableType$covType == 2){
          covParm <- covariateList[[variableName]]
          cov_lvl_lbl <- get_lvl_lbl_mmdl_out(covParm)
          currentMapText <- paste0(currentMapText, cov_lvl_lbl, sep = "")
        }
      }
      finalMapText <- paste(finalMapText, currentMapText)
    }
  }
  
  cat("\n## MAP", finalMapText, file = metamodelFile, append = TRUE)
  
  userDefinedExtraDefsFlags <- grepl("\\w+", unlist(resmodel@userDefinedExtraDefs))
  userDefinedExtraDefs <- unlist(resmodel@userDefinedExtraDefs)[userDefinedExtraDefsFlags]
  if(length(userDefinedExtraDefs) > 0) {
    cat("\n## COLDEF", userDefinedExtraDefs, file = metamodelFile, append = TRUE, sep = "\n")
    hasColDef <- TRUE
  } else {
    hasColDef <- FALSE
  }
  eDoseLines <- Certara.RsNLME::extraDoseLines(resmodel)
  
  if(length(eDoseLines) > 0){
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
  
  if(length(eDoseLines) > 0) {
    if(hasColDef){
    cat(eDoseLines, file = metamodelFile, append = TRUE, sep = "\n")
    } else {
      cat("\n## COLDEF", eDoseLines, file = metamodelFile, append = TRUE, sep = "\n")
      }
    }
  }
  
  statements <- unlist(resmodel@statements)
  cat("\n## MODEL\n", paste(statements, collapse = "\n"), file = metamodelFile, append = TRUE)
  
  cat("\n## ESTARGS\n", file = metamodelFile, append = TRUE)
  
  cat("\n## TABLES\n", file = metamodelFile, append = TRUE)
  # cat("\n# do not change anything in the following binary code!\n", file = metamodelFile, append = TRUE)
  # cat("\n## RSNLMEDATA\n", file = metamodelFile, append = TRUE)
  # resmodelwodata <- resmodel
  # resmodelwodata@inputData <- NULL
  # tempRDS <- file.path(tempdir(TRUE), modelName, paste0(modelName, ".RDS"))
  # saveRDS(resmodelwodata, file = tempRDS)
  # file.append(metamodelFile, tempRDS)
  return(invisible(resmodel))
}
