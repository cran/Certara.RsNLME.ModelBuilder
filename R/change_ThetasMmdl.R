#' Passes the metamodel to estimatesUI and, after returning, saves it as a metamodel file
#'
#' Passes the metamodel to estimatesUI and, after returning, saves it as a metamodel file
#'
#' @param metamodelFile File where the metamodel should be updated
#'
#' @return changed metamodel text
#' @examples
#' if (interactive()) {
#'   OneCpt_IVInfusionFile <- system.file("vignettesdata/OneCpt_IVInfusion.mmdl",
#'   package = "Certara.RsNLME",
#'   mustWork = TRUE)
#'   
#'   change_ThetasMmdl(OneCpt_IVInfusionFile)
#' }
#' 
#' @export
#' @keywords internal
change_ThetasMmdl <- function(metamodelFile) {

  metamodelFile <- gsub("\\", "/", metamodelFile, fixed = TRUE)
  if(!file.exists(metamodelFile)){
    stop("File ", metamodelFile, " not found.")
  }

  # get the text of mmdl
  mmdlPrevModel <- .restore_model(metamodelFile)
  mmdl_withComments <- mmdlPrevModel$metamodelPart

  # get the initial model
  mmdl_model <- Certara.RsNLME::create_model_from_metamodel(metamodelFile, tempdir(TRUE))

  # get the resulted model
  resmodel <- estimatesUI(model = mmdl_model$model)
  if (identical(resmodel, mmdl_model$model)) {
    return(readLines(metamodelFile))
  }

  stopifnot(inherits(resmodel, "NlmePmlModel"))

  Certara.RsNLME::saveUpdatedMetamodel(mmdl_withComments,
                       mmdl_model,
                       resmodel,
                       metamodelFile)
}

.restore_model <- function(metaModelFile) {
  prevModel <- NULL
  
  metaModelRaw <- readBin(metaModelFile, "raw", n = 30000)
  metamodelPart <- rawToChar(metaModelRaw)
  
  return(list(metamodelPart = metamodelPart, prevModel = prevModel))
}
