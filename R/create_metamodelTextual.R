# removing comments from the model statements
.extract_comments <- function(statementsLines, type = "All") {
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

  Comments <- unlist(regmatches(statementsLines, gregexpr(pattern, statementsLines, perl = TRUE)))

  Comments
}

#' Send metamodel to \code{modelTextualUI()} and run shiny application
#'
#' Used by Pirana to send existing metamodel to \code{modelTextualUI()} for
#' editing, and after returning, saves it as a metamodel file e.g, .mmdl.
#'
#' @param metamodelFile Path to existing metamodel file.
#'
#' @return Updated metamodel text.
#'
#' @details If \code{DOSING CYCLE} block is presented in the metamodel, it will be
#' transferred to \code{COLDEF} block with a warning.
#' 
#' @examples
#' if (interactive()) {
#' mmdl_file <- system.file("vignettesdata/OneCpt_IVInfusion.mmdl",
#'   package = "Certara.RsNLME")
#' 
#' create_metamodelTextual(
#'   mmdl_file
#'   )
#' }
#' @export
create_metamodelTextual <- function(metamodelFile) {
  metamodelFile <- gsub("\\", "/", metamodelFile, fixed = TRUE)
  if (!file.exists(metamodelFile)) {
    stop("File ", metamodelFile, " not found.")
  }

  mmdl_model <- Certara.RsNLME::create_model_from_metamodel(metamodelFile, tempdir(TRUE))

  exUserDef <- mmdl_model$model@userDefinedExtraDefs
  mmdl_model$model@userDefinedExtraDefs <- as.list(exUserDef[exUserDef != ""])

  resmodel <- modelTextualUI(baseModel = mmdl_model$model)
  if (identical(resmodel, mmdl_model$model)) {
    return(readLines(metamodelFile))
  }
  stopifnot(class(resmodel)[1] == "NlmePmlModel")

  mmdlPrevModel <- .restore_model(metamodelFile)
  mmdl_withComments <- mmdlPrevModel$metamodelPart
  blocksNoCase <- c("Author:", "Description:", "Based on:")
  blocksCase <- c(
    "DATA1", "MAP1", "DATA", "MAP", "DOSING CYCLE",
    "COLDEF", "MODEL",
    "ESTARGS", "TABLES", "RSNLMEDATA"
  )
  blocks <- c(blocksNoCase, blocksCase)
  # splitting to the blocks
  splitNoCase <- paste0("\\s*", blocksNoCase, collapse = "|")
  splitCase <- paste0("\\s*", blocksCase, collapse = "|")

  mmdl_blocksNoCase <-
    unlist(strsplit(mmdl_withComments,
      split = paste0("(?i)##(?=", splitNoCase, ")"),
      perl = TRUE
    ))
  mmdl_blocks <-
    unlist(strsplit(mmdl_blocksNoCase,
      split = paste0("##(?=", splitCase, ")"),
      perl = TRUE
    ))

  mmdl_blocks <- mmdl_blocks[mmdl_blocks != ""]

  # What do do if MAP is not available in mmdl blocks, we can try....

  if (!(any(grepl("^MAP", x = trimws(mmdl_blocks))))) {
    mmdl_blocks <- append(mmdl_blocks, " MAP", after = 1)
  }

  blockNameParsed <- c()
  for (blockIndex in seq_along(mmdl_blocks)) {
    blockFlag <- sapply(
      blocks,
      function(x, mmdl_block) {
        return(grepl(paste0("^\\s*", x), mmdl_block, ignore.case = TRUE))
      },
      mmdl_blocks[blockIndex]
    )
    if (any(blockFlag)) {
      blockNameParsed[blockIndex] <- trimws(blocks[blockFlag])
    } else {
      blockNameParsed[blockIndex] <- ""
    }
  }
  if (!("MAP" %in% blockNameParsed)) {
    blockNameParsed <- append(blockNameParsed, "MAP", after = 1)
  }


  newMMDL <- character(0)
  covariateList <- resmodel@covariateList
  covnames <- lapply(covariateList, function(x) x@name)
  names(covariateList) <- unlist(covnames)

  OSCR <- ifelse(.Platform$OS.type == "windows", "\r\n", "\n")
  for (blockIndex in seq_along(mmdl_blocks)) {
    if (blockNameParsed[blockIndex] %in% c("DATA1", "DATA")) {
      # keep it as is
      newMMDL <- paste0(newMMDL, "##", mmdl_blocks[[blockIndex]])
    } else if (blockNameParsed[blockIndex] %in% c("MAP1", "MAP")) {
      mapping <- resmodel@columnMapping@mapping
      if (!identical(mapping, mmdl_model$model@columnMapping@mapping)) {
        # there are changes made
        finalMapText <- " "
        for (mapTerm in mapping) {
          columnName <- mapTerm@columnName
          variableType <- mapTerm@variableType
          if (columnName != "?") {
            variableName <- mapTerm@variableName
            currentMapText <- paste0(variableName, "=", columnName)
            if (variableType$type == "covariate") {
              if (variableType$covType == 2) {
                covParm <- covariateList[[variableName]]
                cov_lvl_lbl <- get_lvl_lbl_mmdl_out(covParm)
                currentMapText <- paste0(currentMapText, cov_lvl_lbl, sep = "")
              }
            }
            finalMapText <- paste(finalMapText, currentMapText)
          }
        }

        if ("DATA1" %in% blockNameParsed) {
          newMMDL <- paste0(newMMDL, "## MAP1", finalMapText)
        } else {
          newMMDL <- paste0(newMMDL, "## MAP", finalMapText)
        }

        comments <- paste0(.extract_comments(mmdl_blocks[[blockIndex]]), collapse = OSCR)
        newMMDL <- paste0(newMMDL, OSCR, comments)
      } else {
        # keep it as is
        newMMDL <- paste0(newMMDL, "##", mmdl_blocks[[blockIndex]])
      }
    } else if (blockNameParsed[blockIndex] %in% c("DOSING CYCLE")) {
      warning("DOSING CYCLE block information is transferred to COLDEF block.")
    } else if (blockNameParsed[blockIndex] %in% c("COLDEF")) {
      userDefinedExtraDefs <- resmodel@userDefinedExtraDefs
      if (!identical(userDefinedExtraDefs, mmdl_model$model@userDefinedExtraDefs)) {
        # there are changes made
        userDefinedExtraDefsFlags <- grepl("\\w+", unlist(resmodel@userDefinedExtraDefs))
        userDefinedExtraDefs <- 
          paste0(unlist(resmodel@userDefinedExtraDefs)[userDefinedExtraDefsFlags], collapse = OSCR)
        if (length(userDefinedExtraDefs) > 0) {
          newMMDL <- paste0(newMMDL, "## COLDEF ", OSCR, userDefinedExtraDefs, OSCR)
        } else {
          newMMDL <- paste0(newMMDL, "## COLDEF ", OSCR)
        }
        comments <- paste0(.extract_comments(mmdl_blocks[[blockIndex]]), collapse = OSCR)
        newMMDL <- paste0(newMMDL, OSCR, comments)
      } else {
        # keep it as is
        newMMDL <- paste0(newMMDL, "##", mmdl_blocks[[blockIndex]])
      }
    } else if (blockNameParsed[blockIndex] %in% c("MODEL")) {
      if (!"COLDEF" %in% toupper(blockNameParsed)) {
        userDefinedExtraDefsFlags <- grepl("\\w+", unlist(resmodel@userDefinedExtraDefs))
        userDefinedExtraDefs <- paste0(unlist(resmodel@userDefinedExtraDefs)[userDefinedExtraDefsFlags], collapse = OSCR)
        if (length(userDefinedExtraDefs) > 0) {
          newMMDL <- paste0(newMMDL, "## COLDEF ", OSCR, userDefinedExtraDefs, OSCR)
        }
      }

      statementsWOEnds <-
        sapply(unlist(resmodel@statements),
          function(x) {
            if (!grepl("\\n$", x)) {
              x <- paste0(x, OSCR, collapse = "")
            }
            x
          },
          USE.NAMES = FALSE
        )
      statements <- paste0(statementsWOEnds, collapse = "")

      if (!identical(statements, mmdl_model$model@statements)) {
        # there are changes made
        newMMDL <- paste0(newMMDL, "## MODEL ", statements)
      } else {
        # keep it as is
        newMMDL <- paste0(newMMDL, mmdl_blocks[[blockIndex]])
      }
    } else if (blockNameParsed[blockIndex] %in% c("ESTARGS", "TABLES")) {
      # keep it as is
      newMMDL <- paste0(newMMDL, "##", mmdl_blocks[[blockIndex]])
    } else {
      # keep it as is
      newMMDL <- paste0(newMMDL, "##", mmdl_blocks[[blockIndex]])
    }

    if (!grepl("\\n$", newMMDL)) {
      newMMDL <- paste0(newMMDL, OSCR)
    }
  }

  writeChar(newMMDL, con = metamodelFile, eos = NULL)

  newMMDL
}



get_lvl_lbl_mmdl_out <- function(covParameter) {
  covItems <- covParameter@covarItems

  lvl_lbls_out <- list()
  for (i in seq_along(covItems)) {
    lbl <- trimws(covItems[[i]]@name)
    lvl <- covItems[[i]]@value

    lvl_lbl <- paste(lbl, lvl, sep = " = ")

    lvl_lbls_out[[i]] <- lvl_lbl
  }

  ret <- unlist(lvl_lbls_out)

  ret <- paste0(ret, collapse = ", ")

  ret <- paste0("(", ret, ")")

  return(ret)
}
