#' @importFrom stats na.omit setNames
#' @importFrom utils read.table write.table write.csv
#' @importFrom methods .hasSlot new
#' 
NULL

model_builder_env <- new.env()

utils::globalVariables(
  c("x",
    "y",
    "px",
    "py",
    "subject",
    "input",
    "%+replace%",
    "theme",
    "sortColumns"
    ))
