renderStParmName <- function(model){
  namesStParm <- structuralParameterNames(model)
  r <- vector(mode = "list", length = length(namesStParm))
  for (i in seq_along(namesStParm)) {
    r[[i]] <- tagList(
      # div(style = "padding:4px;"),
      textInput(paste0("stParmName", i), label = NULL, value = namesStParm[[i]]) %>% shinyjs::disabled(),
      # div(style = "padding:11px;")
    )
  }
  r
}

changeStyleChoice <- function(styleInput, choices){
  pos <- match(styleInput, choices)
  return(names(choices)[pos])
}

renderMetaStParm <- function(modelUser, input, stparms){
  choices <- c(LogNormal = "Product * exp(Eta)", LogNormal1 = "Sum * exp(Eta)", LogNormal2 =  "exp(Sum + Eta)", LogitNormal = "ilogit(Sum + Eta)", Normal = "Sum + Eta")
  posStSelect <- grep(pattern = "stParmName", names(input))
  
  for(i in posStSelect){
    n <- names(input)[[i]]
    if(!is.null(input[[n]])){
      st_parmname <- input[[n]]
      if(st_parmname %notin% stparms) next
      panel <- substr(n, 11,13)
      if(input[["pk_switch_population"]] == TRUE){
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                structuralParameter(paramName = ..(st_parmname), fixedEffName = ..(input[[paste0("strFixEffName", panel)]]), randomEffName = ..(input[[paste0("strRanEffName", panel)]]), style = ..(changeStyleChoice(input[[paste0("strStyle",panel)]], choices)), hasRandomEffect = ..(input[[paste0("strRanEff", panel)]]))
            })
          },
          error = function(e) {
            return(modelUser)
          })
        } else {
          modelUser <- tryCatch({
            metaExpr({
              ..(modelUser) %>%
                structuralParameter(paramName = ..(st_parmname), fixedEffName = ..(input[[paste0("strFixEffName", panel)]]), style = ..(changeStyleChoice(input[[paste0("strStyle",panel)]], choices)))
            })
          },
          error = function(e) {
            return(modelUser)
          })
        }
    }
  }
  return(modelUser)
}


remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}

pk_stparms <- c("V", "Cl", "V2", "Cl2", "V3", "Cl3", "Ka", 
                "Ke", "K12", "K21", "K13", "K31", 
                "A", "Alpha", "B", "Beta", "C", "Gamma", "Fe", "Tlag", "Km", "Vmax",
                "MeanDelayTime", "ShapeParamMinusOne", "ShapeParam")

emax_stparms <- c("EC50", "Emax", "Ke0", "IC50", "Gam", "E0", "Imax")

indirect_stparms <- c("Kin", "Kout", "EC50", "Emax", "Ke0", "gam", "s")

linear_stparms <- c("EAlpha", "EBeta", "EGam", "Ke0")
