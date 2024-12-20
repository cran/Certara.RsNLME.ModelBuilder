#app_styling

styleCSS <- 
  
  ".certara-page {
    display: flex;
    flex-direction: column;
    min-height: 100vh;
  }
  
  .bslib-sidebar-layout .sidebar {
    background-color: #fff;
  }
  
  .tab-pane.raneff-pane .card-body.bslib-gap-spacing.html-fill-item.html-fill-container {
    padding: 0px;
  }
  
  .tab-pane.raneff-pane .card-body.bslib-gap-spacing.html-fill-item.html-fill-container .card-body.bslib-gap-spacing.html-fill-item.html-fill-container {
    padding: 12px;
  }
  
  .bslib-card,
  .bslib-card .card-body {
    overflow: visible;
  }
  
  .multi-input-with-checkbox {
    display: flex;
    align-items: baseline;
  }
  
  .multi-input-with-checkbox .col-sm-1,
  .multi-input-with-checkbox .col-sm-2,
  .multi-input-with-checkbox .col-sm-3 {
    align-self: baseline;
  }
  
  .multi-input-with-checkbox .col-checkbox {
    align-self: center;
    margin-bottom: 5px;
    padding-top: 2.3rem;
  }
  
  .ran-eff-matrix-card-body,
  .cov-eff-matrix-card-body{
    gap: 0;
  }
  
  .matrix-row {
    align-items: baseline;
  }
  
  .matrix-row .html-fill-container {
    align-self: flex-end;
  }
  
  .column-mapping-selectors .bslib-gap-spacing,
  .input-options .bslib-gap-spacing {
    row-gap: 0;
  }
  
  .column-mapping-selectors .shiny-html-output {
    padding-top: 5px;
  }
  
  .residual_errors .bslib-gap-spacing,
  .input-options .bslib-gap-spacing {
    row-gap: 0;
  }
  
  h4 {
    color: black;
    font-size: 22px;
    font-family: 'Segoe UI Light', Arial, sans-serif;
  }
  
  h5 {
    color: black; 
    font-size: 16px; 
    font-family: Segoe UI Light, Arial, sans-serif;
  }

  h6 {
    color: #9e9e9e;
    font-size: 0.7rem;
    line-height: 9%;
    margin: 0.366667rem 0 -0.54rem 0;
  }
  
  .shiny-input-container label {
    color: #9e9e9e;
    font-size: 0.7rem;
    text-wrap: wrap;
    margin: 0.366667rem 0 -0.54rem 0;
  }
  
  .shiny-input-container .checkbox label {
    color: initial;
    font-size: initial;
    line-height: initial;
    margin: initial;
  }
  
  .shiny-input-container:has(.selectize-control) {
    margin-bottom: 0;
    white-space: nowrap;
  }
  
  .multi-input-selectInputs .shiny-input-container:has(.selectize-control) {
    margin-bottom: 0;
    height: 3.28rem;
  }
  
  .textual-options .shiny-input-container:has(.selectize-control) {
    margin-bottom: 1rem;
  }
  
  .theta-warning {
    color: red;
    font-size: 0.9em;
  }

  body {
    background-color: #616161
  }
  
  body, .container-fluid {
    background-color: #616161;
    margin: 0;
    padding: 0;
  }
  
  .model-content {
    padding: 10px;
    flex: 1;
  }
  
  .model-content .bslib-mb-spacing {
    margin-bottom: 0;
  }
  
  .model-textual-content{
    padding: 10px;
  }

  .btn {
    background-color: #1d7eba;
    color: #fff;
    text-decoration: none;
    text-align: center;
    letter-spacing: .5px;
    transition: background-color .2s ease-out;
    cursor: pointer;
  }

  .btn:hover {
    background-color: #008CBA;
  }
  
  .save_btn {
    color: black;
    text-decoration: none;
    cursor: pointer;
  }

  .save_btn:hover {
    color: black;   
    text-decoration: none; 
  }
  
  #exitCancel {
    background-color: grey;
  }

  ::-webkit-scrollbar {
    width: 5px;
    height: 5px;
  }

  ::-webkit-scrollbar-track {
    background: #f1f1f1;
  }

  ::-webkit-scrollbar-thumb {
    background: #888;
  }

  ::-webkit-scrollbar-thumb:hover {
    background: #555;
  }

  .container::-webkit-scrollbar {
    display: none; /* Safari and Chrome */
  }

  .certara-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0px 20px;
    background-color: #d3d3d3;
    border-bottom: 2px solid #CC0000;
  }
  
  .logo-title {
    display: flex;
    align-items: center;
  }
  
  .logo-title img {
    margin-right: 10px;
  }

  .certara-footer {
    display: flex;
    justify-content: space-between;
    align-items: center;
    left: 0;
    bottom: 0;
    width: 100%;
    height: 35px;
    background-color: #d3d3d3;
    border-top: 1px solid #CC0000;
    color: #000;
    text-align: left;
    z-index: 12;
  }

  .shiny-output-error-validation {
    color: red;
    white-space: pre-wrap;
    font-size: 12px;
    font-family: 'Segoe UI Light', Arial, sans-serif;
  }

  .irs--shiny .irs-line::before,
  .irs--shiny .irs-bar::before {
    cursor: pointer !important;
  }

  .estimates-accordion .accordion-body {
    padding: 5px;
  }
  
  .ran_effects_div .selectize-control {
    white-space: normal;
  }"

styleInput <- "input:not([type]), input[type=text]:not(.browser-default), input[type=password]:not(.browser-default), input[type=email]:not(.browser-default), input[type=url]:not(.browser-default), input[type=time]:not(.browser-default), input[type=date]:not(.browser-default), input[type=datetime]:not(.browser-default), input[type=datetime-local]:not(.browser-default), input[type=tel]:not(.browser-default), input[type=number]:not(.browser-default), input[type=search]:not(.browser-default), textarea.materialize-textarea {
  background-color: transparent;
  border: none;
  border-bottom: 1px solid #9e9e9e;
  border-radius: 0;
  outline: none;
  height: 1.75rem;
  width: 100%;
  font-size: 14px;
  margin: 0 0 8px 0;
  padding: 0;
  -webkit-box-shadow: none;
  box-shadow: none;
  -webkit-box-sizing: content-box;
  box-sizing: content-box;
  -webkit-transition: border .3s, -webkit-box-shadow .3s;
  transition: border .3s, -webkit-box-shadow .3s;
  transition: box-shadow .3s, border .3s;
  transition: box-shadow .3s, border .3s, -webkit-box-shadow .3s;
}"

certara_header <- function(header_title) {
  div(class = "certara-header",
      div(class = "logo-title",
          tags$a(
            href = "https://www.certara.com",
            target = "_blank",
            class = "brand-logo",
            tags$img(src = "https://cdn.shortpixel.ai/spai/w_133+q_lossless+ret_img+to_webp/https://www.certara.com/app/uploads/2023/05/certara-logo-2023.png")
          ),
          h4(class = "header_title", header_title, style = "margin-top: 20px; font-family: Segoe UI !important")
      ),
      shiny::actionLink(inputId = "exitShiny", 
                        label = "Save & Exit", 
                        icon = icon("save"), 
                        class = "save_btn"
      )
  )
}

certara_footer <- function(url) {
  div(class = "certara-footer",
      tags$p(style = 'margin: 0; padding-right: 5px; font-size:small',
             HTML("&nbsp;&nbsp;"),
             tags$a(href = 'https://www.certara.com/', target = '_blank', 'Home'),
             HTML("&nbsp;&nbsp;"),
             tags$a(href = url, target = '_blank', 'Help'),
             HTML("&nbsp;&nbsp;"),
             tags$a(href = 'https://certara.service-now.com/csm', target = '_blank', 'Support'),
             HTML("&nbsp;&nbsp;"),
             tags$a(href = 'https://www.certara.com/legal/privacy-center/', target = '_blank', 'Privacy Policy')
      ),
      tags$p(style = 'margin: 0; padding-right: 5px; font-size:small; text-align: right;',
             HTML("&#169; 2011-2024 Certara USA, Inc., All rights reserved. Version: 3.0.1")
      )
  )
}

hideTab <- function(inputId, target,
                    session = getDefaultReactiveDomain()) {
  force(target)
  inputId <- session$ns(inputId)
  
  callback <- function() {
    session$sendChangeTabVisibility(
      inputId = inputId,
      target = target,
      type = "hide"
    )
  }
  session$onFlush(callback, once = TRUE)
}

# material_collapsible_item <- function(label, ..., icon =  NULL, active = FALSE) {
#   tags$li(
#     tags$div(
#       class = paste("collapsible-header", if (active) "active"),
#       if (!is.null(icon))
#         tags$i(class = "material-icons", icon),
#       label
#     ),
#     tags$div(
#       class = "collapsible-body",
#       tags$span(
#         ...
#       )
#     )
#   )
# }
# 
# material_collapsible <- function(..., depth = NULL, color = NULL, type = NULL){
#   tags$ul(
#     class = paste(
#       "collapsible",
#       if (!is.null(type)) type,
#       if (!is.null(depth)) paste0("z-depth-", depth),
#       if (!is.null(color)) color),
#     ...
#   )
# }

theme_certara <- function(base_size = 11,
                          base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22,
                          grid = c("none", "horizontal", "both")) {
  grid <- match.arg(grid)
  half_line <- base_size / 2
  theme_c <- ggplot2::theme(
    line = ggplot2::element_line(
      colour = "black",
      size = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect = ggplot2::element_rect(
      fill = "white",
      colour = "black",
      size = base_rect_size,
      linetype = 1
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "plain",
      colour = "black",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.9),
                                      colour = "grey40"),
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 0.8 * half_line / 2),
      vjust = 1
    ),
    axis.text.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = 0.8 * half_line / 2),
      vjust = 0
    ),
    axis.text.y = ggplot2::element_text(
      angle = 0,
      margin = ggplot2::margin(r = 0.8 * half_line / 2),
      hjust = 0.5
    ),
    axis.text.y.right = ggplot2::element_text(
      angle = -90,
      margin = ggplot2::margin(l = 0.8 * half_line / 2),
      hjust = 0.5
    ),
    axis.ticks = ggplot2::element_line(colour = "grey40", size = 0.3),
    axis.ticks.length = ggplot2::unit(half_line, "pt"),
    axis.ticks.length.x = ggplot2::unit(half_line, "pt"),
    axis.ticks.length.x.top = ggplot2::unit(half_line, "pt"),
    axis.ticks.length.x.bottom = ggplot2::unit(half_line, "pt"),
    axis.ticks.length.y = ggplot2::unit(half_line, "pt"),
    axis.ticks.length.y.left = ggplot2::unit(half_line, "pt"),
    axis.ticks.length.y.right = ggplot2::unit(half_line, "pt"),
    axis.title = ggplot2::element_text(colour = "grey40"),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = half_line / 2),
      vjust = 1
    ),
    axis.title.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = half_line / 2),
      vjust = 0
    ),
    axis.title.y = ggplot2::element_text(
      angle = 90,
      margin = ggplot2::margin(r = half_line / 2),
      vjust = 1
    ),
    axis.title.y.right = ggplot2::element_text(
      angle = -90,
      margin = ggplot2::margin(l = half_line / 2),
      vjust = 0
    ),
    legend.background = ggplot2::element_rect(colour = NA),
    legend.spacing = ggplot2::unit(2 * half_line, "pt"),
    legend.spacing.x = ggplot2::unit(half_line, "pt"),
    legend.spacing.y = NULL,
    legend.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    legend.key = ggplot2::element_rect(fill = "white", colour = "NA"),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(
      size = ggplot2::rel(0.9),
      margin = ggplot2::margin(r = 2 * half_line, unit = "pt")
    ),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(size = ggplot2::rel(0.9),
                                         hjust = 0),
    legend.title.align = NULL,
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.box = NULL,
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),
    panel.background = ggplot2::element_rect(fill = "white",
                                             colour = NA),
    panel.border = ggplot2::element_rect(
      fill = NA,
      colour = "grey60",
      size = 0.3
    ),
    panel.grid = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = "grey90", colour = "grey20"),
    strip.text = ggplot2::element_text(
      colour = "grey30",
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(0.3 * half_line, 0.3 * half_line, 0.5 * half_line, 0.3 * half_line)
    ),
    strip.text.x = NULL,
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),
    plot.background = ggplot2::element_rect(colour = "white"),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.8),
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = half_line)
    ),
    plot.tag = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0.5,
      vjust = 0.5
    ),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    complete = TRUE
  )
  
  
  
  if (grid == "horizontal") {
    theme_c <- theme_c %+replace% theme(
      panel.grid.major.y = ggplot2::element_line(colour = "grey90", size = 0.3),
      panel.grid.minor.y = ggplot2::element_line(colour = "grey90", size = 0.3)
    )
  } else if (grid == "both") {
    theme_c <- theme_c %+replace% theme(
      panel.grid.major = ggplot2::element_line(colour = "grey90", size = 0.3),
      panel.grid.minor = ggplot2::element_line(colour = "grey90", size = 0.3)
    )
  }
  
  
  
  theme_c
}
