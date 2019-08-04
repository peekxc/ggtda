#' dmg_select_ui
#' @description Shiny UI module for interacting with various types of persistence data. 
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
dmg_select_ui <- function(id) {
  ns <- NS(id)
  
  # Define the types of visualizations for persistence data
  plot_types <- list("Diagram", "Barcode")
  
  # Selector for diagram type
  pt_selector <- selectInput(ns("pers_pt"), "Plot Type", c(Diagram = "dgm", Barcodes = "barcode"))

  # Conditional panel to change configuration options for different plot types
  tagList(
    pt_selector, 
    conditionalPanel(
      condition = "input.pers_pt == 'dgm'",
      tagList(
        selectInput(ns("dgm_pt"), label = "Orientation", choices = c(Flat="flat",Diagonal="diagonal",Landscape="landscape")), 
        radioButtons(ns("dgm_show_frontier"), label = "Show frontier", choices = c("Yes", "No"), selected = "No")
      ),
      ns = ns
    )
  )
}

#' Persistence diagram selection 
#' @param input, output, session standard \code{shiny} boilerplate
#' @return list with following components
#' \describe{
#'   \item{plot_type}{reactive character string indicating the type of diagram to plot}
#' }
#' @export
dgm_select <-  function(input, output, session) {
  return(
    list(
      plot_type = reactive({ input$pers_pt }), 
      dgm_pt = reactive({ input$dgm_pt }), 
      dgm_show_frontier = reactive({ input$dgm_show_frontier })
    )
  )
}

#' dgm_var_UI
#' @description Variables UI control selection for persistence diagrams.
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @details \code{shiny::\link[shiny]{tagList}} returned includes a selectizeInput, a numericInput, and
#' a dynamically generated uiOutput that becomes a range slider.
#' @import shiny
#' @export
dgm_var_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("dim_select"), label = "Dimensions", choices = c("H0", "H1", "H2"), selected = list("H0", "H1"), multiple = TRUE),
    numericInput(ns("noise_threshold"), label = "Noise threshold", value = 0, min = 0),
    uiOutput(ns("time_window_ui"))
    # selectInput(ns("plot_type"), label = "Plot type", choices = c("Barcodes", "Diagrams", "Landscapes"), selected = "Barcodes")
  )
}

#' dgm_var
#' Variables selection sever-module for persistence diagrams.
#' @param dgm a (non-reactive) data.frame with names 'appear', 'disappear', and 'dim'.
#' @return list with following components
#' \describe{
#'   \item{time_window}{reactive numeric vector indicating the beirth and death time window to show}
#'   \item{dims_selected}{reactive integer indicating the homology groups to show}
#'   \item{noise_threshold}{reactive character string indicating the noise threshold}
#' }
#' @export
dgm_var <- function(input, output, session, dgm){
  if (any(c("appear", "disappear", "dim") %in% names(dgm))){
    dgm_names <- names(dgm)
    names(dgm) <- sapply(dgm_names, function(nm) switch(nm, "dim"="dimension", "appear"="birth", "disappear"="death", nm))
  }
  stopifnot(all(c("birth", "death", "dimension") %in% colnames(dgm)), is.data.frame(dgm), is.factor(dgm$dimension))
  
  ## Time window
  output$time_window_ui <- renderUI({
    ns <- session$ns
    min_time <- min(dgm$birth)
    max_time <- max(dgm$death[dgm$death != Inf])
    sliderInput(ns("time_window"), label = "Time window",
                min = round(min_time, 3), max = round(max_time, 3),
                value = c(min_time, max_time), dragRange = TRUE)
  })
  
  ## Update noise threshold step value
  updateNumericInput(session, inputId = "noise_threshold", step = min(dgm$death - dgm$birth))
  
  ## Return the selected diagram
  return(list(
    time_window = reactive({ input$time_window }),
    dims_selected = reactive({ (match(input$dim_select, c("H0", "H1", "H2"))-1L) }),
    noise_threshold = reactive({ input$noise_threshold })
  ))
}


#' linkedPersistenceOutput
#' @description Output UI module for creating a plot of persistence data.
#' @param id 
#' @export
linkedPersistenceOutput <- function(id, ...){
  ns <- NS(id)
  plotOutput(ns("pers_plot"), click = ns("pers_click"), ...)
}

#' linkedPersistence
#' @param dgm a (non-reactive) data.frame with names 'birth', 'death', and 'dimension'.
#' @param dgm_vars list containing reactive expressions 'time_window', 'dims_selected', and 'noise_threshold'
#' @param plot_params reactive expression of ggplot2 elements to add
#' @import ggplot2
#' @return list with following components
#' \describe{
#'   \item{xvar}{reactive character string indicating x variable selection}
#'   \item{yvar}{reactive character string indicating y variable selection}
#' }
#' @export
linkedPersistence <- function(input, output, session, dgm, dgm_selection, dgm_vars = reactive({ NULL })){
  if (any(c("appear", "disappear", "dim") %in% names(dgm))){
    dgm_names <- names(dgm)
    names(dgm) <- sapply(dgm_names, function(nm) switch(nm, "dim"="dimension", "appear"="birth", "disappear"="death", nm))
  }
  stopifnot(all(c("birth", "death", "dimension") %in% names(dgm)), is.data.frame(dgm), is.factor(dgm$dimension))
  
  ## Reactive that chooses the subset of the diagram based on dimension, noise threshold, etc.
  lifetimes <- abs(dgm$death - dgm$birth)
  sub_dgm <- reactive({
    dim_idx <- as.integer(as.character(dgm$dimension)) %in% dgm_vars$dims_selected()
    thresh_idx <- lifetimes >= dgm_vars$noise_threshold()
    dgm[(dim_idx & thresh_idx),,drop=FALSE]
  })

  ## The output persistence plots
  output$pers_plot <- renderPlot({
    aes_map <- aes(start = birth, end = death, colour = dimension)
    base_plot <- ggplot(sub_dgm(), aes_map) + 
      theme_tda() + 
      coord_cartesian(xlim = dgm_vars$time_window())
    
    ## Switch up the type of geom/stat based on selection
    if (dgm_selection$plot_type() == "barcode"){
      return(base_plot + geom_barcode() + labs(x = "Diameter", y = "Homological features"))
    } else if (dgm_selection$plot_type() == "dgm"){
      diagram_format <- dgm_selection$dgm_pt()
      show_frontier <- (dgm_selection$dgm_show_frontier() == "Yes")
      new_plot <- base_plot + stat_persistence(diagram = diagram_format)
      if (diagram_format == "diagonal" || diagram_format == "landscape"){
        new_plot <- new_plot
      } else if (diagram_format == "flat"){
        new_plot <- new_plot + geom_abline( intercept = 0, slope = 1, color = "darkgoldenrod", linetype = "dashed" )
      } 
      if (show_frontier){
        new_plot <- new_plot + stat_frontier(diagram = diagram_format)
      }
      return(new_plot)
    } else { stop("Unkown plot type selection.") }
  })
  
  ## Return the current diagram subset in view, and the index of the selected feature (if the plot has been clicked)
  selected_feature <- reactive({
    if (is.null(input$pers_click)){ return(NULL) }
    dim_idx <- as.integer(as.character(dgm$dimension)) %in% dgm_vars$dims_selected()
    thresh_idx <- lifetimes >= dgm_vars$noise_threshold()
    which(dim_idx & thresh_idx)[round(input$pers_click$y)]
  })
  return(list(
    dgm_subset = sub_dgm,
    selected_feature_idx = selected_feature
  ))
}



