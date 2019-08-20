#' @title Shiny modules for topological point cloud annotations
#' @name shiny-simplicial-complex
#' @family shiny modules

#' @rdname shiny-simplicial-complex
#' @export
pointCloudInput <- function(id, label = "Point Cloud") {
  # create namespace
  ns <- NS(id)
  
  tagList(
    # selector for point cloud sampler
    selectInput(
      ns("sampler"), label,
      c(
        `Archimedian Spiral` = "arch_spiral",
        Circle = "circle",
        Disk = "disk",
        `Figure Eight` = "figure_eight",
        `Klein Bottle` = "klein_bottle",
        Torus = "torus"
      )
    ),
    # slider for sample size
    sliderInput(
      ns("sample_size"), "Sample Size",
      min = 12, max = 720, step = 12
    )
  )
}

#' @rdname shiny-simplicial-complex
#' @export
pointCloud <- function(input, output, session) {
  
  # get manifold sampler
  sample_fun <- get(paste0("sample_", input$sampler), envir = "tdaunif")
  
  # generate sample
  x <- sample_fun(n = input$sample_size)
  
  x
}

#' @rdname shiny-simplicial-complex
#' @export
simplicialComplexInput <- function(id, label = "Simplicial Complex") {
  # create namespace
  ns <- NS(id)
  
  tagList(
    # selector for type of simplicial complex
    selectInput(
      ns("type"), label,
      c(
        Disk = "disk",
        Vietoris = "vietoris2",
        Cech = "cech2"
      )
    ),
    # slider for diameter
    # -+- reactively set maximum to data diameter -+-
    sliderInput(
      ns("diameter"), "Diameter",
      min = 0, max = 1
    )
  )
}

#' @rdname shiny-simplicial-complex
#' @export
simplicialComplex <- function(input, output, session, x) {
  
  # data diameter
  diameter <- reactive({
    ch <- chull(x)
    max(dist(x[ch, , drop = FALSE]))
  })
  
  # format data as x and y coordinates
  if (ncol(x) > 2) {
    x <- x[, 1:2, drop = FALSE]
  } else if (ncol(x) == 1) {
    stop("Point cloud ", deparse(substitute(x)), " must be 2-dimensional.")
  }
  x <- as.data.frame(x)
  x <- setNames(x, c("x", "y"))
  
  # plot of point cloud
  gg <- ggplot(x, aes(x = x, y = y)) + geom_point()
  
  # topological annotation
  stat_fun <- get(paste0("stat_", input$type), envir = "ggtda")
  stat_layer <- if (input$type == "disk") {
    stat_fun(radius = diameter / 2)
  } else {
    stat_fun(diameter = diameter)
  }
  gg <- gg + stat_layer
  
  # output plot
  output$plot <- renderPlot({
    return(gg)
  })
}
