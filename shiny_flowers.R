library(shiny)
library(ggplot2)
library(viridis)
library(dplyr)
library(tibble)


my_flower <- function(points, num_colour, col_option, angle, alpha, size_min, size_max) {
  # converting the user input string for angle to an executable R expression
  angle_eval <- eval(parse(text = angle))
  
  flower <- tibble(
    n = 1:points,  # Number of points
    r = sqrt(n),   # Radius for each point
    t = (1:points) * angle_eval,  # Calculate t for each point
    colour = n %% num_colour  # Colour cycling based on modulo operation
  ) %>%
    mutate(
      x = r * cos(t),  # X coordinate based on polar to Cartesian conversion
      y = r * sin(t),  # Y coordinate based on polar to Cartesian conversion
      sizes = 1 - sqrt(t/max(t))  # Calculate sizes based on t
    )
  
  # Create the plot
  flower_plot <- ggplot(flower, aes(x = x, y = y, colour = as.factor(colour), size = sizes)) +
    geom_point(alpha = alpha) +
    geom_path(size = 0.01, alpha = 0.3) +
    scale_size_continuous(range = c(size_min, size_max)) +
    scale_colour_viridis_d(option = col_option, guide = "none") +  # Using discrete color scale
    coord_fixed() +
    theme_void(base_family = "Roboto Condensed") +
    theme(legend.position = "none")
  
  return(flower_plot)
}


ui <- fluidPage(
  titlePanel("Drawing flowers"),
  sidebarLayout(
    sidebarPanel(
      selectInput("col_option", "Color:", choices = c("magma", "viridis", "plasma", "inferno", "cividis")),
      textInput("angle", "Enter Angle Formula:", value = "pi * (3 - sqrt(5))"),
      sliderInput("n_cols", "Number of colors:", min = 1, max = 50, value = 10),
      sliderInput("n_points", "Number of points:", min = 100, max = 100000, value = 5000),
      sliderInput("size_min", "Minimum Point Size:", min = 0, max = 20, value = 1),
      sliderInput("size_max", "Maximum Point Size:", min = 0, max = 20, value = 10)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    if (nzchar(input$angle)) {
      plot <- my_flower(
        points = as.numeric(input$n_points),
        num_colour = as.numeric(input$n_cols),
        col_option = input$col_option,
        angle = input$angle,
        alpha = 0.8,
        size_min = input$size_min,
        size_max = input$size_max
      )
      print(plot)
    }
  })
}

shinyApp(ui = ui, server = server)

