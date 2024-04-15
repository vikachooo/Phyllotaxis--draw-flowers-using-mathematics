
library(tidyverse)
library(patchwork)

# Set plot images to a nice size
options(repr.plot.width = 6, repr.plot.height = 6)

# Create circle data to plot
t <- seq(1:44)
x <- sin(t)
y <- cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a circle
p <- ggplot(df, aes(x, y))
p + geom_point() + coord_fixed()


# Define the number of points
points <- 500


#### Golden Angle #### 

# Define the Golden Angle
angle <- pi * (3 - sqrt(5))

#### flower 1 #### 
t <- (1:points) * angle
x1 <- sin(t)
y1 <- cos(t)
df1 <- data.frame(t, x1, y1)

# Calculate sizes based on t, normalized to a suitable range for point sizes
sizes <- sqrt(t/max(t)) * 10  # BIG points
sizes <- max(t) / t * 6 # small points  

# Smaller values of t (which occur at the center of the spiral) result in larger sizes because dividing 
# a larger number by a smaller number yields a larger result.

sizes <- sqrt(t/max(t)) * 10  
sizes <- (1 - sqrt(t/max(t)))

# Make a scatter plot of points in a spiral
flower1 <- ggplot(df1, aes(x1*t, y1*t, color = t, size = sizes)) +
                      geom_point(alpha = 0.8) +
  coord_fixed() +  # This will ensure the plot is round
  scale_size_continuous(range = c(1, 40)) +
  scale_color_gradient(low = "deeppink4", high = "lightpink") +
                        theme(panel.background = element_blank(),
                        axis.title = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank(),
                        legend.position = "none")
flower1


#### flower 2 #### 
angle <- pi * (3 - sqrt(5))
t <- (1:points) * angle
r <- sqrt(1:points)  # Increase radius with each point for a more spiral appearance
x2 <- r * sin(t)
y2 <- r * cos(t)
df2 <- data.frame(x2, y2)

sizes2 <- (1 - sqrt(t/max(t)))


# Make a scatter plot of points in a spiral
flower2 <- ggplot(df2, aes(x2, y2, color = t, size = sizes)) +
  geom_point() +
  coord_fixed() +
  scale_size_continuous(range = c(1, 20)) +
  scale_color_gradient(low = "deeppink4", high = "lightpink") +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

flower2 #better than 1


flower1 + flower2 


#### dandelion ####
flower3 <- ggplot(df1, aes(x1*t, y1*t)) + geom_point(aes(size = t, alpha = 0.5), shape = 8, color = "black") +
  coord_fixed() +
  scale_size_continuous(range = c(1, 10)) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")


flower3




#### Function ####

library(viridis)
library(tibble)


my_flower <- function(points = 5000, num_colour = 9, col_option = "magma", angle = pi * (3 - sqrt(5)), alpha = 0.8,...) {
  # Create the tibble with appropriate assignments for all calculations
  flower <- tibble(
    n = 1:points,  # Number of points
    r = sqrt(n),   # Radius for each point
    t = (1:points) * angle,  # Calculate t for each point
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
    scale_size_continuous(range = c(1, 10)) +
    scale_colour_viridis_d(option = col_option, guide = "none") +  # Using discrete color scale
    coord_fixed() +
    theme_void(base_family = "Roboto Condensed") +
    theme(legend.position = "none")
  
  return(flower_plot)
}

# Example usage
plot <- my_flower(points = 1000, num_colour = 6, col_option = "plasma")
print(plot)


