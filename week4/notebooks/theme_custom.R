library(ggplot2)

theme_custom <- function(base_size = 12, base_family = "sans") {
    theme_minimal(base_size = base_size, base_family = base_family) +
        theme(
            # Text elements
            text = element_text(color = "#2C3E50"),
            plot.title = element_text(
                size = rel(1.4),
                face = "bold",
                hjust = 0,
                margin = margin(b = 10)
            ),
            plot.subtitle = element_text(
                size = rel(1.1),
                hjust = 0,
                margin = margin(b = 15)
            ),
            axis.title = element_text(
                size = rel(1),
                face = "bold"
            ),
            axis.text = element_text(
                size = rel(0.9),
                color = "#5D6D7E"
            ),
            
            # Grid and background
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "#F8F9F9", color = NA),
            panel.grid.major = element_line(color = "#BDC3C7", size = 0.3),
            panel.grid.minor = element_line(color = "#ECF0F1", size = 0.2),
            
            # Axis lines and ticks
            axis.line = element_line(color = "#34495E", size = 0.5),
            axis.ticks = element_line(color = "#34495E", size = 0.3),
            
            # Legend
            legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "#BDC3C7"),
            legend.key = element_rect(fill = "white", color = NA),
            legend.title = element_text(face = "bold", size = rel(1)),
            legend.text = element_text(size = rel(0.9)),
            legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
            
            # Margins
            plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
            
            # Facets
            strip.background = element_rect(fill = "#34495E", color = NA),
            strip.text = element_text(color = "white", face = "bold", size = rel(1))
        )
}

# Test it
ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
    geom_point(size = 3) +
    labs(
        title = "My Custom Theme",
        subtitle = "Horsepower vs MPG by Cylinder",
        x = "Miles per Gallon",
        y = "Horsepower",
        color = "Cylinders"
    ) +
    theme_custom()
