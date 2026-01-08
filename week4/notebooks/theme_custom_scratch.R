library(ggplot2)

theme_custom_scratch <- function(base_size = 12, base_family = "sans") {
    theme(
        # Base text - all text inherits from this
        text = element_text(
            family = base_family,
            face = "plain",
            color = "#2C3E50",
            size = base_size,
            lineheight = 0.9,
            hjust = 0.5,
            vjust = 0.5,
            angle = 0,
            margin = margin(),
            debug = FALSE
        ),
        
        # Plot titles
        plot.title = element_text(
            size = base_size * 1.4,
            face = "bold",
            hjust = 0,
            vjust = 1,
            margin = margin(b = 10)
        ),
        plot.subtitle = element_text(
            size = base_size * 1.1,
            hjust = 0,
            vjust = 1,
            margin = margin(b = 15)
        ),
        plot.caption = element_text(
            size = base_size * 0.8,
            hjust = 1,
            vjust = 1,
            margin = margin(t = 10)
        ),
        plot.tag = element_text(
            size = base_size * 1.2,
            hjust = 0.5,
            vjust = 0.5
        ),
        
        # Axis titles
        axis.title = element_text(
            size = base_size * 1,
            face = "bold"
        ),
        axis.title.x = element_text(
            margin = margin(t = 10),
            vjust = 1
        ),
        axis.title.x.top = element_text(
            margin = margin(b = 10),
            vjust = 0
        ),
        axis.title.y = element_text(
            angle = 90,
            margin = margin(r = 10),
            vjust = 1
        ),
        axis.title.y.right = element_text(
            angle = -90,
            margin = margin(l = 10),
            vjust = 0
        ),
        
        # Axis text (tick labels)
        axis.text = element_text(
            size = base_size * 0.9,
            color = "#5D6D7E"
        ),
        axis.text.x = element_text(
            margin = margin(t = 5),
            vjust = 1
        ),
        axis.text.x.top = element_text(
            margin = margin(b = 5),
            vjust = 0
        ),
        axis.text.y = element_text(
            margin = margin(r = 5),
            hjust = 1
        ),
        axis.text.y.right = element_text(
            margin = margin(l = 5),
            hjust = 0
        ),
        
        # Axis ticks
        axis.ticks = element_line(
            color = "#34495E",
            size = 0.3
        ),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(0.15, "cm"),
        
        # Axis lines
        axis.line = element_line(
            color = "#34495E",
            size = 0.5,
            lineend = "square"
        ),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        
        # Legend
        legend.background = element_rect(
            fill = "white",
            color = "#BDC3C7",
            size = 0.5
        ),
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
        legend.spacing = unit(0.5, "cm"),
        legend.spacing.x = NULL,
        legend.spacing.y = NULL,
        legend.key = element_rect(
            fill = "white",
            color = NA
        ),
        legend.key.size = unit(1.2, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(
            size = base_size * 0.9
        ),
        legend.text.align = NULL,
        legend.title = element_text(
            face = "bold",
            size = base_size * 1,
            hjust = 0
        ),
        legend.title.align = NULL,
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box = NULL,
        legend.box.margin = margin(),
        legend.box.background = element_blank(),
        legend.box.spacing = unit(0.5, "cm"),
        
        # Panel (plot area)
        panel.background = element_rect(
            fill = "white",
            color = NA
        ),
        panel.border = element_blank(),
        panel.grid = element_line(color = "#BDC3C7"),
        panel.grid.major = element_line(
            color = "#BDC3C7",
            size = 0.3
        ),
        panel.grid.minor = element_line(
            color = "#ECF0F1",
            size = 0.2
        ),
        panel.grid.major.x = element_line(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.x = element_line(),
        panel.grid.minor.y = element_line(),
        panel.spacing = unit(0.5, "cm"),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        panel.ontop = FALSE,
        
        # Facet strips
        strip.background = element_rect(
            fill = "#34495E",
            color = NA
        ),
        strip.background.x = element_rect(),
        strip.background.y = element_rect(),
        strip.placement = "inside",
        strip.text = element_text(
            color = "white",
            face = "bold",
            size = base_size * 1,
            margin = margin(t = 5, r = 5, b = 5, l = 5)
        ),
        strip.text.x = element_text(),
        strip.text.y = element_text(angle = -90),
        strip.switch.pad.grid = unit(0.5, "cm"),
        strip.switch.pad.wrap = unit(0.5, "cm"),
        
        # Plot background and margins
        plot.background = element_rect(
            fill = "#F8F9F9",
            color = NA
        ),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        
        # Overall properties
        aspect.ratio = NULL,
        
        # Complete theme
        complete = TRUE
    )
}

# Test it out
ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
    geom_point(size = 3) +
    labs(
        title = "Built from Scratch",
        subtitle = "Horsepower vs MPG by Cylinder",
        x = "Miles per Gallon",
        y = "Horsepower",
        color = "Cylinders"
    ) +
    theme_custom_scratch()
