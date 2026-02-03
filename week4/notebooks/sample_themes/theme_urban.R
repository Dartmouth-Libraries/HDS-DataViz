

# Urban Institute Color Palette
urban_colors <- list(
    # Primary colors
    cyan = "#1696d2",
    gray = "#d2d2d2",
    black = "#000000",
    
    # Secondary colors
    yellow = "#fdbf11",
    magenta = "#ec008b",
    green = "#55b748",
    space_gray = "#5c5859",
    red = "#db2b27",
    
    # Shades of cyan
    cyan_shade = c("#CFE8F3", "#A2D4EC", "#73BFE2", "#46ABDB", "#1696D2", "#12719E", "#0a4c6a", "#062635"),
    
    # Shades of gray
    white_shade = c("#F5F5F5","#ECECEC","#E3E3E3","#DCDBDB","#D2D2D2","#9D9D9D","#696969","#353535"),
    
    yellow_shade = c("#FFF2CF","#FCE39E","#FDD870","#FCCB41","#FDBF11","#E88E2D","#CA5800","#843215"),
    
    magenta_shade = c("#F5CBDF","#EB99C2","#E46AA7","#E54096","#EC008B","#AF1F6B","#761548","#351123"),
    
    green_shade = c("#DCEDD9","#BCDEB4","#98CF90","#78C26D","#55B748","#408941","#2C5C2D","#1A2E19"),
    
    gray_shade = c("#D5D5D4","#ADABAC","#848081","#5C5859","#332D2F","#262223","#1A1717","#0E0C0D"),
    
    red_shade = c("#F8D5D4","#F1AAA9","#E9807D","#E25552","#DB2B27","#A4201D","#6E1614","#370B0A"),
    
    # Categorical palette
    categorical = c("#1696d2", "#000000", "#d2d2d2", "#fdbf11", "#ec008b", "#55b748", "#0a4c6a", "#CA5800", "#5c5859", "#db2b27") #
)

urban_palette <- function(palette = "categorical", n = NULL, reverse = FALSE) {
    pal <- urban_colors[[palette]]
    
    if (reverse) pal <- rev(pal)
    
    if (!is.null(n)) {
        if (n <= length(pal)) {
            pal <- pal[1:n]
        } else {
            pal <- colorRampPalette(pal)(n)
        }
    }
    
    return(pal)
}

# Main Urban Institute theme function
theme_urban <- function(base_size = 12,
                        base_family = "Lato",
                        base_line_size = 0.5,
                        base_rect_size = 0.5) {
    
    theme_minimal(base_size = base_size,
                  base_family = base_family,
                  base_line_size = base_line_size,
                  base_rect_size = base_rect_size) +
        theme(
            # Plot elements
            plot.title = element_text(
                family = "Lato Bold",
                size = rel(2),
                face = "bold",
                hjust = 0,
                margin = margin(b = 10),
                color = urban_colors$black
            ),
            plot.subtitle = element_text(
                size = rel(1.5),
                hjust = 0,
                margin = margin(b = 15),
                color = urban_colors$space_gray
            ),
            plot.caption = element_text(
                size = rel(1),
                hjust = 0,
                margin = margin(t = 15),
                color = urban_colors$space_gray
            ),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.background = element_rect(fill = "white", color = NA),
            
            # Panel elements
            panel.grid.major = element_line(color = urban_colors$gray, size = 0.3),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            
            # Axis elements
            axis.title = element_text(
                family = "Lato Bold",
                size = rel(1.2),
                color = urban_colors$black,
                face = "bold"
            ),
            axis.title.x = element_text(margin = margin(t = 10)),
            axis.title.y = element_text(margin = margin(r = 10)),
            axis.text = element_text(
                size = rel(1),
                color = urban_colors$space_gray
            ),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            
            # Legend elements
            legend.position = "right",
            legend.justification = "left",
            legend.title = element_text(
                size = rel(1.1),
                face = "bold",
                color = urban_colors$black
            ),
            legend.text = element_text(
                size = rel(0.8),
                color = urban_colors$space_gray
            ),
            legend.key = element_blank(),
            legend.background = element_blank(),
            
            # Strip (facet) elements
            strip.text = element_text(
                family = "Lato Bold",
                size = rel(1),
                face = "bold",
                hjust = 0,
                color = urban_colors$black
            ),
            strip.background = element_blank(),
            
            # Margins
            plot.margin = margin(15, 15, 15, 15)
        )
}
    
# Color scale functions
scale_color_urban <- function(palette = "categorical", reverse = FALSE, ...) {
    pal <- urban_palette(palette = palette, reverse = reverse)
    
    ggplot2::discrete_scale(
        "colour", 
        paste0("urban_", palette),
        palette = function(n) pal[1:n],
        ...
    )
}

scale_fill_urban <- function(palette = "categorical", reverse = FALSE, ...) {
    pal <- urban_palette(palette = palette, reverse = reverse)
    
    ggplot2::discrete_scale(
        "fill", 
        paste0("urban_", palette),
        palette = function(n) pal[1:n],
        ...
    )
}

# Continuous color scales
scale_color_urban_c <- function(palette = "cyan_shade", reverse = FALSE, ...) {
    pal <- urban_palette(palette = palette, reverse = reverse)
    
    ggplot2::scale_color_gradientn(colors = pal, ...)
}

scale_fill_urban_c <- function(palette = "cyan_shade", reverse = FALSE, ...) {
    pal <- urban_palette(palette = palette, reverse = reverse)
    
    ggplot2::scale_fill_gradientn(colors = pal, ...)
}

# Example usage
sample_df <- data.frame(
    category = c("Category A", "Category B", "Category C", "Category D"),
    value = c(23, 45, 12, 34)
)

sample_p <- ggplot(sample_df, aes(x = reorder(category, value), y = value)) +
    geom_col(fill = urban_colors$cyan, width = 0.7) +
    coord_flip() +
    labs(
        title = "Example with Lato Font",
        subtitle = "Urban Institute styling with proper typography",
        caption = "Source: Example data",
        x = NULL,
        y = "Values"
    ) +
    theme_urban()

print(sample_p)

# Save with proper font rendering
ggsave("urban_plot.png", sample_p, width = 8, height = 6, dpi = 150)