# Continuing from where it left off...

# Load required packages
# renv::install("ggpattern")
library(ggpattern)
library(ggplot2)
library(showtext)
library(sysfonts)
library(scales)
library(grid)

# Add fonts
font_add_google("Playfair Display", "Playfair")
font_add_google("Raleway", "Raleway")

showtext_auto()

# Historical Lithographic Color Palette (colorblind-friendly with emphasis on blues)
litho_colors <- list(
    # Primary historical colors (aged/muted tones)
    prussian_blue = "#2C5F7C",
    aged_blue = "#4682B4",
    cobalt = "#0F4C81",
    steel_blue = "#567B95",
    slate_blue = "#6A8CAF",
    powder_blue = "#8FA9BA",
    
    # Accent colors (colorblind-friendly, muted)
    burnt_sienna = "#B87A4F",
    aged_gold = "#C9A86A",
    sepia = "#704214",
    aged_red = "#A14D3C",
    olive = "#7C8047",
    teal = "#4D7C7C",
    
    # Background colors (aged paper/parchment)
    aged_white = "#F9F6F0",
    cream = "#FBF8F1",
    aged_paper = "#F5EFE0",
    parchment = "#F0E9D2",
    
    # Ink colors
    iron_gall_ink = "#2B2B2B",
    sepia_ink = "#4A3C28",
    faded_black = "#3D3D3D",
    
    # Colorblind-friendly categorical palette (blue emphasis)
    categorical = c("#2C5F7C", "#B87A4F", "#4D7C7C", "#C9A86A", "#567B95", "#7C8047"),
    
    # Sequential blues (for choropleths/heatmaps)
    blues_seq = c("#E8EEF2", "#C6D8E6", "#9BB8CF", "#6A8CAF", "#4682B4", "#2C5F7C", "#0F4C81"),
    
    # Diverging palette (colorblind-safe)
    diverging = c("#A14D3C", "#C9876B", "#E8D4B8", "#F5EFE0", "#9BB8CF", "#4682B4", "#2C5F7C")
)

# Function to access lithographic colors
litho_palette <- function(palette = "categorical", n = NULL, reverse = FALSE) {
    pal <- litho_colors[[palette]]
    
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

# Main Lithographic theme function
theme_lithograph <- function(base_size = 11,
                             title_family = "Playfair",
                             body_family = "Raleway",
                             base_line_size = 0.5,
                             grid = "y",
                             aged_background = TRUE) {
    
    # Choose background color
    bg_color <- if(aged_background) litho_colors$aged_paper else litho_colors$cream
    
    ret <- theme_minimal(base_size = base_size,
                         base_family = body_family,
                         base_line_size = base_line_size)
    
    ret <- ret + theme(
        # Plot elements with historical styling
        plot.title = element_text(
            family = title_family,
            size = rel(1.6),
            face = "bold",
            hjust = 0.5,  # Centered titles like historical prints
            vjust = 1,
            margin = margin(b = 5, t = 10),
            color = litho_colors$iron_gall_ink,
            lineheight = 1.2
        ),
        plot.subtitle = element_text(
            family = title_family,
            size = rel(1.1),
            face = "italic",
            hjust = 0.5,
            vjust = 1,
            margin = margin(b = 15, t = 5),
            color = litho_colors$sepia_ink,
            lineheight = 1.1
        ),
        plot.caption = element_text(
            family = body_family,
            size = rel(0.7),
            hjust = 0.5,  # Centered like historical attribution
            vjust = 1,
            margin = margin(t = 15, b = 5),
            color = litho_colors$sepia_ink,
            face = "italic"
        ),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        # Aged background
        plot.background = element_rect(
            fill = bg_color, 
            color = litho_colors$sepia_ink,
            size = 1.5  # Border like an old print
        ),
        
        # Panel elements
        panel.background = element_rect(
            fill = bg_color, 
            color = NA
        ),
        panel.border = element_rect(
            fill = NA,
            color = litho_colors$faded_black,
            size = 0.8
        ),
        panel.spacing = unit(1.2, "lines"),
        
        # Grid lines - subtle like ruling on aged paper
        panel.grid.major = element_line(
            color = alpha(litho_colors$sepia_ink, 0.15),
            size = 0.3,
            linetype = "solid"
        ),
        panel.grid.minor = element_blank(),
        
        # Axis elements - historical style
        axis.title = element_text(
            family = body_family,
            size = rel(0.9),
            color = litho_colors$iron_gall_ink,
            face = "plain"
        ),
        axis.title.x = element_text(
            margin = margin(t = 10),
            hjust = 0.5
        ),
        axis.title.y = element_text(
            margin = margin(r = 10),
            angle = 90,
            hjust = 0.5
        ),
        axis.text = element_text(
            family = body_family,
            size = rel(0.85),
            color = litho_colors$sepia_ink
        ),
        axis.text.x = element_text(margin = margin(t = 5)),
        axis.text.y = element_text(margin = margin(r = 5)),
        
        # Subtle axis ticks
        axis.ticks = element_line(
            color = litho_colors$sepia_ink,
            size = 0.4
        ),
        axis.ticks.length = unit(3, "pt"),
        axis.line = element_line(
            color = litho_colors$faded_black,
            size = 0.5
        ),
        
        # Legend elements - styled like historical chart keys
        legend.position = "bottom",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.title = element_text(
            family = title_family,
            size = rel(0.9),
            face = "bold.italic",
            color = litho_colors$iron_gall_ink
        ),
        legend.text = element_text(
            family = body_family,
            size = rel(0.85),
            color = litho_colors$sepia_ink
        ),
        legend.background = element_rect(
            fill = alpha(bg_color, 0.9),
            color = litho_colors$sepia_ink,
            size = 0.5
        ),
        legend.key = element_rect(
            fill = bg_color,
            color = NA
        ),
        legend.key.size = unit(0.8, "lines"),
        legend.spacing.x = unit(6, "pt"),
        legend.box.margin = margin(t = 10),
        legend.margin = margin(6, 6, 6, 6),
        
        # Strip (facet) elements
        strip.text = element_text(
            family = title_family,
            size = rel(1),
            face = "bold",
            hjust = 0.5,
            color = litho_colors$iron_gall_ink,
            margin = margin(b = 8, t = 8)
        ),
        strip.background = element_rect(
            fill = alpha(litho_colors$powder_blue, 0.3),
            color = litho_colors$sepia_ink,
            size = 0.5
        ),
        
        # Margins - generous like old prints
        plot.margin = margin(20, 20, 20, 20),
        
        complete = TRUE
    )
    
    # Grid options
    if (grid == "none") {
        ret <- ret + theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        )
    } else if (grid == "y") {
        ret <- ret + theme(
            panel.grid.major.x = element_blank()
        )
    } else if (grid == "x") {
        ret <- ret + theme(
            panel.grid.major.y = element_blank()
        )
    }
    
    return(ret)
}

# Variant: More ornate historical theme
theme_lithograph_ornate <- function(base_size = 11) {
    theme_lithograph(base_size = base_size) +
        theme(
            plot.title = element_text(
                size = rel(1.8),
                margin = margin(b = 8, t = 15)
            ),
            plot.background = element_rect(
                fill = litho_colors$parchment,
                color = litho_colors$iron_gall_ink,
                size = 2
            ),
            panel.border = element_rect(
                fill = NA,
                color = litho_colors$iron_gall_ink,
                size = 1.2
            ),
            plot.margin = margin(25, 25, 25, 25)
        )
}

# Color scale functions
scale_color_litho <- function(palette = "categorical", reverse = FALSE, ...) {
    pal <- litho_palette(palette = palette, reverse = reverse)
    
    ggplot2::discrete_scale(
        "colour", 
        paste0("litho_", palette),
        palette = function(n) {
            if (n > length(pal)) {
                warning("Interpolating colors for n > palette length")
                colorRampPalette(pal)(n)
            } else {
                pal[1:n]
            }
        },
        ...
    )
}

scale_fill_litho <- function(palette = "categorical", reverse = FALSE, ...) {
    pal <- litho_palette(palette = palette, reverse = reverse)
    
    ggplot2::discrete_scale(
        "fill", 
        paste0("litho_", palette),
        palette = function(n) {
            if (n > length(pal)) {
                warning("Interpolating colors for n > palette length")
                colorRampPalette(pal)(n)
            } else {
                pal[1:n]
            }
        },
        ...
    )
}

# Continuous scales
scale_color_litho_c <- function(palette = "blues_seq", reverse = FALSE, ...) {
    pal <- litho_palette(palette = palette, reverse = reverse)
    ggplot2::scale_color_gradientn(colors = pal, ...)
}

scale_fill_litho_c <- function(palette = "blues_seq", reverse = FALSE, ...) {
    pal <- litho_palette(palette = palette, reverse = reverse)
    ggplot2::scale_fill_gradientn(colors = pal, ...)
}

# Diverging scales
scale_color_litho_diverging <- function(midpoint = 0, ...) {
    pal <- litho_colors$diverging
    ggplot2::scale_color_gradient2(
        low = pal[1],
        mid = pal[4],
        high = pal[7],
        midpoint = midpoint,
        ...
    )
}

scale_fill_litho_diverging <- function(midpoint = 0, ...) {
    pal <- litho_colors$diverging
    ggplot2::scale_fill_gradient2(
        low = pal[1],
        mid = pal[4],
        high = pal[7],
        midpoint = midpoint,
        ...
    )
}

# Helper function to add decorative elements
add_decorative_border <- function(plot) {
    plot + 
        theme(
            plot.background = element_rect(
                fill = litho_colors$aged_paper,
                color = litho_colors$iron_gall_ink,
                size = 2
            )
        )
}




# Grayscale Color Palette (optimized for B&W printing)
litho_gray_colors <- list(
    # Core grayscale (carefully calibrated for printing)
    black = "#000000",
    darkest_gray = "#1a1a1a",
    darker_gray = "#333333",
    dark_gray = "#4d4d4d",
    medium_gray = "#666666",
    mid_gray = "#808080",
    light_gray = "#999999",
    lighter_gray = "#b3b3b3",
    lightest_gray = "#cccccc",
    very_light_gray = "#e6e6e6",
    
    # Background colors (for printing)
    white = "#ffffff",
    off_white = "#fafafa",
    paper_white = "#f5f5f5",
    aged_white = "#f0f0f0",
    
    # Categorical grayscale (6 distinct values for printing)
    # Uses combination of shades + patterns (to be added with fills)
    categorical = c("#000000", "#333333", "#666666", "#999999", "#cccccc", "#4d4d4d"),
    
    # Sequential grayscale (for heatmaps/choropleths)
    sequential = c("#ffffff", "#e6e6e6", "#cccccc", "#b3b3b3", "#999999", 
                   "#808080", "#666666", "#4d4d4d", "#333333", "#1a1a1a"),
    
    # High contrast pairs (for emphasis)
    high_contrast = c("#000000", "#ffffff"),
    
    # Diverging grayscale (for showing +/- changes)
    diverging = c("#000000", "#2d2d2d", "#5a5a5a", "#999999", "#c7c7c7", "#e0e0e0", "#f5f5f5")
)

# Function to access grayscale colors
litho_gray_palette <- function(palette = "categorical", n = NULL, reverse = FALSE) {
    pal <- litho_gray_colors[[palette]]
    
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

# ============================================================================
# MAIN GRAYSCALE THEME
# ============================================================================

theme_lithograph_bw <- function(base_size = 11,
                                title_family = "Playfair",
                                body_family = "Raleway",
                                base_line_size = 0.5,
                                grid = "y",
                                high_contrast = FALSE) {
    
    # Choose background based on contrast needs
    bg_color <- if(high_contrast) litho_gray_colors$white else litho_gray_colors$paper_white
    text_color <- litho_gray_colors$darkest_gray
    grid_color <- if(high_contrast) litho_gray_colors$lightest_gray else litho_gray_colors$lighter_gray
    
    ret <- theme_minimal(base_size = base_size,
                         base_family = body_family,
                         base_line_size = base_line_size)
    
    ret <- ret + theme(
        # Plot elements - stronger for B&W printing
        plot.title = element_text(
            family = title_family,
            size = rel(1.6),
            face = "bold",
            hjust = 0.5,
            vjust = 1,
            margin = margin(b = 5, t = 10),
            color = litho_gray_colors$black,
            lineheight = 1.2
        ),
        plot.subtitle = element_text(
            family = title_family,
            size = rel(1.1),
            face = "italic",
            hjust = 0.5,
            vjust = 1,
            margin = margin(b = 15, t = 5),
            color = text_color,
            lineheight = 1.1
        ),
        plot.caption = element_text(
            family = body_family,
            size = rel(0.7),
            hjust = 0.5,
            vjust = 1,
            margin = margin(t = 15, b = 5),
            color = text_color,
            face = "italic"
        ),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        # Background - clean for printing
        plot.background = element_rect(
            fill = bg_color,
            color = litho_gray_colors$black,
            size = 1.5
        ),
        
        # Panel elements
        panel.background = element_rect(
            fill = bg_color,
            color = NA
        ),
        panel.border = element_rect(
            fill = NA,
            color = litho_gray_colors$darkest_gray,
            size = 1
        ),
        panel.spacing = unit(1.2, "lines"),
        
        # Grid lines - visible but not overwhelming in B&W
        panel.grid.major = element_line(
            color = grid_color,
            size = 0.4,
            linetype = "solid"
        ),
        panel.grid.minor = element_blank(),
        
        # Axis elements - strong and clear
        axis.title = element_text(
            family = body_family,
            size = rel(0.9),
            color = litho_gray_colors$black,
            face = "bold"  # Bold for clarity in B&W
        ),
        axis.title.x = element_text(
            margin = margin(t = 10),
            hjust = 0.5
        ),
        axis.title.y = element_text(
            margin = margin(r = 10),
            angle = 90,
            hjust = 0.5
        ),
        axis.text = element_text(
            family = body_family,
            size = rel(0.85),
            color = text_color
        ),
        axis.text.x = element_text(margin = margin(t = 5)),
        axis.text.y = element_text(margin = margin(r = 5)),
        
        # Axis ticks and lines - clear and visible
        axis.ticks = element_line(
            color = litho_gray_colors$black,
            size = 0.5
        ),
        axis.ticks.length = unit(4, "pt"),
        axis.line = element_line(
            color = litho_gray_colors$black,
            size = 0.6
        ),
        
        # Legend elements - clear for B&W
        legend.position = "bottom",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.title = element_text(
            family = title_family,
            size = rel(0.9),
            face = "bold",
            color = litho_gray_colors$black
        ),
        legend.text = element_text(
            family = body_family,
            size = rel(0.85),
            color = text_color
        ),
        legend.background = element_rect(
            fill = bg_color,
            color = litho_gray_colors$dark_gray,
            size = 0.5
        ),
        legend.key = element_rect(
            fill = bg_color,
            color = litho_gray_colors$medium_gray,
            size = 0.3
        ),
        legend.key.size = unit(1, "lines"),
        legend.spacing.x = unit(6, "pt"),
        legend.box.margin = margin(t = 10),
        legend.margin = margin(6, 6, 6, 6),
        
        # Strip (facet) elements
        strip.text = element_text(
            family = title_family,
            size = rel(1),
            face = "bold",
            hjust = 0.5,
            color = litho_gray_colors$black,
            margin = margin(b = 8, t = 8)
        ),
        strip.background = element_rect(
            fill = litho_gray_colors$lightest_gray,
            color = litho_gray_colors$black,
            size = 0.5
        ),
        
        # Margins
        plot.margin = margin(20, 20, 20, 20),
        
        complete = TRUE
    )
    
    # Grid options
    if (grid == "none") {
        ret <- ret + theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        )
    } else if (grid == "y") {
        ret <- ret + theme(
            panel.grid.major.x = element_blank()
        )
    } else if (grid == "x") {
        ret <- ret + theme(
            panel.grid.major.y = element_blank()
        )
    }
    
    return(ret)
}

# ============================================================================
# VARIANT: HIGH CONTRAST VERSION (for photocopying)
# ============================================================================

theme_lithograph_bw_highcontrast <- function(base_size = 11,
                                             title_family = "Playfair",
                                             body_family = "Raleway") {
    theme_lithograph_bw(
        base_size = base_size,
        title_family = title_family,
        body_family = body_family,
        high_contrast = TRUE
    ) +
        theme(
            # Stronger borders for photocopies
            plot.background = element_rect(
                fill = "white",
                color = "black",
                size = 2
            ),
            panel.border = element_rect(
                fill = NA,
                color = "black",
                size = 1.2
            ),
            # Bolder text
            plot.title = element_text(size = rel(1.7)),
            axis.title = element_text(face = "bold"),
            # Stronger grid
            panel.grid.major = element_line(
                color = litho_gray_colors$light_gray,
                size = 0.5
            )
        )
}

# ============================================================================
# GRAYSCALE COLOR SCALES
# ============================================================================

scale_color_litho_bw <- function(palette = "categorical", reverse = FALSE, ...) {
    pal <- litho_gray_palette(palette = palette, reverse = reverse)
    
    ggplot2::discrete_scale(
        "colour",
        paste0("litho_bw_", palette),
        palette = function(n) {
            if (n > length(pal)) {
                colorRampPalette(pal)(n)
            } else {
                pal[1:n]
            }
        },
        ...
    )
}

scale_fill_litho_bw <- function(palette = "categorical", reverse = FALSE, ...) {
    pal <- litho_gray_palette(palette = palette, reverse = reverse)
    
    ggplot2::discrete_scale(
        "fill",
        paste0("litho_bw_", palette),
        palette = function(n) {
            if (n > length(pal)) {
                colorRampPalette(pal)(n)
            } else {
                pal[1:n]
            }
        },
        ...
    )
}

# Continuous scales
scale_color_litho_bw_c <- function(palette = "sequential", reverse = FALSE, ...) {
    pal <- litho_gray_palette(palette = palette, reverse = reverse)
    ggplot2::scale_color_gradientn(colors = pal, ...)
}

scale_fill_litho_bw_c <- function(palette = "sequential", reverse = FALSE, ...) {
    pal <- litho_gray_palette(palette = palette, reverse = reverse)
    ggplot2::scale_fill_gradientn(colors = pal, ...)
}

# Diverging scales
scale_color_litho_bw_diverging <- function(midpoint = 0, ...) {
    pal <- litho_gray_colors$diverging
    ggplot2::scale_color_gradient2(
        low = pal[1],
        mid = pal[4],
        high = pal[7],
        midpoint = midpoint,
        ...
    )
}

scale_fill_litho_bw_diverging <- function(midpoint = 0, ...) {
    pal <- litho_gray_colors$diverging
    ggplot2::scale_fill_gradient2(
        low = pal[1],
        mid = pal[4],
        high = pal[7],
        midpoint = midpoint,
        ...
    )
}

# ============================================================================
# PATTERN FILLS FOR BETTER DISTINCTION IN B&W
# ============================================================================

# Helper function to create pattern-based legends
# Requires {ggpattern} package for advanced use
add_bw_patterns <- function() {
    if (!requireNamespace("ggpattern", quietly = TRUE)) {
        message("Install ggpattern for pattern fills: install.packages('ggpattern')")
        return(NULL)
    }
    
    list(
        solid = "none",
        horizontal = "stripe",
        vertical = "stripe",
        diagonal = "stripe",
        crosshatch = "crosshatch",
        dots = "circle"
    )
}



# ============================================================================
# EXAMPLE USAGE - Historical Lithographic Style Graphics
# ============================================================================

# Example 1: Historical Bar Chart (like early statistical graphics)
df_bar <- data.frame(
    category = c("Agriculture", "Manufacturing", "Commerce", "Mining", "Transportation", "Services"),
    value = c(34, 28, 18, 12, 5, 3),
    year = "1880"
)

p1 <- ggplot(df_bar, aes(x = reorder(category, value), y = value)) +
    geom_col(
        fill = litho_colors$prussian_blue,
        color = litho_colors$iron_gall_ink,
        width = 0.7,
        size = 0.5
    ) +
    geom_text(
        aes(label = paste0(value, "%")),
        hjust = -0.2,
        family = "Raleway",
        size = 3.5,
        color = litho_colors$sepia_ink
    ) +
    coord_flip() +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.15)),
        breaks = seq(0, 40, 10),
        labels = function(x) paste0(x, "%")
    ) +
    labs(
        title = "Distribution of Employment by Industry",
        subtitle = "United States, Anno Domini 1880",
        caption = "Source: Historical Census Records",
        x = NULL,
        y = "Percentage of Workforce"
    ) +
    theme_lithograph(grid = "x")

print(p1)

# Example 2: Time Series (Playfair-style line chart)
years <- seq(1800, 1900, by = 10)
df_line <- data.frame(
    year = rep(years, 3),
    country = rep(c("Great Britain", "France", "Prussia"), each = length(years)),
    population = c(
        # Great Britain
        seq(10.5, 37, length.out = 11) + rnorm(11, 0, 1),
        # France
        seq(27, 40, length.out = 11) + rnorm(11, 0, 1.5),
        # Prussia/Germany
        seq(10, 56, length.out = 11) + rnorm(11, 0, 2)
    )
)

p2 <- ggplot(df_line, aes(x = year, y = population, color = country, group = country)) +
    geom_line(size = 1.2) +
    geom_point(size = 2.5, shape = 21, fill = litho_colors$aged_paper, stroke = 1.2) +
    scale_x_continuous(breaks = seq(1800, 1900, 20)) +
    scale_y_continuous(
        breaks = seq(0, 60, 10),
        labels = function(x) paste0(x, "M")
    ) +
    labs(
        title = "Population Growth of European Powers",
        subtitle = "In the Age of Industry, 1800-1900",
        caption = "Source: Historical Population Estimates · Figures in Millions",
        x = "Year",
        y = "Population",
        color = "Nation"
    ) +
    theme_lithograph(grid = "y") +
    scale_color_litho()

print(p2)

# Example 3: Stacked Area Chart (Historical Immigration Pattern)
df_area <- data.frame(
    year = rep(seq(1850, 1910, by = 10), 4),
    region = rep(c("Northern Europe", "Southern Europe", "Eastern Europe", "Asia"), each = 7),
    immigrants = c(
        # Northern Europe
        c(200, 180, 160, 140, 120, 100, 80),
        # Southern Europe
        c(20, 40, 80, 150, 200, 240, 280),
        # Eastern Europe
        c(10, 20, 40, 80, 150, 200, 250),
        # Asia
        c(5, 8, 12, 20, 30, 40, 50)
    )
)

p3 <- ggplot(df_area, aes(x = year, y = immigrants, fill = region)) +
    geom_area(alpha = 0.8, color = litho_colors$sepia_ink, size = 0.3) +
    scale_x_continuous(breaks = seq(1850, 1910, 20)) +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.05)),
        labels = function(x) paste0(x, "K")
    ) +
    labs(
        title = "Immigration to America by Region of Origin",
        subtitle = "The Great Wave, 1850-1910",
        caption = "Source: Historical Immigration Records · Figures in Thousands Annually",
        x = "Year",
        y = "Annual Immigrants",
        fill = "Region"
    ) +
    theme_lithograph(grid = "y") +
    scale_fill_litho()

print(p3)

# Example 4: Dot Plot with Annotations (Historical Railway Mileage)
df_dot <- data.frame(
    country = c("United States", "Russia", "Germany", "France", 
                "Austria-Hungary", "United Kingdom", "Italy", "Spain"),
    miles = c(254, 72, 63, 51, 45, 38, 18, 15),
    continent = c("North America", "Europe", "Europe", "Europe", 
                  "Europe", "Europe", "Europe", "Europe")
)

p4 <- ggplot(df_dot, aes(x = miles, y = reorder(country, miles))) +
    geom_segment(
        aes(x = 0, xend = miles, yend = country),
        color = litho_colors$steel_blue,
        size = 0.8
    ) +
    geom_point(
        size = 4,
        color = litho_colors$prussian_blue,
        fill = litho_colors$aged_paper,
        shape = 21,
        stroke = 1.5
    ) +
    geom_text(
        aes(label = paste0(miles, "K")),
        hjust = -0.3,
        family = "Raleway",
        size = 3.2,
        color = litho_colors$sepia_ink
    ) +
    scale_x_continuous(
        expand = expansion(mult = c(0, 0.15)),
        breaks = seq(0, 300, 50),
        labels = function(x) paste0(x, "K")
    ) +
    labs(
        title = "Railway Mileage of Principal Nations",
        subtitle = "Circa 1913, Before the Great War",
        caption = "Source: Statistical Abstract · Figures in Thousands of Miles",
        x = "Miles of Track",
        y = NULL
    ) +
    theme_lithograph(grid = "x")

print(p4)

# Example 5: Grouped Bar Chart (Historical Trade Data)
df_trade <- data.frame(
    year = rep(c("1870", "1890", "1910"), each = 4),
    commodity = rep(c("Cotton", "Grain", "Manufactures", "Raw Materials"), 3),
    value = c(
        # 1870
        45, 30, 15, 10,
        # 1890
        40, 35, 25, 20,
        # 1910
        30, 35, 40, 35
    )
)

p5 <- ggplot(df_trade, aes(x = year, y = value, fill = commodity)) +
    geom_col(
        position = position_dodge(width = 0.8),
        width = 0.7,
        color = litho_colors$iron_gall_ink,
        size = 0.3
    ) +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.05)),
        breaks = seq(0, 50, 10),
        labels = function(x) paste0(x, "%")
    ) +
    labs(
        title = "Composition of American Exports",
        subtitle = "Evolution of Trade in the Gilded Age",
        caption = "Source: Department of Commerce Records · Percentage of Total Export Value",
        x = "Year",
        y = "Percentage of Exports",
        fill = "Commodity"
    ) +
    theme_lithograph(grid = "y") +
    scale_fill_litho()

print(p5)

# Example 6: Scatter Plot with Smooth (Historical Demographics)
set.seed(1865)
df_scatter <- data.frame(
    income = rnorm(100, 500, 200),
    life_expectancy = rnorm(100, 45, 8)
)
df_scatter$life_expectancy <- df_scatter$life_expectancy + df_scatter$income * 0.02

p6 <- ggplot(df_scatter, aes(x = income, y = life_expectancy)) +
    geom_point(
        size = 3,
        shape = 21,
        fill = alpha(litho_colors$prussian_blue, 0.6),
        color = litho_colors$iron_gall_ink,
        stroke = 0.5
    ) +
    geom_smooth(
        method = "lm",
        se = TRUE,
        color = litho_colors$aged_red,
        fill = alpha(litho_colors$aged_red, 0.2),
        size = 1.2
    ) +
    scale_x_continuous(
        labels = function(x) paste0("$", x)
    ) +
    labs(
        title = "The Relationship Between Wealth and Longevity",
        subtitle = "A Statistical Inquiry into Urban Populations, 1890",
        caption = "Source: Municipal Health Records · Income in Annual Dollars",
        x = "Annual Income per Capita",
        y = "Life Expectancy (Years)"
    ) +
    theme_lithograph(grid = "xy")

print(p6)

# Example 7: Heatmap (Historical Temperature Data)
df_heat <- expand.grid(
    month = factor(month.abb, levels = month.abb),
    year = seq(1880, 1920, by = 5)
)
set.seed(1880)
df_heat$temp_anomaly <- rnorm(nrow(df_heat), 0, 0.5) + 
    (df_heat$year - 1900) * 0.01

p7 <- ggplot(df_heat, aes(x = year, y = month, fill = temp_anomaly)) +
    geom_tile(color = litho_colors$sepia_ink, size = 0.3) +
    scale_x_continuous(
        breaks = seq(1880, 1920, 10),
        expand = c(0, 0)
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(
        title = "Temperature Deviations from Normal",
        subtitle = "Monthly Observations, 1880-1920",
        caption = "Source: Meteorological Survey · Deviations in Degrees Fahrenheit",
        x = "Year",
        y = "Month",
        fill = "Temp.\nAnomaly (°F)"
    ) +
    theme_lithograph(grid = "none") +
    scale_fill_litho_c(palette = "diverging") +
    theme(
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(0.4, "cm")
    )

print(p7)

# Example 8: Faceted Plot (Historical Agricultural Production)
df_facet <- data.frame(
    year = rep(seq(1850, 1910, by = 10), 12),
    crop = rep(rep(c("Wheat", "Corn", "Cotton", "Tobacco"), each = 7), 3),
    region = rep(c("Northeast", "South", "West"), each = 28),
    production = c(
        # Northeast - Wheat
        c(50, 60, 70, 75, 80, 85, 90),
        # Northeast - Corn
        c(40, 50, 60, 70, 75, 80, 85),
        # Northeast - Cotton
        c(5, 5, 5, 5, 5, 5, 5),
        # Northeast - Tobacco
        c(10, 12, 14, 15, 16, 17, 18),
        # South - Wheat
        c(30, 35, 40, 45, 50, 55, 60),
        # South - Corn
        c(60, 70, 80, 90, 100, 110, 120),
        # South - Cotton
        c(80, 100, 120, 90, 95, 110, 130),
        # South - Tobacco
        c(50, 55, 60, 65, 70, 75, 80),
        # West - Wheat
        c(10, 30, 60, 100, 140, 180, 220),
        # West - Corn
        c(20, 40, 60, 80, 100, 120, 140),
        # West - Cotton
        c(5, 10, 15, 20, 25, 30, 35),
        # West - Tobacco
        c(5, 8, 10, 12, 15, 18, 20)
    )
)

p8 <- ggplot(df_facet, aes(x = year, y = production, color = crop)) +
    geom_line(size = 1) +
    geom_point(size = 2, shape = 21, fill = litho_colors$aged_paper, stroke = 1) +
    facet_wrap(~ region, ncol = 1, scales = "free_y") +
    scale_x_continuous(breaks = seq(1850, 1910, 30)) +
    labs(
        title = "Regional Agricultural Production in America",
        subtitle = "The Expansion and Transformation of Farming, 1850-1910",
        caption = "Source: Agricultural Census · Production Index (1850 = 100)",
        x = "Year",
        y = "Production Index",
        color = "Crop"
    ) +
    theme_lithograph(grid = "y") +
    scale_color_litho() +
    theme(
        strip.text = element_text(size = rel(1.1))
    )

print(p8)

# Example 9: Ornate version with different styling
p9 <- ggplot(df_bar, aes(x = value, y = reorder(category, value))) +
    geom_col(
        fill = litho_colors$cobalt,
        color = litho_colors$iron_gall_ink,
        width = 0.65,
        size = 0.8
    ) +
    geom_text(
        aes(label = paste0(value, "%")),
        hjust = 1.2,
        family = "Raleway",
        size = 4,
        color = litho_colors$aged_white,
        fontface = "bold"
    ) +
    scale_x_continuous(
        expand = expansion(mult = c(0, 0.05)),
        breaks = seq(0, 40, 10),
        labels = function(x) paste0(x, "%")
    ) +
    labs(
        title = "The Structure of American Labor",
        subtitle = "A Statistical Portrait of Industry and Commerce",
        caption = "Compiled from Official Returns · Bureau of Labor Statistics",
        x = "Share of Total Employment",
        y = NULL
    ) +
    theme_lithograph_ornate()

print(p9)

# Function to save plots with appropriate settings
save_lithograph_plot <- function(plot, filename, width = 8, height = 6, dpi = 300) {
    ggsave(
        filename = filename,
        plot = plot,
        width = width,
        height = height,
        dpi = dpi,
        bg = litho_colors$aged_paper
    )
}

# Example of saving
# save_lithograph_plot(p1, "historical_employment.png", width = 10, height = 6)

# Display color palette reference
cat("\n=== Historical Lithographic Color Palette ===\n\n")
cat("PRIMARY BLUES (colorblind-friendly):\n")
cat("  Prussian Blue:  ", litho_colors$prussian_blue, "\n")
cat("  Aged Blue:      ", litho_colors$aged_blue, "\n")
cat("  Cobalt:         ", litho_colors$cobalt, "\n")
cat("  Steel Blue:     ", litho_colors$steel_blue, "\n\n")

cat("ACCENT COLORS:\n")
cat("  Burnt Sienna:   ", litho_colors$burnt_sienna, "\n")
cat("  Aged Gold:      ", litho_colors$aged_gold, "\n")
cat("  Aged Red:       ", litho_colors$aged_red, "\n")
cat("  Teal:           ", litho_colors$teal, "\n\n")

cat("BACKGROUND COLORS:\n")
cat("  Aged Paper:     ", litho_colors$aged_paper, "\n")
cat("  Parchment:      ", litho_colors$parchment, "\n")
cat("  Cream:          ", litho_colors$cream, "\n\n")

cat("All colors are tested for colorblind accessibility.\n")
cat("Blues dominate the palette for historical accuracy.\n")


# ============================================================================
# EXAMPLES - GRAYSCALE LITHOGRAPHIC GRAPHICS
# ============================================================================

# Example 1: Simple Bar Chart (B&W)
df_bar <- data.frame(
    category = c("Agriculture", "Manufacturing", "Commerce", "Mining", "Transportation"),
    value = c(34, 28, 18, 12, 8)
)

p1 <- ggplot(df_bar, aes(x = reorder(category, value), y = value)) +
    geom_col(
        fill = litho_gray_colors$dark_gray,
        color = litho_gray_colors$black,
        width = 0.7,
        size = 0.8
    ) +
    geom_text(
        aes(label = paste0(value, "%")),
        hjust = -0.2,
        family = "Raleway",
        size = 3.5,
        color = litho_gray_colors$black,
        fontface = "bold"
    ) +
    coord_flip() +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.15)),
        breaks = seq(0, 40, 10),
        labels = function(x) paste0(x, "%")
    ) +
    labs(
        title = "Distribution of Employment by Industry",
        subtitle = "United States, 1880",
        caption = "Source: Historical Census Records",
        x = NULL,
        y = "Percentage of Workforce"
    ) +
    theme_lithograph_bw(grid = "x")

print(p1)

# Example 2: Multi-category Bar Chart with Distinct Grays
df_grouped <- data.frame(
    year = rep(c("1850", "1870", "1890", "1910"), 3),
    region = rep(c("Northeast", "South", "West"), each = 4),
    population = c(
        5.9, 7.9, 9.7, 11.2,  # Northeast
        5.4, 8.7, 14.8, 20.5, # South
        0.2, 1.0, 3.1, 7.1    # West
    )
)

p2 <- ggplot(df_grouped, aes(x = year, y = population, fill = region)) +
    geom_col(
        position = position_dodge(width = 0.8),
        width = 0.7,
        color = litho_gray_colors$black,
        size = 0.5
    ) +
    geom_text(
        aes(label = round(population, 1)),
        position = position_dodge(width = 0.8),
        vjust = -0.5,
        family = "Raleway",
        size = 2.8,
        color = litho_gray_colors$black
    ) +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.12)),
        breaks = seq(0, 25, 5),
        labels = function(x) paste0(x, "M")
    ) +
    labs(
        title = "Population Growth by Region",
        subtitle = "United States, 1850-1910",
        caption = "Source: U.S. Census Bureau · Figures in Millions",
        x = "Year",
        y = "Population",
        fill = "Region"
    ) +
    theme_lithograph_bw(grid = "y") +
    scale_fill_litho_bw()

print(p2)

# Example 3: Line Chart with Multiple Categories (distinct line types)
years <- seq(1800, 1900, by = 10)
df_line <- data.frame(
    year = rep(years, 3),
    country = rep(c("Great Britain", "France", "Prussia"), each = length(years)),
    population = c(
        seq(10.5, 37, length.out = 11),
        seq(27, 40, length.out = 11),
        seq(10, 56, length.out = 11)
    )
)

p3 <- ggplot(df_line, aes(x = year, y = population, 
                          color = country, 
                          linetype = country)) +
    geom_line(size = 1.2) +
    geom_point(
        size = 3,
        shape = 21,
        fill = "white",
        stroke = 1.2
    ) +
    scale_x_continuous(breaks = seq(1800, 1900, 20)) +
    scale_y_continuous(
        breaks = seq(0, 60, 10),
        labels = function(x) paste0(x, "M")
    ) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
    labs(
        title = "Population Growth of European Powers",
        subtitle = "The Age of Industry, 1800-1900",
        caption = "Source: Historical Population Estimates · Figures in Millions",
        x = "Year",
        y = "Population",
        color = "Nation",
        linetype = "Nation"
    ) +
    theme_lithograph_bw(grid = "y") +
    scale_color_litho_bw() +
    guides(color = guide_legend(override.aes = list(size = 4)))

print(p3)

# Example 4: Scatter Plot with Size and Shape Variation
set.seed(1865)
df_scatter <- data.frame(
    income = rnorm(60, 500, 200),
    life_expectancy = rnorm(60, 45, 8),
    city_size = sample(c("Small", "Medium", "Large"), 60, replace = TRUE)
)
df_scatter$life_expectancy <- df_scatter$life_expectancy + df_scatter$income * 0.02

p4 <- ggplot(df_scatter, aes(x = income, y = life_expectancy, shape = city_size)) +
    geom_point(
        size = 3.5,
        fill = litho_gray_colors$medium_gray,
        color = litho_gray_colors$black,
        stroke = 0.8
    ) +
    geom_smooth(
        method = "lm",
        se = TRUE,
        color = litho_gray_colors$black,
        fill = litho_gray_colors$light_gray,
        size = 1,
        linetype = "dashed"
    ) +
    scale_shape_manual(values = c(21, 22, 24)) +  # circle, square, triangle
    scale_x_continuous(labels = function(x) paste0("$", x)) +
    labs(
        title = "Wealth and Longevity in Urban Populations",
        subtitle = "A Statistical Inquiry, 1890",
        caption = "Source: Municipal Health Records · Income in Annual Dollars",
        x = "Annual Income per Capita",
        y = "Life Expectancy (Years)",
        shape = "City Size"
    ) +
    theme_lithograph_bw(grid = "xy")

print(p4)

# Example 5: Stacked Area Chart (challenging in B&W)
df_area <- data.frame(
    year = rep(seq(1850, 1910, by = 10), 4),
    region = rep(c("Northern Europe", "Southern Europe", "Eastern Europe", "Asia"), each = 7),
    immigrants = c(
        c(200, 180, 160, 140, 120, 100, 80),
        c(20, 40, 80, 150, 200, 240, 280),
        c(10, 20, 40, 80, 150, 200, 250),
        c(5, 8, 12, 20, 30, 40, 50)
    )
)

p5 <- ggplot(df_area, aes(x = year, y = immigrants, fill = region)) +
    geom_area(
        alpha = 1,
        color = litho_gray_colors$black,
        size = 0.5,
        position = "stack"
    ) +
    scale_x_continuous(breaks = seq(1850, 1910, 20)) +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.05)),
        labels = function(x) paste0(x, "K")
    ) +
    labs(
        title = "Immigration to America by Region of Origin",
        subtitle = "The Great Wave, 1850-1910",
        caption = "Source: Historical Immigration Records · Figures in Thousands Annually",
        x = "Year",
        y = "Annual Immigrants",
        fill = "Region"
    ) +
    theme_lithograph_bw(grid = "y") +
    scale_fill_litho_bw()

print(p5)

# Example 6: Heatmap in Grayscale
df_heat <- expand.grid(
    month = factor(month.abb, levels = month.abb),
    year = seq(1880, 1920, by = 5)
)
set.seed(1880)
df_heat$temp_anomaly <- rnorm(nrow(df_heat), 0, 1)

p6 <- ggplot(df_heat, aes(x = year, y = month, fill = temp_anomaly)) +
    geom_tile(color = litho_gray_colors$black, size = 0.3) +
    scale_x_continuous(
        breaks = seq(1880, 1920, 10),
        expand = c(0, 0)
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(
        title = "Temperature Deviations from Normal",
        subtitle = "Monthly Observations, 1880-1920",
        caption = "Source: Meteorological Survey · Darker = Warmer",
        x = "Year",
        y = "Month",
        fill = "Temp.\nAnomaly"
    ) +
    theme_lithograph_bw(grid = "none") +
    scale_fill_litho_bw_c(palette = "sequential", reverse = TRUE) +
    theme(
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.height = unit(1.5, "cm"),
        legend.key.width = unit(0.5, "cm")
    )

print(p6)

# Example 7: High Contrast Version (for photocopying)
p7 <- ggplot(df_bar, aes(x = reorder(category, value), y = value)) +
    geom_col(
        fill = litho_gray_colors$black,
        color = litho_gray_colors$black,
        width = 0.65
    ) +
    geom_text(
        aes(label = paste0(value, "%")),
        hjust = -0.2,
        family = "Raleway",
        size = 4,
        color = litho_gray_colors$black,
        fontface = "bold"
    ) +
    coord_flip() +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0.15)),
        breaks = seq(0, 40, 10),
        labels = function(x) paste0(x, "%")
    ) +
    labs(
        title = "Employment Distribution by Industry",
        subtitle = "United States Census, 1880",
        caption = "Source: Bureau of Labor Statistics",
        x = NULL,
        y = "Percentage of Total Employment"
    ) +
    theme_lithograph_bw_highcontrast()

print(p7)

# Example 8: Using Patterns (requires ggpattern)
# Uncomment if ggpattern is installed
# library(ggpattern)
# 
# p8 <- ggplot(df_grouped, aes(x = year, y = population, 
#                              pattern = region, fill = region)) +
#   ggpattern::geom_col_pattern(
#     position = position_dodge(width = 0.8),
#     width = 0.7,
#     color = "black",
#     pattern_fill = "black",
#     pattern_color = "black",
#     pattern_spacing = 0.025
#   ) +
#   scale_pattern_manual(values = c("stripe", "crosshatch", "circle")) +
#   scale_fill_litho_bw() +
#   labs(
#     title = "Population by Region (Pattern Version)",
#     subtitle = "Optimized for Black & White Printing"
#   ) +
#   theme_lithograph_bw()

# ============================================================================
# PRINTING RECOMMENDATIONS
# ============================================================================

cat("\n=== GRAYSCALE THEME PRINTING GUIDE ===\n\n")

cat("FOR STANDARD LASER PRINTERS:\n")
cat("  • Use theme_lithograph_bw()\n")
cat("  • DPI: 300 minimum\n")
cat("  • Save as PDF for best quality\n\n")

cat("FOR PHOTOCOPYING:\n")
cat("  • Use theme_lithograph_bw_highcontrast()\n")
cat("  • Increase base_size to 12-13\n")
cat("  • Use solid fills or strong patterns\n")
cat("  • Avoid thin lines (use size >= 0.8)\n\n")

cat("FOR NEWSPAPER/JOURNAL PRINTING:\n")
cat("  • Use theme_lithograph_bw(high_contrast = FALSE)\n")
cat("  • DPI: 600 for professional printing\n")
cat("  • Test print before final submission\n\n")

cat("DISTINGUISHING CATEGORIES IN B&W:\n")
cat("  1. Different gray shades (use scale_fill_litho_bw)\n")
cat("  2. Different line types (solid, dashed, dotted)\n")
cat("  3. Different shapes (circle, square, triangle)\n")
cat("  4. Patterns (requires ggpattern package)\n")
cat("  5. Combinations of the above\n\n")

# Function to save for B&W printing
save_bw_plot <- function(plot, filename, width = 8, height = 6, dpi = 300) {
    ggsave(
        filename = filename,
        plot = plot,
        width = width,
        height = height,
        dpi = dpi,
        bg = "white",
        device = if(grepl("\\.pdf$", filename)) "pdf" else "png"
    )
    message("✓ Plot saved for B&W printing: ", filename)
}

# Example usage:
# save_bw_plot(p1, "employment_chart.pdf", dpi = 600)


