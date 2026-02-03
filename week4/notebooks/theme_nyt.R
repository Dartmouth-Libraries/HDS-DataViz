# ============================================================================
# NEW YORK TIMES GGPLOT2 THEME - COMPLETE VERSION
# ============================================================================

library(ggplot2)
library(showtext)
library(sysfonts)
library(scales)

# Add NYT fonts
font_add_google("Libre Franklin", "Franklin")
font_add_google("Crimson Pro")
font_add_google("Roboto Condensed", "Roboto Condensed")

showtext_auto()

# ============================================================================
# NEW YORK TIMES COLOR PALETTE
# ============================================================================

nyt_colors <- list(
    # Primary colors
    black = "#121212",
    gray = "#808080",
    light_gray = "#e2e2e2",
    lightest_gray = "#f7f7f7",
    
    # Accent colors (used in various NYT graphics)
    blue = "#2e74c0",
    red = "#cb2c30",
    orange = "#ea9123",
    green = "#3bb143",
    purple = "#7d3f98",
    teal = "#3db7c2",
    
    # Political colors
    democrat_blue = "#3a89c9",
    republican_red = "#e3342f",
    
    # Categorical palette (commonly used in NYT)
    categorical = c("#2e74c0", "#cb2c30", "#ea9123", "#3bb143", "#7d3f98", "#3db7c2"),
    
    # Sequential blues (for choropleths/heatmaps)
    blues = c("#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#084594"),
    
    # Diverging red-blue (for showing change)
    diverging_red_blue = c("#cb2c30", "#ed6c6c", "#f4a79d", "#e2e2e2", "#9ecae1", "#4292c6", "#2171b5")
)

# Function to access NYT colors
nyt_palette <- function(palette = "categorical", n = NULL, reverse = FALSE) {
    pal <- nyt_colors[[palette]]
    
    if (reverse) pal <- rev(pal)
    
    if (!is.null(n)) {
        if (n > length(pal)) {
            warning("Not enough colors in palette. Interpolating.")
            pal <- colorRampPalette(pal)(n)
        } else {
            pal <- pal[1:n]
        }
    }
    
    return(pal)
}

# ============================================================================
# MAIN NYT THEME
# ============================================================================

theme_nyt <- function(base_size = 11,
                      base_family = "Franklin",
                      base_line_size = 0.5,
                      base_rect_size = 0.5,
                      grid = "y") {
    
    ret <- theme_minimal(base_size = base_size,
                         base_family = base_family,
                         base_line_size = base_line_size,
                         base_rect_size = base_rect_size)
    
    ret <- ret + theme(
        # Plot elements
        plot.title = element_text(
            family = "Franklin",
            size = rel(1.4),
            face = "bold",
            hjust = 0,
            vjust = 1,
            margin = margin(b = 8),
            color = nyt_colors$black
        ),
        plot.subtitle = element_text(
            family = "Franklin",
            size = rel(1),
            hjust = 0,
            vjust = 1,
            margin = margin(b = 12),
            color = nyt_colors$gray
        ),
        plot.caption = element_text(
            family = "Franklin",
            size = rel(0.75),
            hjust = 0,
            vjust = 1,
            margin = margin(t = 12),
            color = nyt_colors$gray
        ),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "white", color = NA),
        
        # Panel elements
        panel.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank(),
        panel.spacing = unit(1, "lines"),
        
        # Grid lines - NYT style typically minimal
        panel.grid.major = element_line(color = nyt_colors$light_gray, size = 0.25),
        panel.grid.minor = element_blank(),
        
        # Axis elements
        axis.title = element_text(
            family = "Franklin",
            size = rel(0.85),
            color = nyt_colors$gray,
            face = "plain"
        ),
        axis.title.x = element_text(
            margin = margin(t = 8),
            hjust = 1
        ),
        axis.title.y = element_text(
            margin = margin(r = 8),
            hjust = 1
        ),
        axis.text = element_text(
            family = "Franklin",
            size = rel(0.8),
            color = nyt_colors$gray
        ),
        axis.text.x = element_text(margin = margin(t = 5)),
        axis.text.y = element_text(margin = margin(r = 5)),
        axis.ticks = element_line(color = nyt_colors$light_gray, size = 0.25),
        axis.ticks.length = unit(4, "pt"),
        axis.line = element_blank(),
        
        # Legend elements
        legend.position = "top",
        legend.justification = "left",
        legend.direction = "horizontal",
        legend.title = element_text(
            family = "Franklin",
            size = rel(0.85),
            face = "bold",
            color = nyt_colors$black
        ),
        legend.text = element_text(
            family = "Franklin",
            size = rel(0.8),
            color = nyt_colors$gray
        ),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.spacing.x = unit(4, "pt"),
        legend.margin = margin(b = 8),
        
        # Strip (facet) elements
        strip.text = element_text(
            family = "Franklin",
            size = rel(0.9),
            face = "bold",
            hjust = 0,
            color = nyt_colors$black,
            margin = margin(b = 5)
        ),
        strip.background = element_blank(),
        
        # Margins
        plot.margin = margin(15, 15, 10, 15),
        
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
# VARIANT: Dense data version
# ============================================================================

theme_nyt_dense <- function(base_size = 10,
                            base_family = "Roboto Condensed") {
    theme_nyt(base_size = base_size, 
              base_family = base_family) +
        theme(
            plot.title = element_text(size = rel(1.3)),
            plot.margin = margin(10, 10, 8, 10),
            panel.spacing = unit(0.5, "lines")
        )
}

# ============================================================================
# COLOR SCALES
# ============================================================================

scale_color_nyt <- function(palette = "categorical", reverse = FALSE, ...) {
    pal <- nyt_palette(palette = palette, reverse = reverse)
    
    ggplot2::discrete_scale(
        "colour", 
        paste0("nyt_", palette),
        palette = function(n) {
            if (n > length(pal)) {
                warning("Not enough colors in palette. Interpolating.")
                colorRampPalette(pal)(n)
            } else {
                pal[1:n]
            }
        },
        ...
    )
}

scale_fill_nyt <- function(palette = "categorical", reverse = FALSE, ...) {
    pal <- nyt_palette(palette = palette, reverse = reverse)
    
    ggplot2::discrete_scale(
        "fill", 
        paste0("nyt_", palette),
        palette = function(n) {
            if (n > length(pal)) {
                warning("Not enough colors in palette. Interpolating.")
                colorRampPalette(pal)(n)
            } else {
                pal[1:n]
            }
        },
        ...
    )
}

# Continuous color scales
scale_color_nyt_c <- function(palette = "blues", reverse = FALSE, ...) {
    pal <- nyt_palette(palette = palette, reverse = reverse)
    ggplot2::scale_color_gradientn(colors = pal, ...)
}

scale_fill_nyt_c <- function(palette = "blues", reverse = FALSE, ...) {
    pal <- nyt_palette(palette = palette, reverse = reverse)
    ggplot2::scale_fill_gradientn(colors = pal, ...)
}

# Diverging scales
scale_color_nyt_diverging <- function(midpoint = 0, ...) {
    pal <- nyt_colors$diverging_red_blue
    ggplot2::scale_color_gradient2(
        low = pal[1],
        mid = pal[4],
        high = pal[7],
        midpoint = midpoint,
        ...
    )
}

scale_fill_nyt_diverging <- function(midpoint = 0, ...) {
    pal <- nyt_colors$diverging_red_blue
    ggplot2::scale_fill_gradient2(
        low = pal[1],
        mid = pal[4],
        high = pal[7],
        midpoint = midpoint,
        ...
    )
}

# ============================================================================
# EXAMPLES - NYT Style Graphics
# ============================================================================

library(dplyr)

# Example 1: Classic NYT Bar Chart
df_bar <- data.frame(
    country = c("United States", "China", "Japan", "Germany", "United Kingdom", "France"),
    value = c(25.3, 17.7, 5.1, 4.2, 3.1, 2.9)
)

p1 <- ggplot(df_bar, aes(x = value, y = reorder(country, value))) +
    geom_col(fill = nyt_colors$blue, width = 0.65) +
    geom_text(aes(label = paste0("$", value, "T")), 
              hjust = -0.1, 
              family = "Franklin",
              size = 3.5,
              color = nyt_colors$gray) +
    scale_x_continuous(
        expand = expansion(mult = c(0, 0.15)),
        labels = dollar_format(suffix = "T", prefix = "$")
    ) +
    labs(
        title = "World's Largest Economies",
        subtitle = "Gross domestic product, 2023",
        caption = "Source: World Bank | Note: Values in trillions",
        x = NULL,
        y = NULL
    ) +
    theme_nyt(grid = "x")

print(p1)

# Example 2: Line Chart with Multiple Categories
set.seed(123)
df_line <- data.frame(
    date = rep(seq.Date(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "month"), 3),
    category = rep(c("Category A", "Category B", "Category C"), each = 48),
    value = c(
        cumsum(c(100, rnorm(47, 2, 5))),
        cumsum(c(100, rnorm(47, 1.5, 4))),
        cumsum(c(100, rnorm(47, 1, 3)))
    )
)

p2 <- ggplot(df_line, aes(x = date, y = value, color = category)) +
    geom_line(size = 1) +
    scale_y_continuous(labels = comma) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
    labs(
        title = "Performance Over Time",
        subtitle = "Monthly trends from 2020 to 2023",
        caption = "Source: Example data",
        x = NULL,
        y = "Index (Jan. 2020 = 100)",
        color = NULL
    ) +
    theme_nyt(grid = "y") +
    scale_color_nyt()

print(p2)

# Example 3: Grouped Bar Chart (Political style)
df_grouped <- data.frame(
    group = rep(c("2019", "2020", "2021", "2022", "2023"), 3),
    category = rep(c("Republicans", "Democrats", "Independents"), each = 5),
    percentage = c(
        45, 43, 44, 46, 47,  # Republicans
        48, 51, 52, 50, 49,  # Democrats
        7, 6, 4, 4, 4        # Independents
    )
)

p3 <- ggplot(df_grouped, aes(x = group, y = percentage, fill = category)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(
        aes(label = paste0(percentage, "%")),
        position = position_dodge(width = 0.8),
        vjust = -0.5,
        family = "Franklin",
        size = 3,
        color = nyt_colors$gray
    ) +
    scale_y_continuous(
        limits = c(0, 60),
        breaks = seq(0, 60, 20),
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(0, 0.05))
    ) +
    scale_fill_manual(values = c(
        "Republicans" = nyt_colors$republican_red,
        "Democrats" = nyt_colors$democrat_blue,
        "Independents" = nyt_colors$gray
    )) +
    labs(
        title = "Party Affiliation Over Time",
        subtitle = "Percentage of registered voters by party",
        caption = "Source: Example polling data",
        x = NULL,
        y = NULL,
        fill = NULL
    ) +
    theme_nyt(grid = "y")

print(p3)

# Example 4: Scatter Plot with Annotations
set.seed(42)
df_scatter <- data.frame(
    country = c("Norway", "Switzerland", "Ireland", "Germany", "Hong Kong", 
                "Australia", "Iceland", "Sweden", "Singapore", "Netherlands",
                "United States", "United Kingdom", "Japan", "South Korea", "Israel"),
    gdp = c(75420, 86850, 79925, 46445, 49800, 51812, 66944, 54146, 64103, 52295,
            63544, 44920, 39285, 31846, 41720),
    life_exp = c(82.3, 83.4, 82.1, 81.0, 84.7, 82.8, 82.7, 82.3, 83.1, 82.2,
                 78.5, 81.2, 84.2, 82.7, 82.8)
)

p4 <- ggplot(df_scatter, aes(x = gdp, y = life_exp)) +
    geom_point(
        size = 3,
        color = nyt_colors$blue,
        alpha = 0.7
    ) +
    geom_smooth(
        method = "lm",
        se = TRUE,
        color = nyt_colors$red,
        fill = nyt_colors$light_gray,
        size = 0.8
    ) +
    # Annotate specific points
    ggrepel::geom_text_repel(
        data = df_scatter %>% filter(country %in% c("United States", "Norway", "Japan")),
        aes(label = country),
        family = "Franklin",
        size = 3,
        color = nyt_colors$black,
        box.padding = 0.5
    ) +
    scale_x_continuous(
        labels = dollar_format(prefix = "$", big.mark = ",")
    ) +
    scale_y_continuous(
        labels = function(x) paste0(x, " years")
    ) +
    labs(
        title = "Wealth and Life Expectancy",
        subtitle = "GDP per capita vs. life expectancy in developed nations",
        caption = "Source: World Bank, WHO | Data for 2022",
        x = "GDP per capita",
        y = "Life expectancy"
    ) +
    theme_nyt(grid = "xy")

print(p4)

# Example 5: Area Chart (Stacked)
df_area <- data.frame(
    year = rep(2010:2023, 4),
    category = rep(c("Coal", "Natural Gas", "Nuclear", "Renewables"), each = 14),
    percentage = c(
        # Coal (declining)
        seq(45, 20, length.out = 14),
        # Natural Gas (growing then stable)
        c(24, 25, 26, 28, 30, 32, 34, 35, 36, 37, 37, 38, 38, 38),
        # Nuclear (stable)
        rep(20, 14),
        # Renewables (growing)
        seq(11, 22, length.out = 14)
    )
)

p5 <- ggplot(df_area, aes(x = year, y = percentage, fill = category)) +
    geom_area(alpha = 0.8) +
    scale_x_continuous(breaks = seq(2010, 2023, 2)) +
    scale_y_continuous(
        labels = function(x) paste0(x, "%"),
        expand = c(0, 0)
    ) +
    scale_fill_manual(
        values = c(
            "Coal" = nyt_colors$gray,
            "Natural Gas" = nyt_colors$blue,
            "Nuclear" = nyt_colors$orange,
            "Renewables" = nyt_colors$green
        )
    ) +
    labs(
        title = "The Changing Energy Mix",
        subtitle = "U.S. electricity generation by source",
        caption = "Source: Energy Information Administration",
        x = NULL,
        y = "Share of electricity generation",
        fill = NULL
    ) +
    theme_nyt(grid = "y") +
    theme(legend.position = "right")

print(p5)

# Example 6: Dot Plot (NYT style)
df_dot <- data.frame(
    country = c("Denmark", "New Zealand", "Switzerland", "Iceland", "Norway",
                "Netherlands", "Sweden", "Germany", "Ireland", "United States"),
    score = c(88, 87, 86, 85, 85, 82, 81, 80, 80, 69),
    region = c("Europe", "Oceania", "Europe", "Europe", "Europe",
               "Europe", "Europe", "Europe", "Europe", "Americas")
)

p6 <- ggplot(df_dot, aes(x = score, y = reorder(country, score))) +
    geom_segment(
        aes(x = 0, xend = score, yend = country),
        color = nyt_colors$light_gray,
        size = 1.5
    ) +
    geom_point(
        aes(color = region),
        size = 4
    ) +
    geom_text(
        aes(label = score),
        hjust = -0.5,
        family = "Franklin",
        size = 3,
        color = nyt_colors$gray
    ) +
    scale_x_continuous(
        expand = expansion(mult = c(0, 0.15)),
        limits = c(0, 100),
        breaks = seq(0, 100, 25)
    ) +
    scale_color_manual(
        values = c(
            "Europe" = nyt_colors$blue,
            "Americas" = nyt_colors$red,
            "Oceania" = nyt_colors$green
        )
    ) +
    labs(
        title = "Corruption Perceptions Index",
        subtitle = "Top 10 least corrupt countries, 2023",
        caption = "Source: Transparency International | Score out of 100",
        x = "Corruption score",
        y = NULL,
        color = NULL
    ) +
    theme_nyt(grid = "x")

print(p6)

# Example 7: Small Multiples (Faceted)
set.seed(456)
df_facet <- expand.grid(
    month = 1:12,
    year = 2021:2023,
    category = c("Online", "In-Store", "Mobile")
)

df_facet$sales <- with(df_facet, {
    base <- ifelse(category == "Online", 100, ifelse(category == "Mobile", 60, 150))
    trend <- (year - 2021) * 10
    seasonal <- sin((month - 1) * pi / 6) * 20
    base + trend + seasonal + rnorm(nrow(df_facet), 0, 5)
})

p7 <- ggplot(df_facet, aes(x = month, y = sales, color = category)) +
    geom_line(size = 1) +
    facet_wrap(~ year, ncol = 1, scales = "free_y") +
    scale_x_continuous(
        breaks = c(1, 4, 7, 10),
        labels = c("Jan", "Apr", "Jul", "Oct")
    ) +
    scale_y_continuous(labels = dollar_format(suffix = "M", prefix = "$")) +
    labs(
        title = "Retail Sales by Channel",
        subtitle = "Monthly sales trends across three years",
        caption = "Source: Example retail data",
        x = NULL,
        y = "Monthly sales",
        color = NULL
    ) +
    theme_nyt(grid = "y") +
    scale_color_nyt()

print(p7)

# Example 8: Heatmap (Calendar style)
df_heat <- expand.grid(
    week = 1:52,
    year = 2021:2023
)

set.seed(789)
df_heat$value <- rnorm(nrow(df_heat), 50, 15)

p8 <- ggplot(df_heat, aes(x = week, y = factor(year), fill = value)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradientn(
        colors = nyt_colors$blues,
        name = "Activity\nLevel"
    ) +
    scale_x_continuous(
        breaks = c(1, 13, 26, 39, 52),
        labels = c("Jan", "Apr", "Jul", "Oct", "Dec")
    ) +
    labs(
        title = "Weekly Activity Patterns",
        subtitle = "Heatmap of engagement levels over three years",
        caption = "Source: Example data",
        x = NULL,
        y = NULL
    ) +
    theme_nyt(grid = "none") +
    theme(
        legend.position = "right",
        axis.text.y = element_text(angle = 0)
    )

print(p8)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Add NYT-style annotation box
add_nyt_annotation <- function(plot, label, x, y, color = nyt_colors$blue) {
    plot +
        annotate(
            "label",
            x = x, y = y,
            label = label,
            family = "Franklin",
            size = 3,
            color = color,
            fill = alpha("white", 0.9),
            label.size = 0.5
        )
}

# Save plot with NYT styling
save_nyt_plot <- function(plot, filename, width = 8, height = 6, dpi = 300) {
    ggsave(
        filename = filename,
        plot = plot,
        width = width,
        height = height,
        dpi = dpi,
        bg = "white"
    )
    message("✓ NYT-style plot saved: ", filename)
}

# ============================================================================
# STYLE GUIDE
# ============================================================================

cat("\n=== NEW YORK TIMES STYLE GUIDE ===\n\n")

cat("TYPOGRAPHY:\n")
cat("  • Franklin Gothic (or Libre Franklin) for most text\n")
cat("  • Bold titles, left-aligned\n")
cat("  • Gray subtitle and caption\n")
cat("  • Clean, readable labels\n\n")

cat("COLORS:\n")
cat("  • Primary: Blue (#2e74c0)\n")
cat("  • Accent: Red (#cb2c30), Orange (#ea9123)\n")
cat("  • Political: Democrat Blue (#3a89c9), Republican Red (#e3342f)\n")
cat("  • Minimal color usage - focus on data\n\n")

cat("DESIGN:\n")
cat("  • Clean, minimal aesthetic\n")
cat("  • Subtle grid lines\n")
cat("  • Legend at top or right\n")
cat("  • Direct labeling when possible\n")
cat("  • White background\n\n")

cat("BEST PRACTICES:\n")
cat("  • Use horizontal bar charts for rankings\n")
cat("  • Add context with annotations\n")
cat("  • Clear, descriptive titles\n")
cat("  • Always cite sources in caption\n")
cat("  • Prioritize readability\n\n")

cat("CREATED PLOTS:\n")
cat("  p1 - Horizontal bar chart (economies)\n")
cat("  p2 - Multi-line time series\n")
cat("  p3 - Grouped bar chart (political)\n")
cat("  p4 - Scatter plot with annotations\n")
cat("  p5 - Stacked area chart (energy)\n")
cat("  p6 - Dot plot (corruption index)\n")
cat("  p7 - Small multiples (faceted)\n")
cat("  p8 - Heatmap (calendar style)\n\n")
