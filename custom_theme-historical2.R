# ==============================================================================
# NINETEENTH-CENTURY LITHOGRAPHIC STYLE GUIDE - R IMPLEMENTATION
# ==============================================================================

# Install and load required packages
# ----------------------------------
packages <- c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", 
              "dplyr", "showtext", "scales", "patchwork", "tidyr")

# Install if needed
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
lapply(packages, library, character.only = TRUE)

# ==============================================================================
# SETUP: FONTS AND COLORS
# ==============================================================================

# Load Google Fonts using showtext
# ---------------------------------
library(showtext)
# Add fonts with error handling
tryCatch({
    font_add_google("EB Garamond", "eb_garamond")
    cat("✓ EB Garamond loaded\n")
}, error = function(e) {
    cat("✗ EB Garamond failed:", e$message, "\n")
})

tryCatch({
    font_add_google("Crimson Pro", "crimson")  # Changed from "Crimson Text"
    cat("✓ Crimson Pro loaded\n")
}, error = function(e) {
    cat("✗ Crimson Pro failed:", e$message, "\n")
})

tryCatch({
    font_add_google("Libre Baskerville", "libre_baskerville")
    cat("✓ Libre Baskerville loaded\n")
}, error = function(e) {
    cat("✗ Libre Baskerville failed:", e$message, "\n")
})

tryCatch({
    font_add_google("Libre Franklin", "libre_franklin")
    cat("✓ Libre Franklin loaded\n")
}, error = function(e) {
    cat("✗ Libre Franklin failed:", e$message, "\n")
})


# Enable showtext for rendering
showtext_auto()
showtext_opts(dpi = 300)

# Define Color Palette
# --------------------
lithographic_colors <- list(
    # Base colors
    land = "#F4EAD5",
    water = "#7BA8C5",
    border_primary = "#8B4513",
    border_secondary = "#C19A6B",
    text = "#2C1810",
    
    # Accent colors (colorblind-friendly)
    accent1 = "#2D5F5D",  # Dark teal
    accent2 = "#D4A574",  # Ochre
    accent3 = "#4A6741",  # Olive
    accent4 = "#9B7E6B",  # Warm gray-brown
    
    # Categorical palette
    categorical = c("#2D5F5D", "#D4A574", "#4A6741", "#7BA8C5", "#9B7E6B", "#B8956A"),
    
    # Sequential palette
    sequential = c("#F4EAD5", "#D4C4A8", "#B8956A", "#8B6F47", "#5D4A2F", "#2C1810")
)

# Helper function to lighten colors
lighten <- function(color, amount = 0.3) {
    col_rgb <- col2rgb(color) / 255
    col_rgb <- col_rgb + (1 - col_rgb) * amount
    rgb(col_rgb[1], col_rgb[2], col_rgb[3])
}


# Define Font Scheme
# ------------------
fonts <- list(
    title = "eb_garamond",
    caption = "crimson",
    axis = "libre_franklin",
    map_country = "eb_garamond",
    map_city = "libre_baskerville",
    map_feature = "crimson"
)

# Custom ggplot2 theme
# --------------------
theme_lithographic2 <- function(base_size = 11) {
    theme_minimal(base_family = fonts$axis) +
        theme(
            # Text elements
            plot.title = element_text(
                family = fonts$title,
                size = base_size * 1.8,
                face = "bold",
                color = lithographic_colors$text,
                margin = margin(b = 10)
            ),
            plot.subtitle = element_text(
                family = fonts$title,
                size = base_size * 1.2,
                color = lithographic_colors$text,
                margin = margin(b = 15)
            ),
            plot.caption = element_text(
                family = fonts$caption,
                size = base_size * 0.8,
                color = lithographic_colors$text,
                hjust = 0,
                margin = margin(t = 10)
            ),
            axis.title = element_text(
                family = fonts$axis,
                size = base_size * 0.9,
                color = lithographic_colors$text
            ),
            axis.text = element_text(
                family = fonts$axis,
                size = base_size * 0.8,
                color = lithographic_colors$text
            ),
            legend.title = element_text(
                family = fonts$title,
                size = base_size,
                face = "bold",
                color = lithographic_colors$text
            ),
            legend.text = element_text(
                family = fonts$axis,
                size = base_size * 0.9,
                color = lithographic_colors$text
            ),
            
            # Background
            plot.background = element_rect(fill = lithographic_colors$land, color = NA),
            panel.background = element_rect(fill = lithographic_colors$land, color = NA),
            panel.grid.major = element_line(color = lithographic_colors$border_secondary, 
                                            linewidth = 0.3),
            panel.grid.minor = element_blank(),
            
            # Legend
            legend.background = element_rect(fill = lithographic_colors$land, 
                                             color = lithographic_colors$border_primary,
                                             linewidth = 0.5),
            legend.key = element_rect(fill = lithographic_colors$land, color = NA),
            
            # Margins
            plot.margin = margin(20, 20, 20, 20)
        )
}

# ==============================================================================
# EXAMPLE 1: BAR CHART
# ==============================================================================

# Sample data: fictional book sales by region
book_sales <- data.frame(
    region = c("Northern Europe", "Southern Europe", "British Isles", 
               "Central Europe", "Eastern Europe", "Mediterranean"),
    sales = c(2340, 1890, 3120, 2670, 1450, 1780),
    stringsAsFactors = FALSE
)

bar_chart <- ggplot(book_sales, aes(x = reorder(region, sales), y = sales, 
                                    fill = region)) +
    geom_bar(stat = "identity", color = lithographic_colors$border_primary, 
             linewidth = 0.5) +
    scale_fill_manual(values = lithographic_colors$categorical) +
    coord_flip() +
    labs(
        title = "Book Sales by European Region",
        subtitle = "Nineteenth-Century Trade Data, 1875",
        x = NULL,
        y = "Sales Volume (Units)",
        caption = "Source: Hypothetical historical trade records. Visualization styled after 19th-century lithographic atlases."
    ) +
    theme_lithographic2() +
    theme(legend.position = "none") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3500))

print(bar_chart)

# Save
ggsave("lithographic_bar_chart.png", bar_chart, width = 10, height = 7, dpi = 300, bg = lithographic_colors$land)

# ==============================================================================
# EXAMPLE 2: LINE CHART WITH MULTIPLE SERIES
# ==============================================================================

# Sample data: population growth
years <- seq(1800, 1900, by = 10)
population_data <- data.frame(
    year = rep(years, 3),
    city = rep(c("London", "Paris", "Vienna"), each = length(years)),
    population = c(
        # London
        seq(1, 6.5, length.out = 11) * 100000,
        # Paris
        seq(0.5, 2.7, length.out = 11) * 100000,
        # Vienna
        seq(0.25, 1.7, length.out = 11) * 100000
    )
)

line_chart <- ggplot(population_data, aes(x = year, y = population, 
                                          color = city, group = city)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 3, shape = 21, fill = lithographic_colors$land, stroke = 1.5) +
    scale_color_manual(values = lithographic_colors$categorical[1:3]) +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(
        title = "Urban Population Growth in European Capitals",
        subtitle = "1800–1900",
        x = "Year",
        y = "Population",
        color = "City",
        caption = "Source: Historical census data (approximated). Data points represent decennial estimates."
    ) +
    theme_lithographic2() +
    theme(
        legend.position = c(0.15, 0.8),
        legend.background = element_rect(
            fill = lithographic_colors$land,
            color = lithographic_colors$border_primary,
            linewidth = 0.5
        )
    )

print(line_chart)

ggsave("lithographic_line_chart.png", line_chart, width = 10, height = 7, dpi = 300, bg = lithographic_colors$land)

# ==============================================================================
# EXAMPLE 3: CHOROPLETH MAP
# ==============================================================================

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create sample data: literacy rates by country (fictional historical data)
set.seed(42)
literacy_data <- data.frame(
    name = c("United Kingdom", "France", "Germany", "Spain", "Italy", 
             "Austria", "Russia", "Sweden", "Norway", "Denmark",
             "Netherlands", "Belgium", "Switzerland", "Portugal", "Greece"),
    literacy_rate = c(75, 68, 72, 45, 42, 55, 28, 82, 78, 80, 70, 65, 73, 38, 35)
)

# Join data
world_data <- world %>%
    left_join(literacy_data, by = "name")

# Focus on Europe
europe_bbox <- st_bbox(c(xmin = -25, xmax = 40, ymin = 34, ymax = 71), 
                       crs = st_crs(world))

choropleth_map <- ggplot(data = world_data) +
    geom_sf(aes(fill = literacy_rate), color = lithographic_colors$border_primary, 
            linewidth = 0.3) +
    coord_sf(xlim = c(-25, 40), ylim = c(34, 71), expand = FALSE) +
    scale_fill_gradientn(
        colors = lithographic_colors$sequential,
        na.value = lithographic_colors$land,
        name = "Literacy Rate (%)",
        breaks = seq(20, 80, by = 20),
        guide = guide_colorbar(
            barwidth = 1,
            barheight = 10,
            title.position = "top"
        )
    ) +
    labs(
        title = "Literacy Rates Across Europe",
        subtitle = "Estimated adult literacy, circa 1870",
        caption = "Source: Hypothetical historical census data. Countries without data shown in base color."
    ) +
    theme_lithographic2() +
    theme(
        panel.grid.major = element_line(color = lithographic_colors$water, 
                                        linewidth = 0.2, linetype = "dotted"),
        panel.background = element_rect(fill = lighten(lithographic_colors$water, 0.3)),
        axis.text = element_blank(),
        legend.position = c(0.02, 0.3),
        legend.justification = c(0, 0.5)
    )

print(choropleth_map)

ggsave("lithographic_choropleth_map.png", choropleth_map, width = 10, height = 8, dpi = 300)

# ==============================================================================
# EXAMPLE 4: DETAILED REFERENCE MAP WITH LABELS
# ==============================================================================

# Focus on a smaller region: British Isles
uk_ireland <- world %>%
    filter(name %in% c("United Kingdom", "Ireland"))

# Create fictional cities data
cities <- data.frame(
    name = c("London", "Edinburgh", "Dublin", "Manchester", "Liverpool", 
             "Birmingham", "Glasgow", "Belfast", "Cork", "Cardiff"),
    lon = c(-0.1276, -3.1883, -6.2603, -2.2426, -2.9916, 
            -1.8904, -4.2518, -5.9301, -8.4756, -3.1791),
    lat = c(51.5074, 55.9533, 53.3498, 53.4808, 53.4084, 
            52.4862, 55.8642, 54.5973, 51.8985, 51.4816),
    type = c("capital", "major", "capital", "major", "major",
             "major", "major", "major", "small", "small"),
    stringsAsFactors = FALSE
)

# Define city sizes and colors by type
city_sizes <- c("capital" = 4, "major" = 3, "small" = 2)
city_colors <- c("capital" = lithographic_colors$accent1, 
                 "major" = lithographic_colors$text,
                 "small" = lighten(lithographic_colors$text, 0.3))

reference_map <- ggplot() +
    # Base map
    geom_sf(data = uk_ireland, fill = lithographic_colors$land, 
            color = lithographic_colors$border_primary, linewidth = 0.8) +
    
    # Cities as points
    geom_point(data = cities, aes(x = lon, y = lat, size = type, color = type)) +
    
    # City labels
    ggrepel::geom_text_repel(
        data = cities,
        aes(x = lon, y = lat, label = name, fontface = ifelse(type == "capital", "bold", "plain")),
        family = fonts$map_city,
        color = lithographic_colors$text,
        size = c(5, 4, 5, 3.5, 3.5, 3.5, 4, 3.5, 3, 3),  # Manual sizes
        box.padding = 0.5,
        point.padding = 0.3,
        segment.color = lithographic_colors$border_secondary,
        segment.size = 0.3,
        min.segment.length = 0
    ) +
    
    # Country labels (large)
    annotate("text", x = -2, y = 54, label = "GREAT BRITAIN", 
             family = fonts$map_country, fontface = "bold", 
             size = 8, color = lighten(lithographic_colors$text, 0.4),
             alpha = 0.6) +
    annotate("text", x = -7.5, y = 53, label = "IRELAND", 
             family = fonts$map_country, fontface = "bold", 
             size = 7, color = lighten(lithographic_colors$text, 0.4),
             alpha = 0.6) +
    
    # Scale
    scale_size_manual(values = city_sizes, guide = "none") +
    scale_color_manual(values = city_colors, 
                       name = "Settlement Type",
                       labels = c("Capital City", "Major City", "Town")) +
    
    # Coordinate system
    coord_sf(xlim = c(-11, 2), ylim = c(49.5, 59), expand = FALSE) +
    
    labs(
        title = "Principal Cities of the British Isles",
        subtitle = "Reference map with major settlements, circa 1880",
        caption = "Cartography styled after nineteenth-century lithographic atlases. Scale approximate."
    ) +
    
    theme_lithographic2() +
    theme(
        panel.background = element_rect(fill = lighten(lithographic_colors$water, 0.3)),
        panel.grid.major = element_line(color = lithographic_colors$water, 
                                        linewidth = 0.2, linetype = "dotted"),
        axis.text = element_text(size = 7, family = fonts$axis),
        legend.position = c(0.02, 0.02),
        legend.justification = c(0, 0)
    )

print(reference_map)

ggsave("lithographic_reference_map.png", reference_map, width = 8, height = 10, dpi = 300)

# ==============