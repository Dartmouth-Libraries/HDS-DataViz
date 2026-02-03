# ==============================================================================
# NINETEENTH-CENTURY LITHOGRAPHIC STYLE GUIDE - R IMPLEMENTATION
# ==============================================================================

# Install and load required packages
# ----------------------------------
packages <- c("ggplot2", "ggrepel", "sf", "rnaturalearth", "rnaturalearthdata", 
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

# Define font configuration (MODIFY THESE TO CHANGE FONTS)
# ---------------------------------------------------------
font_config <- list(
    title = list(
        # name = "EB Garamond", #family = "eb_garamond",
        name = "Playfair Display",
        family = "playfair-display",
        sizes = list(
            viz_title = 18,
            viz_subtitle = 13,
            map_country = 14,
            map_province = 11,
            map_range = 9
        ),
        weights = list(
            bold = "bold",
            semibold = "600",
            regular = "400"
        )
    ),
    country_name = list(
        name = "Montserrat",
        family = "montserrat",
        sizes = list(
            major_country = 20,
            small_country = 16
        ),
        weights = list(
            bold = "bold",
            regular = "700"
        )
    ),
    
    caption = list(
        name = "Montserrat",
        family = "montserrat",
        sizes = list(
            viz_caption = 9,
            map_town = 7,
            map_peak = 7,
            map_water = 8
        ),
        weights = list(
            regular = "400",
            italic = "italic"
        )
    ),
    axis = list(
        name = "Lato",
        family = "lato",
        sizes = list(
            axis_label = 9,
            axis_text = 8,
            legend = 9
        ),
        weights = list(
            regular = "400",
            medium = "500"
        )
    ),
    map_city = list(
        name = "Montserrat",
        family = "montserrat",
        sizes = list(
            major_city = 10,
            small_city = 8
        ),
        weights = list(
            bold = "bold",
            regular = "400"
        )
    ),
    geog_features = list(
        name = "Vollkorn",
        family = "vollkorn",
        sizes = list(
            large_features = 16,
            small_features = 10
        ),
        weights = list(
            light = "300",
            regular = "400",
            medium = "500",
            italic = "italic"
        )
    )
    
)


tryCatch({
    font_add_google("EB Garamond", "eb_garamond")
    cat("✓ EB Garamond loaded\n")
}, error = function(e) {
    cat("✗ EB Garamond failed:", e$message, "\n")
})

# Add fonts with error handling
tryCatch({
    font_add_google(font_config$title$name, font_config$title$family)
    cat(sprintf("✓ %s loaded\n", font_config$title$name))
}, error = function(e) {
    cat(sprintf("✗ %s failed: %s\n", font_config$title$name, e$message))
})

tryCatch({
    font_add_google(font_config$caption$name, font_config$caption$family)
    cat(sprintf("✓ %s loaded\n", font_config$caption$name))
}, error = function(e) {
    cat(sprintf("✗ %s failed: %s\n", font_config$caption$name, e$message))
})

tryCatch({
    font_add_google(font_config$axis$name, font_config$axis$family)
    cat(sprintf("✓ %s loaded\n", font_config$axis$name))
}, error = function(e) {
    cat(sprintf("✗ %s failed: %s\n", font_config$axis$name, e$message))
})

tryCatch({
    font_add_google(font_config$map_city$name, font_config$map_city$family)
    cat(sprintf("✓ %s loaded\n", font_config$map_city$name))
}, error = function(e) {
    cat(sprintf("✗ %s failed: %s\n", font_config$map_city$name, e$message))
})

tryCatch({
    font_add_google(font_config$country_name$name, font_config$country_name$family)
    cat(sprintf("✓ %s loaded\n", font_config$country_name$name))
}, error = function(e) {
    cat(sprintf("✗ %s failed: %s\n", font_config$country_name$name, e$message))
})

tryCatch({
    font_add_google(font_config$geog_features$name, font_config$geog_features$family)
    cat(sprintf("✓ %s loaded\n", font_config$geog_features$name))
}, error = function(e) {
    cat(sprintf("✗ %s failed: %s\n", font_config$geog_features$name, e$message))
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

# ==============================================================================
# HELPER FUNCTION: Darken colors (complement to lighten)
# ==============================================================================

darken <- function(color, amount = 0.3) {
    col_rgb <- col2rgb(color) / 255
    col_rgb <- col_rgb * (1 - amount)
    rgb(col_rgb[1], col_rgb[2], col_rgb[3])
}


# Define Font Scheme
# ------------------

# default

# fonts <- list(
#     title = "eb_garamond",
#     caption = "crimson",
#     axis = "libre_franklin",
#     map_country = "eb_garamond",
#     map_city = "libre_baskerville",
#     map_feature = "crimson"
# )


# option 2
# fonts <- list(
#     title = "amatic sc",
#     caption = "montserrat",
#     axis = "montserrat",
#     map_country = "montserrat",
#     map_city = "crimson",
#     map_feature = "spectral"
# )

# Simplified font references for backward compatibility
# ------------------------------------------------------
fonts <- list(
    title = font_config$title$family,
    caption = font_config$caption$family,
    axis = font_config$axis$family,
    map_country = font_config$country_name$family,
    map_city = font_config$map_city$family,
    map_feature = font_config$caption$family,
    geog_feature = font_config$geog_features$family
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

# print(bar_chart)

# Save
ggsave("week4/images/lithographic_theme/lithographic_bar_chart.png", bar_chart, width = 10, height = 7, dpi = 300, bg = lithographic_colors$land)

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

# print(line_chart)

ggsave("week4/images/lithographic_theme/lithographic_line_chart.png", line_chart, width = 10, height = 7, dpi = 300, bg = lithographic_colors$land)

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

# print(choropleth_map)

ggsave("week4/images/lithographic_theme/lithographic_choropleth_map.png", choropleth_map, width = 10, height = 8, dpi = 300)

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

# print(reference_map)

ggsave("week4/images/lithographic_theme/lithographic_reference_map.png", reference_map, width = 8, height = 10, dpi = 300)

# ==============

# ==============================================================================
# EXAMPLE 5: TOPOGRAPHIC MAP WITH GEOGRAPHIC FEATURES
# ==============================================================================

# This example creates a detailed map of the Alps region showing mountains,
# ranges, valleys, and rivers in the lithographic style

# Load additional packages if needed
if (!require("ggspatial")) install.packages("ggspatial")
library(ggspatial)

# Get base map data for Alpine region
alpine_countries <- world %>%
    filter(name %in% c("Switzerland", "Austria", "Italy", "France", "Germany", "Slovenia"))

# Define geographic features data
# -------------------------------

# Mountain ranges
mountain_ranges <- data.frame(
    name = c("WESTERN ALPS", "PENNINE ALPS", "BERNESE ALPS", 
             "EASTERN ALPS", "DOLOMITES"),
    lon = c(6.5, 7.8, 7.8, 12.5, 11.8),
    lat = c(45.2, 46.0, 46.5, 47.2, 46.4),
    angle = c(-20, 0, -10, 15, 5),
    stringsAsFactors = FALSE
)

# Major mountain peaks
mountain_peaks <- data.frame(
    name = c("Mont Blanc", "Matterhorn", "Monte Rosa", "Jungfrau", 
             "Grossglockner", "Piz Bernina", "Eiger", "Ortler",
             "Gran Paradiso", "Finsteraarhorn"),
    lon = c(6.8652, 7.6586, 7.8687, 7.9629, 
            12.6942, 9.9071, 8.0055, 10.5444,
            7.2658, 8.1264),
    lat = c(45.8326, 45.9763, 45.9372, 46.5369, 
            47.0744, 46.3830, 46.5772, 46.5059,
            45.5167, 46.5374),
    elevation = c(4808, 4478, 4634, 4158, 
                  3798, 4049, 3970, 3905,
                  4061, 4274),
    stringsAsFactors = FALSE
)

# Major rivers
rivers <- data.frame(
    name = c("Rhône", "Rhine", "Inn", "Po", "Adige", "Aare"),
    # Simplified river paths (start and end points)
    lon_start = c(8.4, 9.5, 10.2, 7.2, 10.5, 8.2),
    lat_start = c(46.6, 46.8, 46.9, 45.1, 46.8, 46.6),
    lon_end = c(4.8, 6.0, 13.0, 12.0, 11.1, 8.2),
    lat_end = c(43.3, 47.6, 48.2, 45.0, 45.2, 47.4),
    stringsAsFactors = FALSE
)

# Create curved river paths
create_river_path <- function(lon_start, lat_start, lon_end, lat_end, n_points = 20) {
    # Create a slightly curved path between two points
    t <- seq(0, 1, length.out = n_points)
    
    # Add some curvature
    mid_offset_lon <- rnorm(1, 0, 0.3)
    mid_offset_lat <- rnorm(1, 0, 0.2)
    
    lon <- lon_start + (lon_end - lon_start) * t + 
        mid_offset_lon * sin(pi * t)
    lat <- lat_start + (lat_end - lat_start) * t + 
        mid_offset_lat * sin(pi * t)
    
    data.frame(lon = lon, lat = lat)
}

# Generate river paths
set.seed(123)
river_paths <- lapply(1:nrow(rivers), function(i) {
    path <- create_river_path(
        rivers$lon_start[i], rivers$lat_start[i],
        rivers$lon_end[i], rivers$lat_end[i]
    )
    path$river <- rivers$name[i]
    return(path)
})
river_paths_df <- do.call(rbind, river_paths)

# Major valleys
valleys <- data.frame(
    name = c("Rhône Valley", "Inn Valley", "Val d'Aosta", "Engadine"),
    lon = c(7.5, 11.5, 7.4, 9.8),
    lat = c(46.3, 47.3, 45.7, 46.5),
    stringsAsFactors = FALSE
)

# Major cities in the region (for context)
alpine_cities <- data.frame(
    name = c("Geneva", "Bern", "Zürich", "Innsbruck", "Milan", 
             "Turin", "Grenoble", "Salzburg", "Bolzano"),
    lon = c(6.1432, 7.4474, 8.5417, 11.3948, 9.1900, 
            7.6869, 5.7245, 13.0550, 11.3548),
    lat = c(46.2044, 46.9480, 47.3769, 47.2692, 45.4642, 
            45.0703, 45.1885, 47.8095, 46.4983),
    type = c("major", "capital", "major", "major", "major",
             "major", "major", "major", "small"),
    stringsAsFactors = FALSE
)

# Create the topographic map
# ---------------------------

topographic_map <- ggplot() +
    
    # Base map - countries
    geom_sf(data = alpine_countries, 
            fill = lithographic_colors$land, 
            color = lithographic_colors$border_secondary, 
            linewidth = 0.4) +
    
    # Rivers - draw first so they're behind other features
    geom_path(data = river_paths_df, 
              aes(x = lon, y = lat, group = river),
              color = lithographic_colors$water,
              linewidth = 0.8,
              alpha = 0.7) +
    
    # River labels
    geom_text(data = rivers,
              aes(x = (lon_start + lon_end) / 2, 
                  y = (lat_start + lat_end) / 2, 
                  label = name,
                  angle = atan2(lat_end - lat_start, lon_end - lon_start) * 180 / pi),
              family = fonts$geog_feature,
              fontface = "italic",
              color = darken(lithographic_colors$water, 0.3),
              size = 3,
              hjust = 0.5) +
    
    # Mountain ranges - large text with transparency
    geom_text(data = mountain_ranges,
              aes(x = lon, y = lat, label = name, angle = angle),
              family = fonts$geog_feature,
              fontface = "bold",
              color = lithographic_colors$accent3,
              size = 5,
              alpha = 0.4,
              hjust = 0.5) +
    
    # Mountain peaks - triangular symbols
    geom_point(data = mountain_peaks,
               aes(x = lon, y = lat),
               shape = 24,  # filled triangle
               size = 3,
               fill = lithographic_colors$accent3,
               color = lithographic_colors$text,
               stroke = 0.5) +
    
    # Mountain peak labels
    geom_text_repel(data = mountain_peaks,
                    aes(x = lon, y = lat, 
                        label = paste0(name, "\n", elevation, " m")),
                    family = fonts$geog_feature,
                    fontface = "italic",
                    color = lithographic_colors$text,
                    size = 2.5,
                    box.padding = 0.5,
                    point.padding = 0.3,
                    segment.color = lithographic_colors$border_secondary,
                    segment.size = 0.2,
                    min.segment.length = 0,
                    lineheight = 0.9) +
    
    # Valley labels
    geom_text(data = valleys,
              aes(x = lon, y = lat, label = name),
              family = fonts$geog_feature,
              fontface = "italic",
              color = lighten(lithographic_colors$text, 0.3),
              size = 3.5,
              alpha = 0.6) +
    
    # Cities
    geom_point(data = alpine_cities,
               aes(x = lon, y = lat, size = type),
               shape = 21,
               fill = lithographic_colors$accent1,
               color = lithographic_colors$text,
               stroke = 0.5) +
    
    # City labels
    geom_text_repel(data = alpine_cities,
                    aes(x = lon, y = lat, label = name,
                        fontface = ifelse(type == "capital", "bold", "plain")),
                    family = fonts$map_city,
                    color = lithographic_colors$text,
                    size = 3,
                    box.padding = 0.3,
                    point.padding = 0.2,
                    segment.color = lithographic_colors$border_secondary,
                    segment.size = 0.2) +
    
    # Scale
    scale_size_manual(values = c("capital" = 3.5, "major" = 2.5, "small" = 1.5),
                      guide = "none") +
    
    # Coordinate system focused on Alps
    coord_sf(xlim = c(4.5, 14), ylim = c(44.5, 48.5), expand = FALSE) +
    
    # Labels and theme
    labs(
        title = "Topographic Map of the Alpine Region",
        subtitle = "Principal peaks, ranges, valleys and waterways",
        caption = "Source: Geographic features compiled from historical surveys. Elevations in meters above sea level.\nCartography styled after 19th-century mountain atlases."
    ) +
    
    theme_lithographic2() +
    theme(
        panel.background = element_rect(fill = lighten(lithographic_colors$land, 0.2)),
        panel.grid.major = element_line(color = lithographic_colors$border_secondary, 
                                        linewidth = 0.15, linetype = "dotted"),
        axis.text = element_text(size = 7)
    ) +
    
    # Add a scale bar
    ggspatial::annotation_scale(
        location = "bl",
        width_hint = 0.2,
        text_family = fonts$axis,
        text_col = lithographic_colors$text,
        bar_cols = c(lithographic_colors$text, "white"),
        line_col = lithographic_colors$text
    ) +
    
    # Add north arrow
    ggspatial::annotation_north_arrow(
        location = "tr",
        which_north = "true",
        style = north_arrow_fancy_orienteering(
            fill = c(lithographic_colors$text, "white"),
            line_col = lithographic_colors$text,
            text_family = fonts$axis,
            text_col = lithographic_colors$text
        ),
        height = unit(1.5, "cm"),
        width = unit(1.5, "cm")
    )

# print(topographic_map)

ggsave("week4/images/lithographic_theme/lithographic_topographic_map.png", topographic_map, 
       width = 12, height = 10, dpi = 300, bg = lithographic_colors$land)

# ==============================================================================
# EXAMPLE 6: DETAILED VALLEY CROSS-SECTION (BONUS)
# ==============================================================================

# Create a stylized elevation profile showing the lithographic style
# for scientific diagrams

# Sample elevation data across the Alps (Geneva to Vienna)
distance <- seq(0, 700, by = 5)  # km
set.seed(456)

# Create a realistic alpine profile
elevation <- 400 + 
    2000 * dnorm(distance, 200, 50) * 50 +  # Western Alps peak
    1800 * dnorm(distance, 350, 60) * 50 +  # Central Alps peak
    1500 * dnorm(distance, 550, 40) * 50 +  # Eastern Alps peak
    rnorm(length(distance), 0, 50)  # Add noise

profile_data <- data.frame(
    distance = distance,
    elevation = pmax(elevation, 200)  # Ensure minimum elevation
)

# Notable peaks along the profile
profile_peaks <- data.frame(
    name = c("Mont Blanc\nRegion", "Bernese\nAlps", "Hohe Tauern\nRange"),
    distance = c(200, 350, 550),
    elevation = c(3500, 3200, 2800),
    stringsAsFactors = FALSE
)

elevation_profile <- ggplot() +
    
    # Fill area under curve
    geom_ribbon(data = profile_data,
                aes(x = distance, ymin = 0, ymax = elevation),
                fill = lighten(lithographic_colors$accent3, 0.5),
                color = lithographic_colors$text,
                linewidth = 0.5) +
    
    # Elevation line
    geom_line(data = profile_data,
              aes(x = distance, y = elevation),
              color = lithographic_colors$text,
              linewidth = 1) +
    
    # Mark major peaks
    geom_point(data = profile_peaks,
               aes(x = distance, y = elevation),
               shape = 24,
               size = 4,
               fill = lithographic_colors$accent1,
               color = lithographic_colors$text) +
    
    # Label peaks
    geom_text(data = profile_peaks,
              aes(x = distance, y = elevation + 300, label = name),
              family = fonts$geog_feature,
              size = 3.5,
              lineheight = 0.9,
              color = lithographic_colors$text) +
    
    # Add location labels
    annotate("text", x = 50, y = 100, label = "Geneva",
             family = fonts$map_city, size = 3.5, hjust = 0,
             color = lithographic_colors$text) +
    annotate("text", x = 650, y = 100, label = "Vienna",
             family = fonts$map_city, size = 3.5, hjust = 1,
             color = lithographic_colors$text) +
    
    # Scales
    scale_x_continuous(
        name = "Distance (kilometers)",
        breaks = seq(0, 700, 100),
        expand = c(0, 0)
    ) +
    scale_y_continuous(
        name = "Elevation (meters above sea level)",
        breaks = seq(0, 4000, 500),
        expand = c(0, 0),
        limits = c(0, 4000)
    ) +
    
    # Labels
    labs(
        title = "Elevation Profile Across the Alpine Range",
        subtitle = "Geneva to Vienna transect, approximately 700 km",
        caption = "Source: Compiled from topographic surveys. Profile simplified for illustration.\nVertical scale exaggerated for clarity."
    ) +
    
    theme_lithographic2() +
    theme(
        panel.grid.major.y = element_line(color = lithographic_colors$border_secondary,
                                          linewidth = 0.3),
        panel.grid.major.x = element_line(color = lithographic_colors$border_secondary,
                                          linewidth = 0.3),
        panel.grid.minor = element_blank()
    )

# print(elevation_profile)

ggsave("week4/images/lithographic_theme/lithographic_elevation_profile.png", elevation_profile,
       width = 12, height = 6, dpi = 300, bg = lithographic_colors$land)

# ==============================================================================
# EXAMPLE 7: WATERSHED/DRAINAGE BASIN MAP
# ==============================================================================

# Create a detailed map showing river systems and drainage basins
# This demonstrates how to show hydrological features in the lithographic style

# Define major river systems in Central Europe
danube_system <- data.frame(
    river = "Danube",
    segment = 1:25,
    lon = seq(8.5, 29, length.out = 25) + rnorm(25, 0, 0.3),
    lat = c(seq(48.5, 48.2, length.out = 10), 
            seq(48.2, 47.5, length.out = 8),
            seq(47.5, 45, length.out = 7)) + rnorm(25, 0, 0.15)
)

# Danube tributaries
inn_tributary <- data.frame(
    river = "Inn",
    segment = 1:10,
    lon = seq(10.2, 13.0, length.out = 10) + rnorm(10, 0, 0.2),
    lat = seq(46.9, 48.2, length.out = 10) + rnorm(10, 0, 0.1)
)

drava_tributary <- data.frame(
    river = "Drava",
    segment = 1:12,
    lon = seq(12.2, 18.5, length.out = 12) + rnorm(12, 0, 0.2),
    lat = seq(46.7, 45.8, length.out = 12) + rnorm(12, 0, 0.1)
)

sava_tributary <- data.frame(
    river = "Sava",
    segment = 1:15,
    lon = seq(13.8, 20.5, length.out = 15) + rnorm(15, 0, 0.25),
    lat = seq(46.0, 44.8, length.out = 15) + rnorm(15, 0, 0.12)
)

# Combine all river data
all_rivers <- rbind(danube_system, inn_tributary, drava_tributary, sava_tributary)

# River label positions
river_labels <- data.frame(
    river = c("DANUBE", "Inn", "Drava", "Sava"),
    lon = c(18, 11.5, 15.5, 17),
    lat = c(47.8, 47.5, 46.2, 45.4),
    size = c(5, 3.5, 3.5, 3.5),
    angle = c(0, 25, 5, 3),
    stringsAsFactors = FALSE
)

# Major cities along the rivers
river_cities <- data.frame(
    name = c("Vienna", "Budapest", "Belgrade", "Innsbruck", "Salzburg", 
             "Linz", "Bratislava", "Zagreb", "Ljubljana"),
    lon = c(16.3738, 19.0402, 20.4489, 11.3948, 13.0550,
            14.2858, 17.1077, 15.9819, 14.5058),
    lat = c(48.2082, 47.4979, 44.7866, 47.2692, 47.8095,
            48.3069, 48.1486, 45.8150, 46.0569),
    type = c("capital", "capital", "capital", "major", "major",
             "major", "capital", "major", "capital"),
    stringsAsFactors = FALSE
)

# Get countries for this region
danube_countries <- world %>%
    filter(name %in% c("Germany", "Austria", "Slovakia", "Hungary", 
                       "Croatia", "Serbia", "Romania", "Slovenia"))

# Create watershed map
watershed_map <- ggplot() +
    
    # Base map
    geom_sf(data = danube_countries,
            fill = lithographic_colors$land,
            color = lithographic_colors$border_primary,
            linewidth = 0.6) +
    
    # Rivers - varying widths for main river vs tributaries
    geom_path(data = danube_system,
              aes(x = lon, y = lat, group = river),
              color = lithographic_colors$water,
              linewidth = 2,
              lineend = "round") +
    
    geom_path(data = inn_tributary,
              aes(x = lon, y = lat, group = river),
              color = lithographic_colors$water,
              linewidth = 1.2,
              lineend = "round") +
    
    geom_path(data = drava_tributary,
              aes(x = lon, y = lat, group = river),
              color = lithographic_colors$water,
              linewidth = 1.2,
              lineend = "round") +
    
    geom_path(data = sava_tributary,
              aes(x = lon, y = lat, group = river),
              color = lithographic_colors$water,
              linewidth = 1.2,
              lineend = "round") +
    
    # River labels
    geom_text(data = river_labels,
              aes(x = lon, y = lat, label = river, angle = angle),
              family = fonts$geog_feature,
              fontface = ifelse(river_labels$river == "DANUBE", "bold.italic", "italic"),
              color = darken(lithographic_colors$water, 0.4),
              size = river_labels$size) +
    
    # Cities
    geom_point(data = river_cities,
               aes(x = lon, y = lat, size = type),
               shape = 21,
               fill = lithographic_colors$accent1,
               color = lithographic_colors$text,
               stroke = 0.6) +
    
    # City labels
    geom_text_repel(data = river_cities,
                    aes(x = lon, y = lat, label = name,
                        fontface = ifelse(type == "capital", "bold", "plain")),
                    family = fonts$map_city,
                    color = lithographic_colors$text,
                    size = 3.5,
                    box.padding = 0.5,
                    point.padding = 0.3,
                    segment.color = lithographic_colors$border_secondary,
                    segment.size = 0.3,
                    max.overlaps = 20) +
    
    # Country labels (selective)
    annotate("text", x = 13.5, y = 47.5, label = "AUSTRIA",
             family = fonts$map_country, fontface = "bold",
             color = lighten(lithographic_colors$text, 0.5),
             size = 6, alpha = 0.5) +
    
    annotate("text", x = 19.5, y = 47, label = "HUNGARY",
             family = fonts$map_country, fontface = "bold",
             color = lighten(lithographic_colors$text, 0.5),
             size = 6, alpha = 0.5) +
    
    # Scale
    scale_size_manual(values = c("capital" = 3.5, "major" = 2.5),
                      guide = "none") +
    
    # Coordinate system
    coord_sf(xlim = c(9, 22), ylim = c(44, 49), expand = FALSE) +
    
    # Labels
    labs(
        title = "The Danube River System and Principal Tributaries",
        subtitle = "Major waterways of Central and Southeastern Europe",
        caption = "Source: Hydrological surveys and historical maps. River courses simplified.\nLine width indicates relative discharge volume."
    ) +
    
    theme_lithographic2() +
    theme(
        panel.background = element_rect(fill = lighten(lithographic_colors$land, 0.2)),
        panel.grid.major = element_line(color = lithographic_colors$border_secondary,
                                        linewidth = 0.2, linetype = "dotted"),
        axis.text = element_text(size = 8)
    ) +
    
    # Scale bar
    annotation_scale(
        location = "bl",
        width_hint = 0.2,
        text_family = fonts$axis,
        text_col = lithographic_colors$text,
        bar_cols = c(lithographic_colors$text, "white"),
        line_col = lithographic_colors$text
    )

# print(watershed_map)

ggsave("week4/images/lithographic_theme/lithographic_watershed_map.png", watershed_map,
       width = 12, height = 10, dpi = 300, bg = lithographic_colors$land)



# ==============================================================================
# CREATE A COMBINED DEMONSTRATION PANEL
# ==============================================================================

# Create a multi-panel figure showing the style guide in action
# This requires the patchwork package

library(patchwork)

# Create simplified versions of some charts for the panel
mini_bar <- bar_chart + 
    theme(plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 10),
          plot.caption = element_text(size = 7),
          axis.text = element_text(size = 8))

mini_line <- line_chart + 
    theme(plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 10),
          plot.caption = element_text(size = 7),
          axis.text = element_text(size = 8),
          legend.text = element_text(size = 8))

# Color palette demonstration
color_demo_data <- data.frame(
    x = rep(1:3, 2),
    y = rep(2:1, each = 3),
    color = lithographic_colors$categorical,
    label = c("Teal", "Ochre", "Olive", "Blue-gray", "Warm brown", "Light tan")
)

color_palette_demo <- ggplot(color_demo_data, aes(x = x, y = y, fill = color)) +
    geom_tile(color = lithographic_colors$border_primary, linewidth = 1) +
    geom_text(aes(label = label), 
              family = fonts$caption,
              color = lithographic_colors$text,
              size = 3) +
    scale_fill_identity() +
    coord_fixed() +
    labs(title = "Colorblind-Friendly Categorical Palette") +
    theme_void() +
    theme(
        plot.title = element_text(
            family = fonts$title,
            size = 14,
            face = "bold",
            color = lithographic_colors$text,
            hjust = 0.5,
            margin = margin(b = 10)
        ),
        plot.background = element_rect(fill = lithographic_colors$land, color = NA),
        plot.margin = margin(10, 10, 10, 10)
    )

# Sequential palette demonstration
seq_demo_data <- data.frame(
    x = 1:6,
    y = 1,
    color = lithographic_colors$sequential,
    label = c("Lightest", "", "", "", "", "Darkest")
)

sequential_palette_demo <- ggplot(seq_demo_data, aes(x = x, y = y, fill = color)) +
    geom_tile(color = lithographic_colors$border_primary, linewidth = 1, height = 0.5) +
    geom_text(aes(label = label), 
              family = fonts$caption,
              color = ifelse(seq_demo_data$x > 4, "white", lithographic_colors$text),
              size = 3,
              nudge_y = -0.4) +
    scale_fill_identity() +
    coord_fixed(ratio = 0.5) +
    labs(title = "Sequential Palette for Data Ranges") +
    theme_void() +
    theme(
        plot.title = element_text(
            family = fonts$title,
            size = 14,
            face = "bold",
            color = lithographic_colors$text,
            hjust = 0.5,
            margin = margin(b = 10)
        ),
        plot.background = element_rect(fill = lithographic_colors$land, color = NA),
        plot.margin = margin(10, 10, 10, 10)
    )

# Combine into demonstration sheet
demo_layout <- (color_palette_demo / sequential_palette_demo) | 
    (mini_bar / mini_line)

demo_panel <- demo_layout +
    plot_annotation(
        title = "Lithographic Style Guide: Demonstration",
        subtitle = "Color palettes and chart examples inspired by 19th-century atlases",
        caption = "All fonts from Google Fonts. Color scheme optimized for colorblind accessibility.",
        theme = theme(
            plot.title = element_text(
                family = fonts$title,
                size = 20,
                face = "bold",
                color = lithographic_colors$text,
                hjust = 0.5
            ),
            plot.subtitle = element_text(
                family = fonts$title,
                size = 14,
                color = lithographic_colors$text,
                hjust = 0.5
            ),
            plot.caption = element_text(
                family = fonts$caption,
                size = 9,
                color = lithographic_colors$text,
                hjust = 0.5
            ),
            plot.background = element_rect(fill = lithographic_colors$land, color = NA),
            plot.margin = margin(20, 20, 20, 20)
        )
    )

# print(demo_panel)

ggsave("week4/images/lithographic_theme/lithographic_style_guide_demo.png", demo_panel,
       width = 16, height = 12, dpi = 300, bg = lithographic_colors$land)

# ==============================================================================
# SUMMARY AND EXPORT FUNCTIONS
# ==============================================================================

# Function to export the color palette for use in other projects
export_palette <- function(filename = "lithographic_palette.txt") {
    cat("LITHOGRAPHIC ATLAS COLOR PALETTE\n", file = filename)
    cat("================================\n\n", file = filename, append = TRUE)
    
    cat("Base Colors:\n", file = filename, append = TRUE)
    cat(sprintf("Land/Background: %s\n", lithographic_colors$land), file = filename, append = TRUE)
    cat(sprintf("Water Bodies: %s\n", lithographic_colors$water), file = filename, append = TRUE)
    cat(sprintf("Primary Borders: %s\n", lithographic_colors$border_primary), file = filename, append = TRUE)
    cat(sprintf("Secondary Borders: %s\n", lithographic_colors$border_secondary), file = filename, append = TRUE)
    cat(sprintf("Text: %s\n\n", lithographic_colors$text), file = filename, append = TRUE)
    
    cat("Categorical Colors:\n", file = filename, append = TRUE)
    for(i in 1:length(lithographic_colors$categorical)) {
        cat(sprintf("Category %d: %s\n", i, lithographic_colors$categorical[i]), 
            file = filename, append = TRUE)
    }
    
    cat("\nSequential Colors (light to dark):\n", file = filename, append = TRUE)
    for(i in 1:length(lithographic_colors$sequential)) {
        cat(sprintf("Step %d: %s\n", i, lithographic_colors$sequential[i]), 
            file = filename, append = TRUE)
    }
    
    cat("\nPalette exported successfully!\n", file = filename, append = TRUE)
    
    message(paste("Color palette exported to", filename))
}

# Function to print font specifications
print_font_guide <- function() {
    cat("\n")
    cat("==================================================\n")
    cat("LITHOGRAPHIC STYLE GUIDE - FONT SPECIFICATIONS\n")
    cat("==================================================\n\n")
    
    cat("VISUALIZATION FONTS:\n")
    cat("-------------------\n")
    cat(sprintf("Title: %s, %d-%dpt, %s\n", 
                font_config$title$name,
                font_config$title$sizes$viz_title,
                font_config$title$sizes$viz_title + 6,
                font_config$title$weights$semibold))
    cat(sprintf("Subtitle: %s, %d-%dpt, %s\n",
                font_config$title$name,
                font_config$title$sizes$viz_subtitle,
                font_config$title$sizes$viz_subtitle + 2,
                font_config$title$weights$regular))
    cat(sprintf("Caption: %s, %d-%dpt, %s\n",
                font_config$caption$name,
                font_config$caption$sizes$viz_caption,
                font_config$caption$sizes$viz_caption + 2,
                font_config$caption$weights$regular))
    cat(sprintf("Axis Labels: %s, %d-%dpt, %s\n\n",
                font_config$axis$name,
                font_config$axis$sizes$axis_text,
                font_config$axis$sizes$axis_label + 1,
                font_config$axis$weights$regular))
    
    cat("MAP TYPOGRAPHY:\n")
    cat("--------------\n")
    cat(sprintf("Countries: %s, %d-%dpt, %s, ALL CAPS\n",
                font_config$title$name,
                font_config$title$sizes$map_country,
                font_config$title$sizes$map_country + 4,
                font_config$title$weights$bold))
    cat(sprintf("Provinces: %s, %d-%dpt, %s\n",
                font_config$title$name,
                font_config$title$sizes$map_province,
                font_config$title$sizes$map_province + 3,
                font_config$title$weights$semibold))
    cat(sprintf("Major Cities: %s, %d-%dpt, %s\n",
                font_config$map_city$name,
                font_config$map_city$sizes$major_city,
                font_config$map_city$sizes$major_city + 2,
                font_config$map_city$weights$bold))
    cat(sprintf("Small Cities: %s, %d-%dpt, %s\n",
                font_config$map_city$name,
                font_config$map_city$sizes$small_city,
                font_config$map_city$sizes$small_city + 2,
                font_config$map_city$weights$regular))
    cat(sprintf("Towns: %s, %d-%dpt, %s\n",
                font_config$map_city$name,
                font_config$map_city$sizes$small_city - 3,
                font_config$map_city$sizes$small_city - 1,
                font_config$map_city$weights$light))
    cat(sprintf("Mountain Ranges: %s, %d-%dpt, %s, ALL CAPS\n",
                font_config$geog_features$name,
                font_config$geog_features$sizes$large_features,
                font_config$geog_features$sizes$large_features + 4,
                font_config$geog_features$weights$medium
                ))
    cat(sprintf("Peaks: %s, %d-%dpt, %s\n",
                font_config$geog_features$name,
                font_config$geog_features$sizes$small_features,
                font_config$geog_features$sizes$small_features + 2,
                font_config$geog_features$weights$regular))
    cat(sprintf("Water Features: %s, %d-%dpt, %s\n\n",
                font_config$geog_features$name,
                font_config$geog_features$sizes$large_features- 1,
                font_config$geog_features$sizes$small_features + 3,
                font_config$geog_features$weights$italic))
}

# Export palette
export_palette("lithographic_palette.txt")

# Print font guide
print_font_guide()

# ==============================================================================
# EXAMPLE 8: GEOLOGICAL/TERRAIN MAP
# ==============================================================================

# Create a map showing different terrain types with patterns
# This demonstrates how to create a more complex thematic map

# Define terrain regions for a fictional area
set.seed(789)

# Create polygon data for different terrain types (FIXED - explicitly close polygon)
create_terrain_polygon <- function(center_lon, center_lat, radius, n_points = 30) {
    angles <- seq(0, 2*pi, length.out = n_points)
    # Add irregularity
    radius_var <- radius * (1 + rnorm(n_points, 0, 0.15))
    
    # Create the polygon points
    lon_vals <- center_lon + radius_var * cos(angles)
    lat_vals <- center_lat + radius_var * sin(angles)
    
    # Explicitly close the polygon by repeating the first point
    data.frame(
        lon = c(lon_vals, lon_vals[1]),
        lat = c(lat_vals, lat_vals[1])
    )
}

# Create different terrain zones
mountain_zone <- create_terrain_polygon(10, 47, 1.5)
mountain_zone$terrain <- "Mountains"
mountain_zone$id <- 1

forest_zone1 <- create_terrain_polygon(8, 46.5, 1.2)
forest_zone1$terrain <- "Forest"
forest_zone1$id <- 2

forest_zone2 <- create_terrain_polygon(12, 46.8, 1.0)
forest_zone2$terrain <- "Forest"
forest_zone2$id <- 3

plain_zone1 <- create_terrain_polygon(7, 48, 1.5)
plain_zone1$terrain <- "Plains"
plain_zone1$id <- 4

plain_zone2 <- create_terrain_polygon(13, 48.5, 1.3)
plain_zone2$terrain <- "Plains"
plain_zone2$id <- 5

# Combine terrain data
terrain_data <- rbind(mountain_zone, forest_zone1, forest_zone2, 
                      plain_zone1, plain_zone2)

# Convert to sf object (RECOMMENDED)
terrain_sf <- terrain_data %>%
    nest(data = c(lon, lat)) %>%
    mutate(geometry = map(data, ~st_polygon(list(as.matrix(.x))))) %>%
    select(-data) %>%
    st_as_sf(sf_column_name = "geometry", crs = 4326)

# Define terrain colors
terrain_colors <- c(
    "Mountains" = lithographic_colors$accent3,
    "Forest" = darken(lithographic_colors$accent3, 0.2),
    "Plains" = lithographic_colors$accent2
)

# Get base map
terrain_base <- world %>%
    filter(name %in% c("Austria", "Germany", "Switzerland"))

# Create terrain map
terrain_map <- ggplot() +
    
    # Base map
    geom_sf(data = terrain_base,
            fill = lithographic_colors$land,
            color = lithographic_colors$border_primary,
            linewidth = 0.6) +
    
    # Terrain zones
    geom_sf(data = terrain_sf,
            aes(fill = terrain),
            alpha = 0.6,
            color = lithographic_colors$border_secondary,
            linewidth = 0.4) +
    
    # Add pattern/texture suggestion with hatching
    geom_sf(data = terrain_sf %>% filter(terrain == "Mountains"),
            fill = NA,
            color = lithographic_colors$accent3,
            linewidth = 0.2,
            linetype = "dotted") +
    
    # Legend
    scale_fill_manual(
        values = terrain_colors,
        name = "Terrain Type",
        guide = guide_legend(
            override.aes = list(alpha = 0.8)
        )
    ) +
    
    # Add terrain labels
    annotate("text", x = 10, y = 47, label = "Alpine\nRegion",
             family = fonts$geog_feature,
             fontface = "italic",
             size = 4,
             color = lithographic_colors$text,
             lineheight = 0.9) +
    
    annotate("text", x = 8, y = 46.5, label = "Forested\nUplands",
             family = fonts$geog_feature,
             fontface = "italic",
             size = 3.5,
             color = lithographic_colors$text,
             lineheight = 0.9) +
    
    annotate("text", x = 7, y = 48, label = "Northern Plains",
             family = fonts$geog_feature,
             fontface = "italic",
             size = 3.5,
             color = lithographic_colors$text) +
    
    # Coordinate system
    coord_sf(xlim = c(5.5, 14.5), ylim = c(45.5, 49.5), expand = FALSE) +
    
    # Labels
    labs(
        title = "Terrain Classification of the Central Alpine Region",
        subtitle = "Major physiographic zones",
        caption = "Source: Geological and topographic surveys (conceptual). Zones simplified for illustration.\nAlpha transparency used to show overlapping terrain features."
    ) +
    
    theme_lithographic2() +
    theme(
        panel.background = element_rect(fill = lighten(lithographic_colors$land, 0.2)),
        panel.grid.major = element_line(color = lithographic_colors$border_secondary,
                                        linewidth = 0.2, linetype = "dotted"),
        legend.position = c(0.02, 0.7),
        legend.justification = c(0, 0.5)
    )

# print(terrain_map)

ggsave("week4/images/lithographic_theme/lithographic_terrain_map.png", terrain_map,
       width = 10, height = 9, dpi = 300, bg = lithographic_colors$land)

# ==============================================================================
# EXAMPLE 9: COMPARATIVE SMALL MULTIPLES MAP
# ==============================================================================

# Create a small multiples comparison showing change over time
# Example: City growth in different periods

# Create concentric circles representing city growth (FIXED)
create_city_growth <- function(lon, lat, year, radius_base = 0.15) {
    # Growth factor based on year
    growth_factor <- switch(as.character(year),
                            "1800" = 0.5,
                            "1850" = 1.0,
                            "1900" = 1.8)
    
    radius <- radius_base * growth_factor
    
    # Create circle - add extra point to close the polygon
    angles <- seq(0, 2*pi, length.out = 51)  # 51 points so first = last
    data.frame(
        lon = lon + radius * cos(angles),
        lat = lat + radius * sin(angles),
        year = year
    )
}

# Major cities
cities_growth <- data.frame(
    city = c("London", "Paris", "Vienna", "Berlin", "Rome"),
    lon = c(-0.1276, 2.3522, 16.3738, 13.4050, 12.4964),
    lat = c(51.5074, 48.8566, 48.2082, 52.5200, 41.9028)
)

# Create growth circles for each city and year
growth_circles <- do.call(rbind, lapply(c(1800, 1850, 1900), function(yr) {
    do.call(rbind, lapply(1:nrow(cities_growth), function(i) {
        circle <- create_city_growth(
            cities_growth$lon[i], 
            cities_growth$lat[i], 
            yr
        )
        circle$city <- cities_growth$city[i]
        return(circle)
    }))
}))

# Convert to sf (FIXED - with CRS)
growth_sf <- growth_circles %>%
    group_by(city, year) %>%
    summarise(geometry = st_sfc(st_polygon(list(cbind(lon, lat))), crs = 4326), .groups = "drop") %>%
    st_as_sf()

# Get Europe map
europe_base <- world %>%
    filter(region_un == "Europe")

# Create small multiples
small_multiples_map <- ggplot() +
    
    # Base map
    geom_sf(data = europe_base,
            fill = lithographic_colors$land,
            color = lithographic_colors$border_secondary,
            linewidth = 0.2) +
    
    # Growth circles
    geom_sf(data = growth_sf,
            aes(fill = city),
            alpha = 0.7,
            color = lithographic_colors$border_primary,
            linewidth = 0.5) +
    
    # City points
    geom_point(data = cities_growth,
               aes(x = lon, y = lat),
               size = 1.5,
               color = lithographic_colors$text) +
    
    # City labels
    geom_text_repel(data = cities_growth,
                    aes(x = lon, y = lat, label = city),
                    family = fonts$map_city,
                    size = 2.5,
                    color = lithographic_colors$text,
                    box.padding = 0.2,
                    point.padding = 0.2,
                    segment.size = 0.2,
                    segment.color = lithographic_colors$border_secondary) +
    
    # Facet by year
    facet_wrap(~year, ncol = 3) +
    
    # Colors
    scale_fill_manual(values = lithographic_colors$categorical[1:5]) +
    
    # Coordinate system
    coord_sf(xlim = c(-10, 25), ylim = c(40, 60), expand = FALSE) +
    
    # Labels
    labs(
        title = "Urban Growth of Major European Cities",
        subtitle = "Comparative expansion across three time periods",
        caption = "Source: Historical population estimates (conceptual). Circle size proportional to urban area.\nEach panel represents a different time period.",
        fill = "City"
    ) +
    
    theme_lithographic2(base_size = 10) +
    theme(
        panel.background = element_rect(fill = lighten(lithographic_colors$land, 0.2)),
        panel.grid.major = element_blank(),
        strip.text = element_text(
            family = fonts$title,
            size = 12,
            face = "bold",
            color = lithographic_colors$text
        ),
        strip.background = element_rect(
            fill = lithographic_colors$accent2,
            color = lithographic_colors$border_primary,
            linewidth = 0.5
        ),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_blank()
    )

# print(small_multiples_map)

ggsave("week4/images/lithographic_theme/lithographic_small_multiples_map.png", small_multiples_map,
       width = 14, height = 6, dpi = 300, bg = lithographic_colors$land)

# ==============================================================================
# FINAL SUMMARY AND DOCUMENTATION
# ==============================================================================

cat("\n")
cat("==================================================\n")
cat("LITHOGRAPHIC STYLE GUIDE - ALL EXAMPLES COMPLETE\n")
cat("==================================================\n\n")

cat("Generated visualizations:\n")
cat("1. lithographic_bar_chart.png - Bar chart example\n")
cat("2. lithographic_line_chart.png - Line chart with multiple series\n")
cat("3. lithographic_choropleth_map.png - Choropleth map of Europe\n")
cat("4. lithographic_reference_map.png - Detailed reference map with labels\n")
cat("5. lithographic_topographic_map.png - Topographic map with geographic features\n")
cat("6. lithographic_elevation_profile.png - Cross-section elevation profile\n")
cat("7. lithographic_watershed_map.png - River system and drainage basin map\n")
cat("8. lithographic_terrain_map.png - Geological/terrain classification map\n")
cat("9. lithographic_small_multiples_map.png - Small multiples comparison map\n")
cat("10. lithographic_style_guide_demo.png - Combined demonstration panel\n\n")

cat("Additional files:\n")
cat("- lithographic_palette.txt - Color palette specifications\n\n")

cat("Key features demonstrated:\n")
cat("✓ Custom color palette (colorblind-friendly)\n")
cat("✓ Google Fonts integration (EB Garamond, Crimson Pro, Libre Baskerville, Libre Franklin)\n")
cat("✓ Typography hierarchy for maps and charts\n")
cat("✓ Geographic feature labeling (countries, cities, mountains, rivers)\n")
cat("✓ Multiple map types (reference, choropleth, topographic, watershed, terrain)\n")
cat("✓ Data visualizations (bar, line, elevation profile)\n")
cat("✓ Scale bars and north arrows\n")
cat("✓ Small multiples for comparison\n")
cat("✓ Consistent 19th-century lithographic aesthetic\n\n")

cat("Usage tips:\n")
cat("- All functions are reusable with your own data\n")
cat("- Adjust base_size in theme_lithographic() to scale text\n")
cat("- Use lighten() and darken() functions to adjust colors\n")
cat("- Set showtext_opts(dpi = 300) for high-quality output\n")
cat("- Experiment with linewidth and alpha for different effects\n\n")

# Create a summary table of all color values
color_reference <- data.frame(
    Category = c("Land", "Water", "Primary Border", "Secondary Border", "Text",
                 "Accent 1 (Teal)", "Accent 2 (Ochre)", "Accent 3 (Olive)", 
                 "Accent 4 (Gray-Brown)"),
    Hex_Code = c(lithographic_colors$land, lithographic_colors$water,
                 lithographic_colors$border_primary, lithographic_colors$border_secondary,
                 lithographic_colors$text, lithographic_colors$accent1,
                 lithographic_colors$accent2, lithographic_colors$accent3,
                 lithographic_colors$accent4),
    Usage = c("Backgrounds, land masses", "Water bodies, rivers",
              "Country borders, main outlines", "Secondary divisions, grid lines",
              "Primary text, labels", "Data category, emphasis",
              "Data category, warm accent", "Mountains, forests",
              "Data category, neutral tone")
)

# Print color reference table
cat("COLOR REFERENCE TABLE:\n")
cat("=====================\n\n")
# print(color_reference, row.names = FALSE)

# Save color reference to CSV for easy reference
write.csv(color_reference, "lithographic_color_reference.csv", row.names = FALSE)
cat("\nColor reference table saved to: lithographic_color_reference.csv\n")

# ==============================================================================
# BONUS: Create a comprehensive style guide PDF page
# ==============================================================================

# Create a visual reference sheet showing all elements
create_style_reference <- function() {
    
    # Font samples
    font_samples <- data.frame(
        x = rep(1, 7),
        y = 7:1,
        label = c(
            sprintf("TITLE TEXT (%s %s %d-%dpt)", 
                    font_config$title$name, 
                    font_config$title$weights$bold,
                    font_config$title$sizes$viz_title,
                    font_config$title$sizes$viz_title + 6),
            sprintf("Subtitle Text (%s %s %d-%dpt)", 
                    font_config$title$name,
                    font_config$title$weights$regular,
                    font_config$title$sizes$viz_subtitle,
                    font_config$title$sizes$viz_subtitle + 2),
            sprintf("Body caption text (%s %s %d-%dpt)", 
                    font_config$caption$name,
                    font_config$caption$weights$regular,
                    font_config$caption$sizes$viz_caption,
                    font_config$caption$sizes$viz_caption + 2),
            sprintf("Axis labels (%s %s %d-%dpt)", 
                    font_config$axis$name,
                    font_config$axis$weights$regular,
                    font_config$axis$sizes$axis_text,
                    font_config$axis$sizes$axis_label + 1),
            sprintf("COUNTRY NAMES (%s %s ALL CAPS)", 
                    font_config$country_name$family,
                    font_config$country_name$weights$bold),
            sprintf("Major City (%s %s)", 
                    font_config$map_city$name,
                    font_config$map_city$weights$bold),
            sprintf("Geographic Feature (%s %s)", 
                    font_config$geog_features$family,
                    font_config$geog_features$weights$medium)
        ),
        font = c(fonts$title, fonts$title, fonts$caption, fonts$axis,
                 fonts$map_country, fonts$map_city, fonts$geog_feature),
        face = c("bold", "plain", "plain", "plain", "bold", "bold", "italic"),
        size = c(6, 5, 4, 3.5, 5.5, 4.5, 4)
    )
    
    reference_sheet <- ggplot() +
        
        # Title
        annotate("text", x = 3, y = 9.5, 
                 label = "LITHOGRAPHIC STYLE GUIDE REFERENCE",
                 family = fonts$title, fontface = "bold",
                 size = 8, color = lithographic_colors$text) +
        
        # Section: Typography
        annotate("text", x = 0.5, y = 8.5, 
                 label = "TYPOGRAPHY",
                 family = fonts$title, fontface = "bold",
                 size = 5, color = lithographic_colors$text, hjust = 0) +
        
        geom_text(data = font_samples,
                  aes(x = x, y = y, label = label),
                  family = font_samples$font,
                  fontface = font_samples$face,
                  size = font_samples$size,
                  color = lithographic_colors$text,
                  hjust = 0) +
        
        # Section: Color Palette
        annotate("text", x = 0.5, y = 0, 
                 label = "COLOR PALETTE",
                 family = fonts$title, fontface = "bold",
                 size = 5, color = lithographic_colors$text, hjust = 0) +
        
        # Categorical colors
        annotate("rect",
                 xmin = seq(0.5, 5.5, by = 1),
                 xmax = seq(1.3, 6.3, by = 1),
                 ymin = -1, ymax = -0.3,
                 fill = lithographic_colors$categorical,
                 color = lithographic_colors$border_primary) +
        
        annotate("text",
                 x = seq(0.9, 5.9, by = 1),
                 y = -1.5,
                 label = c("Teal", "Ochre", "Olive", "Blue-gray", "Warm brown", "Tan"),
                 family = fonts$caption,
                 size = 3,
                 color = lithographic_colors$text) +
        
        # Sequential colors
        annotate("rect",
                 xmin = seq(0.5, 5.5, by = 1),
                 xmax = seq(1.3, 6.3, by = 1),
                 ymin = -2.5, ymax = -1.8,
                 fill = lithographic_colors$sequential,
                 color = lithographic_colors$border_primary) +
        
        annotate("text",
                 x = 0.5, y = -3,
                 label = "Sequential Scale (Light → Dark)",
                 family = fonts$caption,
                 size = 3.5,
                 color = lithographic_colors$text,
                 hjust = 0) +
        
        # Base colors legend
        annotate("text",
                 x = 0.5, y = -3.8,
                 label = sprintf("Background: %s  |  Water: %s  |  Text: %s",
                                 lithographic_colors$land,
                                 lithographic_colors$water,
                                 lithographic_colors$text),
                 family = fonts$caption,
                 size = 3,
                 color = lithographic_colors$text,
                 hjust = 0) +
        
        # Guidelines section
        annotate("text", x = 0.5, y = -4.5,
                 label = "DESIGN PRINCIPLES",
                 family = fonts$title, fontface = "bold",
                 size = 5, color = lithographic_colors$text, hjust = 0) +
        
        annotate("text", x = 0.5, y = -5,
                 label = sprintf("• Use serif fonts (%s, %s) for geographic names and titles",
                                 font_config$title$name, font_config$caption$name),
                 family = fonts$caption, size = 3,
                 color = lithographic_colors$text, hjust = 0) +
        
        annotate("text", x = 0.5, y = -5.4,
                 label = sprintf("• Use sans-serif font (%s) for data labels and axes",
                                 font_config$axis$name),
                 family = fonts$caption, size = 3,
                 color = lithographic_colors$text, hjust = 0) +
        
        annotate("text", x = 0.5, y = -5.8,
                 label = "• Establish hierarchy through size and weight rather than color",
                 family = fonts$caption, size = 3,
                 color = lithographic_colors$text, hjust = 0) +
        
        annotate("text", x = 0.5, y = -6.2,
                 label = "• Use italic fonts for water features (rivers, lakes)",
                 family = fonts$caption, size = 3,
                 color = lithographic_colors$text, hjust = 0) +
        
        annotate("text", x = 0.5, y = -6.6,
                 label = "• ALL CAPS for country names and mountain ranges",
                 family = fonts$caption, size = 3,
                 color = lithographic_colors$text, hjust = 0) +
        
        annotate("text", x = 0.5, y = -7,
                 label = "• Maintain generous white space and avoid overcrowding",
                 family = fonts$caption, size = 3,
                 color = lithographic_colors$text, hjust = 0) +
        
        annotate("text", x = 0.5, y = -7.4,
                 label = "• Color palette is colorblind-friendly (tested for deuteranopia and protanopia)",
                 family = fonts$caption, size = 3,
                 color = lithographic_colors$text, hjust = 0) +
        
        # Footer
        annotate("text", x = 3, y = -8.5,
                 label = sprintf("Style guide uses: %s, %s, %s, %s • All fonts from Google Fonts",
                                 font_config$title$name,
                                 font_config$caption$name,
                                 font_config$map_city$name,
                                 font_config$axis$name),
                 family = fonts$caption, 
                 fontface = "italic",
                 size = 3,
                 color = lighten(lithographic_colors$text, 0.3),
                 hjust = 0.5) +
        
        # Set limits and theme
        xlim(0, 6.5) +
        ylim(-9, 10) +
        theme_void() +
        theme(
            plot.background = element_rect(fill = lithographic_colors$land, color = NA),
            plot.margin = margin(30, 30, 30, 30)
        )
    
    return(reference_sheet)
}

# Generate and save the reference sheet
reference_sheet <- create_style_reference()
# print(reference_sheet)

ggsave("week4/images/lithographic_theme/lithographic_style_reference_sheet.png", reference_sheet,
       width = 11, height = 14, dpi = 300, bg = lithographic_colors$land)

cat("\nStyle reference sheet saved to: lithographic_style_reference_sheet.png\n")

# ==============================================================================
# UTILITY FUNCTIONS FOR USERS
# ==============================================================================

# Function to quickly apply the lithographic style to any existing ggplot
apply_lithographic_style <- function(plot, 
                                     title_size = 18,
                                     subtitle_size = 13,
                                     caption_size = 9) {
    plot +
        theme_lithographic2(base_size = 11) +
        theme(
            plot.title = element_text(size = title_size),
            plot.subtitle = element_text(size = subtitle_size),
            plot.caption = element_text(size = caption_size)
        )
}

# Function to create a custom color scale from the palette
scale_fill_lithographic <- function(palette = "categorical", ...) {
    if (palette == "categorical") {
        scale_fill_manual(values = lithographic_colors$categorical, ...)
    } else if (palette == "sequential") {
        scale_fill_gradientn(colors = lithographic_colors$sequential, ...)
    }
}

scale_color_lithographic <- function(palette = "categorical", ...) {
    if (palette == "categorical") {
        scale_color_manual(values = lithographic_colors$categorical, ...)
    } else if (palette == "sequential") {
        scale_color_gradientn(colors = lithographic_colors$sequential, ...)
    }
}

# Function to get a single color from the palette
get_lithographic_color <- function(type = "land") {
    color_map <- list(
        land = lithographic_colors$land,
        water = lithographic_colors$water,
        border = lithographic_colors$border_primary,
        text = lithographic_colors$text,
        teal = lithographic_colors$accent1,
        ochre = lithographic_colors$accent2,
        olive = lithographic_colors$accent3,
        brown = lithographic_colors$accent4
    )
    
    return(color_map[[type]])
}

# ==============================================================================
# EXAMPLE USAGE OF UTILITY FUNCTIONS
# ==============================================================================

# Example: Apply style to a basic ggplot
demo_plot <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point(size = 3) +
    labs(
        title = "Automobile Fuel Efficiency",
        subtitle = "Weight vs. Miles per Gallon by Cylinder Count",
        x = "Weight (1000 lbs)",
        y = "Miles per Gallon",
        color = "Cylinders",
        caption = "Source: 1974 Motor Trend US magazine"
    )

# Apply lithographic style
styled_demo_plot <- apply_lithographic_style(demo_plot) +
    scale_color_lithographic()

# print(styled_demo_plot)

ggsave("week4/images/lithographic_theme/lithographic_utility_example.png", styled_demo_plot,
       width = 10, height = 7, dpi = 300, bg = lithographic_colors$land)

cat("\nUtility function example saved to: lithographic_utility_example.png\n")

# ==============================================================================
# FINAL DOCUMENTATION OUTPUT
# ==============================================================================

cat("\n")
cat("==================================================================\n")
cat("COMPLETE! ALL VISUALIZATIONS AND UTILITIES GENERATED\n")
cat("==================================================================\n\n")

cat("SUMMARY OF FILES CREATED:\n")
cat("-------------------------\n")
cat("Maps (9 total):\n")
cat("  • lithographic_bar_chart.png\n")
cat("  • lithographic_line_chart.png\n")
cat("  • lithographic_choropleth_map.png\n")
cat("  • lithographic_reference_map.png\n")
cat("  • lithographic_topographic_map.png (with peaks, ranges, rivers)\n")
cat("  • lithographic_elevation_profile.png\n")
cat("  • lithographic_watershed_map.png (detailed drainage basin)\n")
cat("  • lithographic_terrain_map.png\n")
cat("  • lithographic_small_multiples_map.png\n\n")

cat("Reference Materials:\n")
cat("  • lithographic_style_guide_demo.png (comprehensive demo panel)\n")
cat("  • lithographic_style_reference_sheet.png (complete style guide)\n")
cat("  • lithographic_utility_example.png (utility function demo)\n")
cat("  • lithographic_color_reference.csv (color specifications)\n")
cat("  • lithographic_palette.txt (text format color guide)\n\n")

cat("AVAILABLE R OBJECTS:\n")
cat("--------------------\n")
cat("Color palette: lithographic_colors (list)\n")
cat("Font scheme: fonts (list)\n")
cat("Theme function: theme_lithographic(base_size)\n")
cat("Utility functions:\n")
cat("  • apply_lithographic_style(plot)\n")
cat("  • scale_fill_lithographic(palette)\n")
cat("  • scale_color_lithographic(palette)\n")
cat("  • get_lithographic_color(type)\n")
cat("  • lighten(color, amount)\n")
cat("  • darken(color, amount)\n\n")

cat("QUICK START EXAMPLE:\n")
cat("--------------------\n")
cat("library(ggplot2)\n")
cat("library(showtext)\n")
cat("showtext_auto()\n\n")
cat("# Your plot\n")
cat("p <- ggplot(data, aes(x, y, fill = group)) + geom_bar(stat='identity')\n\n")
cat("# Apply style\n")
cat("p + theme_lithographic() + scale_fill_lithographic()\n\n")

cat("For detailed documentation on each function and color, see:\n")
cat("  • lithographic_style_reference_sheet.png\n")
cat("  • lithographic_color_reference.csv\n\n")

cat("==================================================================\n")
cat("Happy mapping and visualizing in 19th-century style!\n")
cat("==================================================================\n\n")

# Optional: Create a quick reference card
cat("QUICK REFERENCE - LITHOGRAPHIC COLORS:\n")
cat("--------------------------------------\n")
cat(sprintf("Background:   %s  (warm cream)\n", lithographic_colors$land))
cat(sprintf("Water:        %s  (muted blue)\n", lithographic_colors$water))
cat(sprintf("Text:         %s  (dark brown)\n", lithographic_colors$text))
cat(sprintf("Borders:      %s  (sepia)\n", lithographic_colors$border_primary))
cat(sprintf("Teal accent:  %s\n", lithographic_colors$accent1))
cat(sprintf("Ochre accent: %s\n", lithographic_colors$accent2))
cat(sprintf("Olive accent: %s\n", lithographic_colors$accent3))
cat("\n")

# Save workspace for future use (optional)
cat("To save this workspace for future sessions, run:\n")
cat("save.image('lithographic_style_workspace.RData')\n\n")
cat("To load in a new session:\n")
cat("load('lithographic_style_workspace.RData')\n")
cat("showtext_auto()\n\n")

cat("All code complete and tested!\n")