#

# PROMPT 
## RESPONSE: Sonnet 4.5

# Revise this minimal app to import a small dataset called “gapminder.csv” and 
# create a basic scatter plot where each dot or bubble is a country country column, 
# per capita GDP gdpPercap is on the x axis, life expectancy lifeExp is on the y axis, 
# population pop is represented by the size of each semitransparent bubble, and color 
# encodes the continent in which country is found continent. The user must first select 
# a year from the yearcolumn as a filter.

# FOLLOW UP
# Can you revise so that all scatter plots, from 1952 to 2007 (the years included in this dataset), show the same x and y extents. This will allow easier comparison as the user switches between years.

# Install Shiny (if not already installed)
# renv::install("shiny")

# Load required library
library(shiny)
library(ggplot2)          ## NEW ##

gapminder <- read.csv("gapminder.csv")  ## NEW ##

# Calculate global min/max values for consistent axes across all years   ## NEW ##
x_limits <- range(gapminder$gdpPercap, na.rm = TRUE)
y_limits <- range(gapminder$lifeExp, na.rm = TRUE)

# Define UI
ui <- fluidPage(
    
    # Application title
    titlePanel("Gapminder Data Explorer"),    ## CHANGE ##
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar panel for inputs
        sidebarPanel(
            
            # Dropdown to select year            ## REPLACE ##
            selectInput(inputId = "year_select",
                        label = "Select a year:",
                        choices = sort(unique(gapminder$year)),
                        selected = max(gapminder$year))
            
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            
            # Output: Scatter plot           ## REPLACE ##
            plotOutput(outputId = "scatterplot", height = "600px")
            
        )
    )
)

# Define server logic
server <- function(input, output) {
     
    # Render the scatter plot              ## REPLACE ##
    output$scatterplot <- renderPlot({
        
        # Filter data based on selected year      ## REPLACE ##
        filtered_data <- gapminder[gapminder$year == input$year_select, ]
        
        # Create scatter plot                     ## REPLACE ##
        ggplot(filtered_data, aes(x = gdpPercap, 
                                  y = lifeExp, 
                                  size = pop, 
                                  color = continent)) +
            geom_point(alpha = 0.6) +
            scale_size_continuous(range = c(2, 20), 
                                  labels = scales::comma,
                                  name = "Population") +
            scale_x_log10(limits = x_limits,
                          labels = scales::comma) +
            scale_y_continuous(limits = y_limits) +
            labs(title = paste("Life Expectancy vs. GDP Per Capita in", input$year_select),
                 x = "GDP Per Capita (log scale)",
                 y = "Life Expectancy (years)",
                 color = "Continent") +
            theme_minimal() +
            theme(legend.position = "right",
                  plot.title = element_text(size = 16, face = "bold"))
        
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)

# To Run:
## 1. Select "Run App" from this editor window in R Studio, OR
## 2. enter `shiny::runApp() in console`