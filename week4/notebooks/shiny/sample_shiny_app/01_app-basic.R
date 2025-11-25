#

# PROMPT 
## RESPONSE: Sonnet 4.5

# Write the R code for a minimal Shiny app in a single app.R file. 
# The app should have a fluidPage layout with a title panel, a sidebar with one slider input,
# and a main panel that displays a basic histogram of random data.

# Install Shiny (if not already installed)
# renv::install("shiny")

# Load required library
library(shiny)

# Define UI
ui <- fluidPage(
    
    # Application title
    titlePanel("Minimal Shiny App Example"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar panel for inputs
        sidebarPanel(
            
            # Slider input for number of observations
            sliderInput(inputId = "n_obs",
                        label = "Number of observations:",
                        min = 10,
                        max = 500,
                        value = 100)
            
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            
            # Output: Histogram
            plotOutput(outputId = "histogram")
            
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    # Render the histogram
    output$histogram <- renderPlot({
        
        # Generate random data based on slider input
        random_data <- rnorm(n = input$n_obs, mean = 0, sd = 1)
        
        # Create histogram
        hist(random_data,
             breaks = 30,
             col = "steelblue",
             border = "white",
             main = "Histogram of Random Normal Data",
             xlab = "Value",
             ylab = "Frequency")
        
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)

# To Run:
## 1. Select "Run App" from this editor window in R Studio, OR
## 2. enter `shiny::runApp() in console`