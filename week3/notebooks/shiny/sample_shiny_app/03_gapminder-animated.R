#

# PROMPT 
## RESPONSE: Sonnet 4.5

# Now modify this code to animate this graph allowing the user to press a play button 
# and the graph then slowly moves from the first year in the dataset (1952) to the last (2007).

## Follow ups - originally adjusting the speed of the animation didn't work
# and required multiple steps to fix it

# Install Shiny (if not already installed)
# renv::install("shiny")

# Load required library
library(shiny)
library(ggplot2)          

gapminder <- read.csv("gapminder.csv")  

# Calculate global min/max values for consistent axes across all years   
x_limits <- range(gapminder$gdpPercap, na.rm = TRUE)
y_limits <- range(gapminder$lifeExp, na.rm = TRUE)

# Get all available years in order          ## NEW ##
all_years <- sort(unique(gapminder$year))

# Define UI
ui <- fluidPage(
    
    # Application title
    titlePanel("Gapminder Data Explorer"),    
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar panel for inputs
        sidebarPanel(
            
            # Dropdown to select year            
            selectInput(inputId = "year_select",
                        label = "Select a year:",
                        choices = all_years,   ## CHANGE ##
                        selected = min(all_years)),           ## CHANGE ##
            
            sliderInput("animation_speed", 
                        "Animation Speed (seconds per year):",
                        min = 1, max = 4, value = 1.5, step = 0.5),
                        
            # Add some spacing                  ## NEW ##
            br(),
            
            # Play/Pause button                 ## NEW ##
            actionButton(inputId = "play_button",
                         label = "Play Animation",
                         icon = icon("play"),
                         class = "btn-primary"),
            
            # Stop button                       ## NEW ##
            actionButton(inputId = "stop_button",
                         label = "Stop",
                         icon = icon("stop")),
            
            # Add some spacing and information  ## NEW ##
            br(),
            br(),
            helpText("Click 'Play Animation' to automatically cycle through years from 1952 to 2007.")
            
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            
            # Output: Scatter plot          
            plotOutput(outputId = "scatterplot", height = "600px")
            
        )
    )
)

# Define server logic
server <- function(input, output, session) {     ## CHANGE - add session ##
    
    # Reactive value to track animation state     ## NEW - down to line 138 just before output$scatteplot ##
    animation_active <- reactiveVal(FALSE)
    
    # Create a reactive timer that fires based on animation speed  ## UPDATE ##
    # We'll create this dynamically
    timer <- reactiveVal(NULL)
    
    # Observer for Play button
    observeEvent(input$play_button, {
        
        # Toggle animation state
        if (!animation_active()) {
            # Start animation
            animation_active(TRUE)
            
            # Change button label to indicate animation is running
            updateActionButton(session, "play_button", 
                               label = "Playing...",
                               icon = icon("pause"))
            
            # Find the current year index
            current_index <- which(all_years == input$year_select)
            
            # If we're at the last year, start from the beginning
            if (current_index == length(all_years)) {
                updateSelectInput(session, "year_select", selected = all_years[1])
            }
        }
    })
    
    # Observer for Stop button
    observeEvent(input$stop_button, {
        animation_active(FALSE)
        updateActionButton(session, "play_button", 
                           label = "Play Animation",
                           icon = icon("play"))
    })
    
    # Create a reactive timer based on the animation speed
    auto_advance_timer <- reactive({
        # Convert seconds to milliseconds
        interval_ms <- input$animation_speed * 1000
        reactiveTimer(intervalMs = interval_ms)()
    })
    
    # Observer that advances the year when timer fires
    observeEvent(auto_advance_timer(), {
        
        # Only advance if animation is active
        if (animation_active()) {
            
            # Get current year index
            current_index <- which(all_years == input$year_select)
            
            # Check if we can advance to the next year
            if (current_index < length(all_years)) {
                
                # Move to next year
                next_index <- current_index + 1
                updateSelectInput(session, "year_select", 
                                  selected = all_years[next_index])
                
            } else {
                # Reached the end, stop animation
                animation_active(FALSE)
                updateActionButton(session, "play_button", 
                                   label = "Play Animation",
                                   icon = icon("play"))
            }
        }
    })
    
     
    # Render the scatter plot              
    output$scatterplot <- renderPlot({
        
        # Filter data based on selected year      
        filtered_data <- gapminder[gapminder$year == input$year_select, ]
        
        # Create scatter plot                     
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
                  plot.title = element_text(size = 16, face = "bold"),
                  plot.title.position = "plot"                           ## NEW ##
                  )
        
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)

# To Run:
## 1. Select "Run App" from this editor window in R Studio, OR
## 2. enter `shiny::runApp() in console`