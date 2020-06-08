# Attach necessary packages.
library(tidyverse)
library(shiny)
library(shinythemes)

# Load in the necessary datasets.
spooky <- read_csv("spooky_data.csv")

# Create the user interface:
ui <- fluidPage(
  # Edit the theme style:
  theme = shinytheme("flatly"),
  # Add a title panel:
  titlePanel("What a cool title!"),
  # Add a sidebar/main panel layout:
  sidebarLayout(
    sidebarPanel("put widgets here",
                 # Add in a dropdown selection menu widget: "inputID" will indicate how you refer to the widget selections later in the server, "label" will add a label to the top of the widget, and "choices" will indicate the inputs of the variable you're going to use it to explore/update based on.
                 selectInput(inputId = "state_select",
                             label = "Choose a state",
                             choices = unique(spooky$state)# The unique() function makes a vector with all of the unique entries in the state column to use as our choises (instead of typing them out manually).
                             ),
                 # Add in a radio button widget to select a region in which the user can explore costumes.
                 radioButtons(inputId = "region_select",
                              label = "Choose region:",
                              choices = unique(spooky$region_us_census))
                 ),
    mainPanel("put outputs here",
              # Below, are listed all of the outputs from the server function.
              p("State's top candies:"),
              tableOutput(outputId = "candy_table"),
              p("Region's top costumes:"),
              plotOutput(outputId = "costume_graph")
              )
  )
)

# Create the server function:
server <- function(input, output) {
  # Create a reactive subset of 'spooky' that only includes the state selected by the widget 'state_select.'
  state_candy <- reactive({
    spooky %>%
      filter(state == input$state_select) %>%
      select(candy, pounds_candy_sold)
  })
  # Create a reactive table of the top 3 candies for that state by amount sold (lbs).
  output$candy_table <- renderTable({
    state_candy()
  })
  # Create a reactive subset of 'spooky' that only includes the region selected by the widget 'region_select.'
  region_costume <- reactive({
    spooky %>%
      filter(region_us_census == input$region_select) %>%
      count(costume, rank)
  })
  # Create a reactive graph of the top 3 costumes for that region.
  output$costume_graph <- renderPlot({
    ggplot(region_costume(), aes(x = costume, y = n)) +
      geom_col(aes(fill = rank)) +
      coord_flip() +
      scale_fill_manual(values=
                          c("black", "purple", "orange")) +
      theme_minimal()
  })
}
# Note: Reactive objects require parentheses after the object name (like functions). So, for example, above we have written state_candy() instead of state_candy.
# Note #2: Every output in the server needs to be called in ui in order to play out in the app.

# Combine them into an app:
shinyApp(ui = ui, server = server)
# When you run the above line, the shiny app should open up in a new window.

