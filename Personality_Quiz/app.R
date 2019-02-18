

library(shiny)
library(tidyverse)
library(shinythemes)

##Copy and pasted from last lab, we can change all the details but keep some structure?
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Bren Student Personality Quiz Data"),
  
  navbarPage("Expecto Enneagram!",
             
             tabPanel("Summary",
                      h1("Welcome!"),
                      h2("How it all began..."),
                      p("Then some paragraph text. Old Faithful Geyser Data Description: Waiting time between eruptions and the duration of the eruption for the Old Faithful geyser in Yellowstone National Park, Wyoming, USA."),
                      p("Followed by another paragraph of text..."),
                      h1("Here's a quick snapshot of our data:"),
                      p("You get the idea...)"),
                      h2("Specialization Responses"),
                      h2("Hogwarts House Responses"),
                      h2("Enneagram Types"),
                      h2("Astrology Signs")
                      
             ),
             
             tabPanel("Enneagram Types",
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("bins",
                                      "Number of bins:",
                                      min = 1,
                                      max = 50,
                                      value = 30),
                          
                          selectInput("color", 
                                      "Select histogram color:",
                                      choices = c("purple","blue","orange"))
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("distPlot")
                        )
                      )),
             
             
             tabPanel("Patronuses",
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioButtons("scattercolor", 
                                       "Select scatterplot color:",
                                       choices = c("red","blue","gray50"))
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("scatter")
                        )
                      )),
             tabPanel("Spatial Data",
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioButtons("scattercolor", 
                                       "Select scatterplot color:",
                                       choices = c("red","blue","gray50"))
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("scatter")
                        )
                      ))
             
  )
  
)









# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = input$color, border = 'white')
  })
  
  output$scatter <- renderPlot({
    
    ggplot(faithful, aes(x = waiting, y = eruptions)) +
      geom_point(color = input$scattercolor) +
      geom_smooth(method = "lm", se = FALSE)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)