

library(shiny)
library(tidyverse)
library(shinythemes)
library(readr)

##Copy and pasted from last lab, we can change all the details but keep some structure?
# Define UI for application that draws a histogram

#Code for formatting the dataframe:

final_df <- read_csv("updatedfinal.csv")
Danger_ratings <- c("50.4", "57.6", "2.4", "71.2", "68", "40", "3", "6.4", "69.2", "93.4", "23.4", "35.4", "34.2", "3", NA, "34.8", "29.2", "6.4", NA, "92.2", "5.4", "4.4", "15", "5.6", NA, "13.4", "3", "6", "6", "7.2", "9.4", "69.2", "58.8", "4.4", NA, "40.8", "9", "5.4", "2", "17.8", NA, "65", "8.4", "30.6", "3", "67.8", "25.2", "21.4", "92.6", "17.8", "16.8", "28.4", "23.8")
final_df$Danger_Rating <- Danger_ratings

#datapasta is magical and i can't beleive that worked wow
final_df$Danger_Rating <- as.numeric(final_df$Danger_Rating)

colnames(final_df) <- c("Time", "Name", "Program", "Specialization", "Age", "Astrological Sign", "Home State", "Favorite Color", "Cat vs Dog", "Hogwarts House", "Patronus", "Introvert vs Extrovert", "Myers-Briggs", "Enneagram Type", "Enneagram Wing", "Favorite R Color", "Patronus Danger Rating")




ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Bren Student Personality Quiz Data"),
  
  navbarPage("Expecto Enneagram!",
             
             tabPanel("Summary",
                      h1("Welcome!"),
                      h2("How it all began..."),
                      p("Then some paragraph text."),
                      p("Followed by another paragraph of text..."),
                      h1("Here's a quick snapshot of our data:"),
                      p("You get the idea...)"),
                      h2("Specialization Responses"),
                      h2("Hogwarts House Responses"),
                      h2("Enneagram Types"),
                      h2("Astrology Signs")
                      
             ),
             
             tabPanel("Enneagram Types",
                  
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("radio",
                                      inputId = "column",
                                      label = h3("Select Category"),
                                      choices = list("Age", "Astrologial Sign", "Bren Specialization", "Dog or Cat", "Extroverted or Introverted", "Favorite Color", "Hogwarts House", "Myers-Briggs Personality Type", "Year")
                                      )
                          uiOutput("secondSelection")
                        )
                      )),
             
             
             tabPanel("Patronuses",
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioButtons(
                                       inputId = "x",
                                       label = "Select one:",
                                       choices = c("Enneagram Type","Myers-Briggs","Specialization", "Astrological Sign"), 
                                       selected = "Myers-Briggs"),
                          radioButtons(
                                       inputId = "variable",
                                       label = "Select one:",
                                       choices = c("Hogwarts House", "Dog vs Cat", "Introvert vs Extrovert"),
                                       selected = "Hogwarts House")
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput(outputId = "scatter")
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
  
  output$secondSelection <- renderUI({
    selectInput
  })
  
  
  #panel2 - patronuses
  output$scatter <- renderPlot({
    
    ggplot(data = final_df, aes_string(x = input$x, y = `Patronus Danger Rating`)) +
      geom_point(aes(color = input$variable))
      
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)