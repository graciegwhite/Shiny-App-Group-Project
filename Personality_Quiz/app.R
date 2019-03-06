

library(shiny)
library(tidyverse)
library(shinythemes)
library(readr)
library(ggplot2)
library(beyonce)

##Copy and pasted from last lab, we can change all the details but keep some structure?
# Define UI for application that draws a histogram

#Code for formatting the dataframe:

final_df <- read_csv("cleandata.csv")
Danger_ratings <- c("50.4", "57.6", "2.4", "71.2", "68", "40", "3", "6.4", "69.2", "93.4", "23.4", "35.4", "34.2", "3", NA, "34.8", "29.2", "6.4", NA, "92.2", "5.4", "4.4", "15", "5.6", NA, "13.4", "3", "6", "6", "7.2", "9.4", "69.2", "58.8", "4.4", NA, "40.8", "9", "5.4", "2", "17.8", NA, "65", "8.4", "30.6", "3", "67.8", "25.2", "21.4", "92.6", "17.8", "16.8", "28.4", "23.8")
final_df$Danger_Rating <- Danger_ratings

#datapasta is magical and i can't beleive that worked wow
final_df$Danger_Rating <- as.numeric(final_df$Danger_Rating)
#rename so less complicated
colnames(final_df) <- c("Time", "Name", "Program", "Specialization", "Age", "Astrological Sign", "Home State", "Favorite Color", "Dog vs Cat", "Hogwarts House", "Patronus", "Introvert vs Extrovert", "Myers-Briggs", "Enneagram Type", "Enneagram Wing", "Favorite_R_Color", "Patronus Danger Rating")


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
                        ),
                        mainPanel(
                          plotOutput(outputId = "bar"),
                          uiOutput("secondSelection"))
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
                        
                        mainPanel(
                          # Show a plot of the generated distribution
                          plotOutput(outputId = "scatter"))
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
                          plotOutput("tbd")
                        )
                      ))
             
  )
  
)









# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$secondSelection <- renderUI({
    selectInput(
      "Select",
      "Select Group",
      choices = unique(chase_data==input$column)
    )
  })
  
  output$bar <- renderPlot({
    chase_data %>% 
      filter(Enneagram != "NA") %>% 
      filter(input$column == "secondSelection") %>% 
      ggplot(aes(x = Enneagram, fill = Enneagram)) +
      geom_bar(color = "grey") +
      geom_text(stat = "count", 
                aes(label =..count..), 
                color = "white",
                nudge_y = -0.3,
                size = 3,
                vjust = "inward") +
      coord_polar(theta = "x") +
      theme_minimal() +
      scale_x_discrete(drop=FALSE) +
      scale_y_discrete(drop=FALSE) +
      scale_fill_discrete(drop=FALSE) +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(color = "black", size = 10))
    
  })
  
  
  #panel2 - patronuses
  output$scatter <- renderPlot({
    column <- sym(input$x)
    col <- sym(input$variable)
    
    
    
    ggplot(data = final_df, aes(x = !!column, y = `Patronus Danger Rating`)) +
      geom_point(aes(color = !!col), size = 3) +
      theme_light() +
      scale_color_manual(values = beyonce_palette(129)) +
      labs(title = "Patronus Danger Ratings \n How do you compare?") +
      theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5), plot.title = element_text(hjust = 0.5, face = "bold", size = 15), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))
  })
  
  
  
  
  
  
  
  print("got here")
  
}

# Run the application 
shinyApp(ui = ui, server = server)
