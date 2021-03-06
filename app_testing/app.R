

library(shiny)
library(tidyverse)
library(shinythemes)
library(readr)
library(ggplot2)

##Copy and pasted from last lab, we can change all the details but keep some structure?
# Define UI for application that draws a histogram

#Code for formatting the dataframe:

final_df <- read_csv("cleandata.csv")
Danger_ratings <- c("50.4", "57.6", "2.4", "71.2", "68", "40", "3", "6.4", "69.2", "93.4", "23.4", "35.4", "34.2", "3", NA, "34.8", "29.2", "6.4", NA, "92.2", "5.4", "4.4", "15", "5.6", NA, "13.4", "3", "6", "6", "7.2", "9.4", "69.2", "58.8", "4.4", NA, "40.8", "9", "5.4", "2", "17.8", NA, "65", "8.4", "30.6", "3", "67.8", "25.2", "21.4", "92.6", "17.8", "16.8", "28.4", "23.8")
final_df$Danger_Rating <- Danger_ratings

#datapasta is magical and i can't beleive that worked wow
final_df$Danger_Rating <- as.numeric(final_df$Danger_Rating)
#rename so less complicated
colnames(final_df) <- c("Time", "Name", "Program", "Specialization", "Age", "Astrological_Sign", "Home_State", "Favorite_Color", "Dog_vs_Cat", "Hogwarts_House", "Patronus", "Introvert_vs_Extrovert", "Myers-Briggs", "Enneagram_Type", "Enneagram_Wing", "Favorite_R_Color", "Patronus_Danger_Rating")

hog_Icons <- icons(
  iconUrl = ifelse(df_map$`What is your Hogwarts House?` == "Hufflepuff",
                   "https://vignette.wikia.nocookie.net/pottermore/images/5/5e/Hufflepuff_crest.png/revision/latest/scale-to-width-down/180?cb=20111112232427",
                   ifelse(df_map$`What is your Hogwarts House?`== "Gryffindor", "http://leafletjs.com/examples/custom-icons/leaf-red.png",
                          ifelse(df_map$`What is your Hogwarts House?`== "Slytherin", "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                                 "https://www.seekpng.com/png/small/152-1521662_harry-potter-ravenclaw-crest-mens-crewneck-sweatshirt-harry.png"))
                   
  ),
  iconWidth = 17, iconHeight = 30,
  iconAnchorX = 17, iconAnchorY = 30
)

df_map <- read_csv("updatedfinal_AP_testing.csv") 

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
                                       choices = list("Age", "Astrological Sign", "Bren Specialization", "Dog or Cat", "Extroverted or Introverted", "Favorite Color", "Hogwarts House", "Myers-Briggs Personality Type", "Year")
                          ),
                          uiOutput("secondSelection")
                        ),
                        mainPanel(
                          plotOutput(outputId = "bar")
                        )
                      )),
             
             
             tabPanel("Patronuses",
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioButtons(
                            inputId = "x",
                            label = "Select one:",
                            choices = c("Enneagram_Type","Myers-Briggs","Specialization", "Astrological_Sign"), 
                            selected = "Myers-Briggs"),
                          radioButtons(
                            inputId = "variable",
                            label = "Select one:",
                            choices = c("Hogwarts_House", "Dog_vs_Cat", "Introvert_vs_Extrovert"),
                            selected = "Hogwarts_House")
                        ),
                        
                        mainPanel(
                          # Show a plot of the generated distribution
                          plotOutput(outputId = "scatter"))
                      )),
             
             
             tabPanel("Spatial Data",
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioButtons("radio",
                                       inputId = "column",
                                       label = h3("Where are my Hogwarts Housemates?"),
                                       choices = list("Gryffindor", "Hufflepuff", "Slytherin", "Ravenclaw")
                          ),
                          uiOutput("map")
                        ),
                        
                        #Show a map of Hogwarts People
                        mainPanel(
                          leafletOutput(outputId = "map")
                        )
                      ))
             
  )
  
)









# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$secondSelection <- renderUI({
    selectInput(
      inputId = "group",
      "Select",
      "Select Group",
      choices = unique(chase_data[,input$column])
    )
  })
  
  output$bar <- renderPlot({
    chase_data %>% 
      filter(Enneagram != "NA") %>% 
      filter(input$column == input$group) %>% 
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
    
    
    
    ggplot(data = final_df, aes(x = !!column, y = Patronus_Danger_Rating)) +
      geom_point(aes(color = !!col))
  })
  
  #Panel3 - map
  output$map <- renderLeaflet({
    df_map %>% 
      leaflet() %>% 
      addTiles() %>% 
      addMarkers(lng = df_map$Longitude, lat = df_map$Latitude, icon = hog_Icons)
  })
  
  
  
  
  
  print("got here")
  
}

# Run the application 
shinyApp(ui = ui, server = server)


############################

library(shiny)
library(tidyverse)
library(shinythemes)
library(readr)
library(ggplot2)
library(beyonce)
library(plotly)

##Copy and pasted from last lab, we can change all the details but keep some structure?
# Define UI for application that draws a histogram

#Code for formatting the dataframe:

final_df <- read_csv("cleandata.csv")
Danger_ratings <- c("50.4", "57.6", "2.4", "71.2", "68", "40", "3", "6.4", "69.2", "93.4", "23.4", "35.4", "34.2", "3", NA, "34.8", "29.2", "6.4", NA, "92.2", "5.4", "4.4", "15", "5.6", NA, "13.4", "3", "6", "6", "7.2", "9.4", "69.2", "58.8", "4.4", NA, "40.8", "9", "5.4", "2", "17.8", NA, "65", "8.4", "30.6", "3", "67.8", "25.2", "21.4", "92.6", "17.8", "16.8", "28.4", "23.8")
final_df$Danger_Rating <- Danger_ratings

#datapasta is magical and i can't beleive that worked wow
final_df$Danger_Rating <- as.numeric(final_df$Danger_Rating)
final_df$`What is your Enneagram personalty type?` <- as.character(final_df$`What is your Enneagram personalty type?`)
#rename so less complicated
colnames(final_df) <- c("Time", "Name", "Program", "Specialization", "Age", "Astrological Sign", "Home State", "Favorite Color", "Dog vs Cat", "Hogwarts House", "Patronus", "Introvert vs Extrovert", "Myers-Briggs", "Enneagram Type", "Enneagram Wing", "Favorite_R_Color", "Patronus Danger Rating", "Latitude", "Longitude")
# clean data
clean_df <- read_csv("cleandata.csv")

clean_df$Danger_rating <- Danger_ratings

colnames(clean_df) <- c("Time", "Name", "Program", "Specialization", "Age", "Astrological Sign", "Home State", "Favorite Color", "Dog vs Cat", "Hogwarts House", "Patronus", "Introvert vs Extrovert", "Myers-Briggs", "Enneagram Type", "Enneagram Wing", "Favorite_R_Color", "Patronus Danger Rating", "Latitude", "Longitude")

chase_data <- clean_df

clean_df$`Enneagram Type` <- as.character(clean_df$`Enneagram Type`)
chase_data$`Enneagram Type` <- as.character(chase_data$`Enneagram Type`)

hog_Icons <- icons(
  iconUrl = ifelse(df_map$`What is your Hogwarts House?` == "Hufflepuff",
                   "https://vignette.wikia.nocookie.net/pottermore/images/5/5e/Hufflepuff_crest.png/revision/latest/scale-to-width-down/180?cb=20111112232427",
                   ifelse(df_map$`What is your Hogwarts House?`== "Gryffindor", "http://leafletjs.com/examples/custom-icons/leaf-red.png",
                          ifelse(df_map$`What is your Hogwarts House?`== "Slytherin", "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                                 "https://www.seekpng.com/png/small/152-1521662_harry-potter-ravenclaw-crest-mens-crewneck-sweatshirt-harry.png"))
                   
  ),
  iconWidth = 17, iconHeight = 30,
  iconAnchorX = 17, iconAnchorY = 30
)


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
                      plotOutput(outputId = "specialization"),
                      h2("Hogwarts House Responses"),
                      plotOutput(outputId = "house"),
                      h2("Enneagram Types"),
                      plotOutput(outputId = "enneagram"),
                      h2("Astrology Signs"),
                      plotOutput(outputId = "astrology")
                      
             ),
             
             tabPanel("Enneagram Types",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(
                            inputId = "chase",
                            label = h3("Select Category"),
                            choices = c("Age", "Astrological Sign", "Dog vs Cat", "Introvert vs Extrovert", "Favorite Color", "Hogwarts House","Home State", "Myers-Briggs", "Program", "Specialization")
                          ),
                          uiOutput("secondSelection")
                        ),
                        mainPanel(
                          plotOutput(outputId = "bar")
                        ))),
             
             
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
                          plotlyOutput(outputId = "scatter"))
                      )),
             
             
             tabPanel("Spatial Data",
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioButtons("scattercolor", 
                                       "Where are my Hogwarts Housemates?",
                                       choices = c("Gryffindor", "Hufflepuff", "Slytherin", "Ravenclaw"))
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          
                          leafletOutput("mymap", height = 1000)
                          
                        )
                      ))
             
  )
  
)









# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # graphs for intro page?
  
  output$specialization <- renderPlot({
    clean_df %>% 
      ggplot(aes(x = Specialization, fill = Specialization)) +
      geom_bar(color = "grey", width = .8, size = 1) +
      xlab("Bren Specialization") +
      ylab("Number of Respondants") +
      theme_minimal() +
      theme(legend.position = "none") +
      geom_text(stat='count', aes(label=..count..), vjust=-1) +
      scale_fill_brewer(palette = "Set1")
    
  })
  
  output$house <- renderPlot({
    clean_df %>% 
      ggplot(aes(x = `Hogwarts House`, fill = `Hogwarts House`)) +
      geom_bar(color = "grey", width = .8, size = 1) +
      scale_fill_manual(values = c("red4", "yellow2", "midnightblue", "darkgreen")) +
      xlab("Hogwarts House") +
      ylab("Number of Respondants") +
      theme_minimal() +
      theme(legend.position = "none") +
      geom_text(stat='count', aes(label=..count..), vjust=-1)
    
  })
  
  output$enneagram <- renderPlot({
    clean_df %>% 
      ggplot(aes(x = `Enneagram Type`, fill = `Enneagram Type`)) +
      geom_bar(color = "grey", width = .8, size = 1) +
      scale_fill_brewer(palette = "Set3") +
      xlab("Enneagram Type") +
      ylab("Number of Respondants") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous(breaks = seq(0,15, by = 5)) +
      geom_text(stat='count', aes(label=..count..), vjust=-1)
    
  })
  
  output$astrology <- renderPlot({
    clean_df %>% 
      ggplot(aes(x = `Astrological Sign`, fill = `Astrological Sign`)) +
      geom_bar(color = "grey", width = .8, size = 1) +
      xlab("Astrological Sign") +
      ylab("Number of Respondants") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous(breaks = seq(0,10, by = 2)) +
      geom_text(stat='count', aes(label=..count..), vjust=-1) +
      scale_fill_brewer(palette = "Paired")
    
  })
  
  output$secondSelection <- renderUI({
    selectInput(
      inputId = "group",
      "Select",
      "Select Group",
      choices = unique(chase_data[,input$chase])
    )
  })
  
  
  output$bar <- renderPlot({
    
    chase <- input$chase
    group <- input$group
    
    bar_data <- chase_data %>% 
      dplyr::filter(`Enneagram Type` != "NA") %>%
      dplyr::filter(`Program` != "NA") %>% 
      dplyr::filter(`Age` != "NA") %>% 
      dplyr::filter(`Myers-Briggs` != "NA") %>% 
      dplyr::filter(`Home State` != "NA") %>% 
      dplyr::filter(`Favorite Color` != "NA")
    
    ggplot(bar_data[bar_data[[input$chase]] %in% (input$group),], aes(x = `Enneagram Type`, fill = `Enneagram Type`)) +
      geom_bar(color = "grey") +
      geom_text(stat = "count", 
                aes(label =..count..), 
                color = "white",
                nudge_y = -0.15,
                size = 2.5,
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
            axis.text.x = element_text(color = "black", size = 10)) +
      ggtitle(group) +
      theme(plot.title = element_text(size = 30, face = "bold")) +
      labs(fill = "Enneagram Type") +
      scale_fill_brewer(palette = "Paired")
    
  })
  
  
  #panel2 - patronuses
  output$scatter <- renderPlotly({
    column <- sym(input$x)
    col <- sym(input$variable)
    
    
    
    patronus <- ggplot(data = final_df, aes(x = !!column, y = `Patronus Danger Rating`, label = Patronus)) +
      geom_point(aes(color = !!col, text = Name), size = 4, alpha = .8) +
      theme_light() +
      scale_color_manual(values = c("#EAB364","#A4CABC", "#B2473E", "#ACBD78")) +
      labs(title = "Patronus Danger Ratings: \n How do you compare?") +
      theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5), plot.title = element_text(hjust = 0.5, face = "bold", size = 15), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"),legend.title=element_text(size=8), legend.justification = "center", legend.text = element_text(size = 8),  legend.position=c(0.85, -0.75))
    ggplotly(patronus, tooltip = c("Name", "color", "Patronus"), width = 600, height = 500)
  })
  
  #Panel 3: Map
  
  data <- reactive({
    
    x <- map_data
    
  })
  
  output$mymap <- renderLeaflet({
    
    map_data <- data()
    
    m <- leaflet() %>% 
      addTiles() %>% 
      addMarkers(lng = df_map$Longitude, lat = df_map$Latitude, icon = hog_Icons)
    
    m
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



