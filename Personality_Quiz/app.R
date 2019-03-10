

library(shiny)
library(tidyverse)
library(shinythemes)
library(readr)
library(ggplot2)
library(beyonce)
library(plotly)
library(leaflet)
library(viridis)
library(janitor)
library(sf)

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
colnames(final_df) <- c("Time", "Name", "Program", "Specialization", "Age", "Astrological Sign", "Home State", "Favorite Color", "Dog vs Cat", "Hogwarts House", "Patronus", "Introvert vs Extrovert", "Myers-Briggs", "Enneagram Type", "Enneagram Wing", "Favorite_R_Color", "Latitude", "Longitude", "Patronus Danger Rating")
# clean data
clean_df <- read_csv("cleandata.csv")

clean_df$Danger_rating <- Danger_ratings

colnames(clean_df) <- c("Time", "Name", "Program", "Specialization", "Age", "Astrological Sign", "Home State", "Favorite Color", "Dog vs Cat", "Hogwarts House", "Patronus", "Introvert vs Extrovert", "Myers-Briggs", "Enneagram Type", "Enneagram Wing", "Favorite_R_Color", "Latitude", "Longitude", "Patronus Danger Rating")

chase_data <- clean_df %>% 
  filter(`Enneagram Type` != "NA")

df_map <- clean_df%>% 
  filter(`Latitude` != "NA")

df_sf <- st_as_sf(df_map, coords = c("Longitude", "Latitude"), crs = 4326)

clean_df$`Enneagram Type` <- as.character(clean_df$`Enneagram Type`)
chase_data$`Enneagram Type` <- as.character(chase_data$`Enneagram Type`)

hog_Icons <- icons(
  iconUrl = ifelse(df_map$`Hogwarts House` == "Hufflepuff",
                   "https://vignette.wikia.nocookie.net/pottermore/images/5/5e/Hufflepuff_crest.png/revision/latest/scale-to-width-down/180?cb=20111112232427",
                   ifelse(df_map$`Hogwarts House`== "Gryffindor", "http://leafletjs.com/examples/custom-icons/leaf-red.png",
                          ifelse(df_map$`Hogwarts House`== "Slytherin", "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                                 "https://www.seekpng.com/png/small/152-1521662_harry-potter-ravenclaw-crest-mens-crewneck-sweatshirt-harry.png"))
                   
  ),
  iconWidth = 17, iconHeight = 30,
  iconAnchorX = 17, iconAnchorY = 30
)


ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Bren Student Personality Quiz Data"),
  
  navbarPage("Navigation:",
             
             tabPanel("Intoduction",
                      h1("Welcome!"),
                      h2("How it all began..."),
                      p("The night before the Earth System Science final exam, a group of weary MESMs grew tired of calculating adiabatic lift rates, and instead decided to take as many fun online personality quizzes as possible. While comparing the results, the group realized that these quizes had the potential to make a really awesome dataset, they just needed more respondants. In a quest for an n>30, they started spamming the 2020 cohort on multiple platforms, and responses began to flow in."),
                      p("We hoped to answer questions such as, is everyone from New England a Slytherin? Can we predict how dangerous your patronus is? Do CMRMs share an enneagram type? Do astrological signs influence literally anything? Turns out we can't really answer any of these questions, but we hope you have as much fun exploring the results as we did!"),
                      h1("Here's a quick snapshot of some of our data:"),
                      h2("Hogwarts House Responses"),
                      plotOutput(outputId = "house"),
                      h2("Specialization Responses"),
                      plotOutput(outputId = "specialization"),
                      h2("Enneagram Types"),
                      plotOutput(outputId = "enneagram"),
                      h2("Astrology Signs"),
                      plotOutput(outputId = "astrology")
                      
             ),
             
             tabPanel("Expecto Enneagram!",
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
             
             
             tabPanel("Patronus Danger Ratings",
                      
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
                          h2("Hover your mouse over the different points to see details about your classmates!"),
                          p("Note: Patronus dangers ratings were determined through a scientifically rigorous process that may or may not have occured during a watch party for The Bachelor and after a bottle of wine."),
                          # Show a plot of the generated distribution
                          plotlyOutput(outputId = "scatter"))
                      )),
             
             
             tabPanel("Where in the World are the Hufflepufffs?",
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          
                          checkboxGroupInput("hogwarts", 
                                             "Where are my Hogwarts Housemates?",
                                             choices = list("Gryffindor" = "Gryffindor", 
                                                            "Hufflepuff" = "Hufflepuff", "Slytherin" = "Slytherin", "Ravenclaw" = "Ravenclaw")),
                          checkboxGroupInput("dog_cat", 
                                             "Where are the dog and cat people?",
                                             choices = list("Dog" = "Dog", 
                                                            "Cat" = "Cat")),
                          checkboxGroupInput("spec", 
                                             "Where are my specialization peers?",
                                             choices = list("CMRM" = "CMRM", 
                                                            "EC" = "EC",
                                                            "CEM" = "CEM",
                                                            "PPR" = "PPR",
                                                            "CP" = "CP",
                                                            "WRM" = "WRM",
                                                            "EPE" = "EPE",
                                                            "Data Science" = "Data Science",
                                                            "Community Ecology" = "Community Ecology"))
                          
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                        
                          leafletOutput("mymap", height = 500)
                          
                        )
                      ))
             
  )
  
)









# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # graphs for intro page?
  
  
  output$house <- renderPlot({
    clean_df %>% 
      ggplot(aes(x = `Hogwarts House`, fill = `Hogwarts House`, na.rm = TRUE)) +
      geom_bar(color = "grey", width = .8, size = 1, na.rm = TRUE) +
      scale_fill_manual(values = c("#B2473E","#EAB364","midnightblue","darkgreen")) +
      xlab("Hogwarts House") +
      ylab("Number of Respondants") +
      theme_minimal() +
      theme(legend.position = "none") +
      geom_text(stat='count', aes(label=..count..), vjust=-1)
    
  })
  
  
  output$specialization <- renderPlot({
    clean_df %>% 
      ggplot(aes(x = Specialization, fill = Specialization, na.rm = TRUE)) +
      geom_bar(color = "grey", width = .8, size = 1, na.rm = TRUE) +
      xlab("Bren Specialization") +
      ylab("Number of Respondants") +
      theme_minimal() +
      theme(legend.position = "none") +
      geom_text(stat='count', aes(label=..count..), vjust=-1) +
      scale_fill_viridis(discrete = TRUE)
    
  })
  
  
   output$enneagram <- renderPlot({
     clean_df %>% 
       ggplot(aes(x = `Enneagram Type`, fill = `Enneagram Type`)) +
       geom_bar(color = "grey", width = .8, size = 1) +
       scale_fill_viridis(discrete = TRUE) +
       xlab("Enneagram Type") +
       ylab("Number of Respondants") +
       theme_minimal() +
       theme(legend.position = "none") +
       scale_y_continuous(breaks = seq(0,15, by = 5)) +
       geom_text(stat='count', aes(label=..count..), vjust=-1)
     
   })
   
   output$astrology <- renderPlot({
     subset(clean_df, !is.na(`Astrological Sign`)) %>% 
       ggplot(aes(x = `Astrological Sign`, fill = `Astrological Sign`)) +
       geom_bar(color = "grey", width = .8, size = 1) +
       xlab("Astrological Sign") +
       ylab("Number of Respondants") +
       theme_minimal() +
       theme(legend.position = "none") +
       scale_y_continuous(breaks = seq(0,10, by = 2)) +
       geom_text(stat='count', aes(label=..count..), vjust=-1) +
       scale_fill_viridis(discrete = TRUE)
     
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
        theme(plot.title = element_text(size = 25, face = "bold")) +
        labs(fill = "Enneagram Type") +
        scale_fill_viridis(discrete = TRUE)
    
  })
  
  
  #panel2 - patronuses
  output$scatter <- renderPlotly({
    column <- sym(input$x)
    col <- sym(input$variable)
    
    
    
    patronus <- ggplot(data = final_df, aes(x = !!column, y = `Patronus Danger Rating`, label = Patronus)) +
      geom_point(aes(color = !!col, text = Name), size = 4, alpha = .8) +
      theme_light() +
      scale_color_viridis(discrete = TRUE) +
      labs(title = "Patronus Danger Ratings: \n How do you compare?", y = "Patronus Danger Rating") +
      theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5), plot.title = element_text(face = "bold", size = 15), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"),legend.title=element_text(size=8), legend.justification = "center", legend.text = element_text(size = 8),  legend.position=c(0.85, -0.75))
    ggplotly(patronus, tooltip = c("Name", "color", "Patronus"), width = 700, height = 500)
  })
  
  #Panel 3: Map
  
  output$mymap <- renderLeaflet({
    
    hog_data <- df_sf %>% 
      clean_names() %>% 
      filter(hogwarts_house %in% input$hogwarts)
    
    dc_data <- df_sf %>% 
      clean_names() %>% 
      filter(dog_vs_cat %in% input$dog_cat)
    
    spec_data <- df_sf %>% 
      clean_names() %>% 
      filter(specialization %in% input$spec)
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      addMarkers(data = dc_data) %>% 
      addMarkers(data = hog_data) %>%
      addMarkers(data = spec_data) %>%
      setView(lng=-95.822841, lat= 38.515979, zoom = 4)
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
