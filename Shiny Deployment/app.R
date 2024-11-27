library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(shiny)
library(bslib)
library(dplyr)
library(shinyjs)

#reading in manipulated data! 

all_information_for_shiny_app <- read.csv("Data/All_2024_Info.csv")

#also reading in ages for the header
ages <- read.csv("Data/2024_Age.csv") %>% 
  dplyr::select(player_id, player_age) %>% 
  rename(batter = player_id)

all_information_for_shiny_app <- all_information_for_shiny_app %>% 
  inner_join(ages)

#geting the strengths and plots functions
source("Scripts/Strengths.R")
source("Scripts/Plots.R")

#as described in the manipulation code, some choices are too small
#sample for certain situations

by_hand_choices <- c("Swing%", "Zone Swing%", 
                     "Chase%", "Whiff%", 
                     "GB%", "Hard Hit%")

by_hand_initial_selections <- c("Swing%",
                                "Chase%", "Whiff%", 
                                "GB%")

by_count_choices <- c("Swing%", "Zone Swing%", 
                      "Chase%", "Whiff%", 
                      "GB%", "Hard Hit%",
                      "Bat Speed", "Swing Length")

by_count_initial_selections <- c("Swing%", 
                                 "Chase%", "Whiff%", 
                                 "GB%", "Hard Hit%")

by_pitch_type_choices <- c("Swing%", "Zone Swing%", 
                           "Chase%", "Whiff%", 
                           "GB%", "Hard Hit%")

by_pitch_type_initial_selections <- c("Chase%", "Whiff%", 
                                      "GB%", "Hard Hit%")

#for the selecter
players <- all_information_for_shiny_app %>% 
  pull(PlayerName)

ui <- page_sidebar(
  useShinyjs(),  
  title = "Analysis of 2024 Hitters",
  sidebar = sidebar(
    navset_tab(
      nav_panel(
        "Batter",
        selectInput("batter", 
                    "Select Batter:",
                    choices = players),
        actionButton("generate", "Generate Analysis", class = "btn-primary")
      ),
      nav_panel(
        "Filters",
        #was really busy when I had too many variables but I didn't want to drop
        #unused variables, this felt like a good solution
        checkboxGroupInput(
          "pitch_type_filter",
          "Pitch Type Filter:",
          choices = by_pitch_type_choices,
          selected = by_pitch_type_initial_selections
        ),
        checkboxGroupInput(
          "count_filter",
          "Count Filter:",
          choices = by_count_choices,
          selected = by_count_initial_selections
        ),
        checkboxGroupInput(
          "handedness_filter",
          "Pitcher Hand Filter:",
          choices = by_hand_choices,
          selected = by_hand_initial_selections
        )
        
        )
    )
  ),
  

  div(
    style = "min-height: 800px; overflow-y: auto;",
    card(
      height = "120px",
      class = "mb-3",
      card_body(
        div(
          style = "display: flex; align-items: center; gap: 20px;",
          #image that i'm pulling from statcast later
          img(
            id = "player_image",
            src = "",
            height = "80px",
            style = "object-fit: contain;"
          ),
          div(
            style = "display: flex; align-items: center; gap: 20px;",
            h2(
              style = "margin: 0; padding: 0;",
              textOutput("player_name")
            ),
            h3(
              style = "margin: 0; padding: 0;",
              textOutput("player_position")
            ),
            h3(
              style = "margin: 0; padding: 0;",
              textOutput("player_hand")
            ),
            h3(
              style = "margin: 0; padding: 0;",
              textOutput("player_age")
            )
            ))
      )
    ),
    
  layout_columns(
    fill = FALSE,
    navset_card_tab(
      nav_panel("Overall Batting",
                card_header("20-80 scale. More favorable outcomes are closer to 80"),
                girafeOutput("overall_plot"),
      ),
      nav_panel("Defense and Speed",
                card_header("20-80 scale. More favorable outcomes are closer to 80"),
                girafeOutput("def_plot"),
      )
    ),
    navset_card_tab(
      
      nav_panel("By Pitch Type",
                card_header("20-80 scale, FB is Fastball, BB is Breaking Ball, OS is Offspeed"),
                girafeOutput("pitch_type_plot")
      ),
      nav_panel("By Count",
                card_header("20-80 scale"),
                girafeOutput("count_plot")
      ),
      nav_panel("By Pitcher Hand", 
                card_header("20-80, comparing to hitters of the same handedness vs that pitcher hand"),
                girafeOutput("pitcher_hand_plot")
      )
    )
  ),
  
  card(
    card_header("Batter Analysis"),
    layout_columns(
      card(
        card_header("Strengths"),
        textOutput("strengths_text")
      ),
      card(
        card_header("Weaknesses"),
        textOutput("weaknesses_text")
      )
    )
  )
  )
)

server <- function(input, output) {
  #reactive values to store results
  analysis_results <- reactiveVal(list(
    strengths = "",
    weaknesses = ""
  ))
  
  #and let's generate those strengths and weaknesses
  observeEvent(input$generate, {
    analysis_results(list(
      strengths = strengths_paragraph(Name_Input = input$batter, data_frame = all_information_for_shiny_app),
      weaknesses = weaknesses_paragraph(Name_Input = input$batter, data_frame = all_information_for_shiny_app)
    ))
    
    this_player_id <- all_information_for_shiny_app %>% 
      filter(PlayerName == input$batter) %>% 
      pull(batter)
    
    position <- all_information_for_shiny_app %>% 
      filter(PlayerName == input$batter) %>% 
      pull(Pos)
    
    age <- all_information_for_shiny_app %>% 
      filter(PlayerName == input$batter) %>% 
      pull(player_age)
    
    handedness <- all_information_for_shiny_app %>% 
      filter(PlayerName == input$batter) %>% 
      pull(batter_handedness)
    
    batter_data <- all_information_for_shiny_app %>% 
      filter(PlayerName == input$batter)
    
    #base URL for MLB player images
    image_url <- sprintf(
      "https://img.mlbstatic.com/mlb-photos/image/upload/w_600,d_people:generic:action:hero:current.png,q_auto:best,f_auto/v1/people/%s/action/hero/current",
      this_player_id
    )
    #pulling it here!
    runjs(sprintf("document.getElementById('player_image').src = '%s'", image_url))
    
    this_player_name <- input$batter
    
    output$player_name <- renderText({
      this_player_name
      })
    
    output$player_position <- renderText({
      paste0("Primary Position: ", position)
    })
    
    output$player_hand <- renderText({
      paste0("Bats: ", handedness)
    })
    
    
    output$player_age <- renderText({
      paste0("Age in 2024: ", age)
    })
    
    
    output$overall_plot <- renderGirafe({
      
      percentiles_plot_overall(batter_data, input$batter)
    })
    
    output$def_plot <- renderGirafe({
      
      percentiles_plot_defense(batter_data, input$batter)
    })
    
    output$pitcher_hand_plot <- renderGirafe({
      
      percentiles_plot_handedness(batter_data, input$batter, input$handedness_filter)
    })
    
    output$count_plot <- renderGirafe({
      
      percentiles_plot_count(batter_data, input$batter, input$count_filter)
    })
    
    output$pitch_type_plot <- renderGirafe({
      
      percentiles_plot_pitch_type(batter_data, input$batter, input$pitch_type_filter)
    })
    
    
  })
  
  
  
  output$strengths_text <- renderText({
    analysis_results()$strengths
  })
  
  output$weaknesses_text <- renderText({
    analysis_results()$weaknesses
  })
}

shinyApp(ui = ui, server = server)
