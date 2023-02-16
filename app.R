library(shiny)
 library(tidyverse)
 library(here)
 library(janitor)
 library(magrittr)
 library(dplyr)
 library(ggplot2)
 library(tidyr)
 library(lubridate)

 df <- read_csv(here("strong.csv")) %>% # This must be the directory of the server. Raw/ is my local
   clean_names() %>% 
   select(-8:-last_col()) %>% 
   mutate(datetime = ymd_hms(date)) %>% 
   mutate(hour = lubridate::hour(date)) %>% 
   mutate(weekday = lubridate::wday(date, label = TRUE)) %>% 
   mutate(month = month(date, label = TRUE, abbr = FALSE)) %>% 
   mutate(date = as.Date(date)) %>% 
   mutate(set_order = as.factor(set_order))
 
 df2 <- df %>% 
   distinct(datetime, .keep_all = TRUE) %>% 
   group_by(hour, weekday) %>% 
   summarise(totalexercises = n())
 
 linebreaks <- function(n){HTML(strrep(br(), n))}
 
 ui <- fluidPage(
   titlePanel("Cole's Gym Progress"),
   sidebarLayout(
     sidebarPanel(
       selectInput("exercise_name", "Exercise:", 
                   unique(as.character(df$exercise_name)), 
                   selected = "Hack Squat"),
       p("This app displays my exercise weight and frequency progression over time. Also shown is a exercise calendar showing what
      times of the day and on what days of the week I exercise."), 
       linebreaks(2),
       p("This app was built using R and the tidyverse to compute stats, ggplot2 for data vizualizations and 
      hosted with Shiny. Data was logged and stored using the Strong App. All exercise was done at Snap Fitness Kildonan."),
       width = 3),
     
     mainPanel(
       tabsetPanel(
       tabPanel(
         "Exercises", 
         plotOutput("barPlot", width = "100%"),
         plotOutput("barPlot2", width = "100%", height = 200)),
       tabPanel("Exercise Heatmap",
       plotOutput("heatMap", width = 500, height = 600)
       )
       )
     )
   )
 )
 
 server <- function(input, output) {
   output$barPlot <- renderPlot({
     if (input$exercise_name == "All") {
       ggplot(df, aes(x = date, y = weight, fill = fct_rev(set_order))) +
         geom_bar(stat = "identity") +
         scale_y_continuous() +
         theme_bw() +
         labs(x = "Date",
              y = "Weight (lbs)",
              title = "Weight Progression",
              colour = "Rep Number") +
         theme(plot.caption = element_text(hjust = 0)) +
         scale_fill_viridis_d(name = "Rep Number")
     } else {
       ggplot(df[df$exercise_name == input$exercise_name,],
              aes(x = date, y = weight, fill = set_order)) +
         geom_bar(stat = "identity", position = "dodge", width = 2) +
         scale_y_continuous() +
         scale_x_date(expand = c(0, 0)) +
         theme_bw() +
         labs(x = "Date",
              y = "Weight (lbs)",
              title = "Weight Progression",
              colour = "Rep Number") +
         theme(plot.caption = element_text(hjust = 0, face = "bold")) +
         scale_fill_viridis_d(name = "Set")
     }
   })
   
   output$barPlot2 <- renderPlot({
     if (input$exercise_name == "All") {
       ggplot(df, aes(x = date, y = reps, fill = fct_rev(set_order))) +
         geom_bar(stat = "identity") +
         scale_y_continuous(breaks = seq(0,20, by = 5)) +
         theme_bw() +
         labs(x = "Date",
              y = "Weight (lbs)",
              title = "Weight Progression",
              colour = "Rep Number") +
         theme(plot.caption = element_text(hjust = 0)) +
         scale_fill_viridis_d(name = "Rep Number")
     } else {
       ggplot(df[df$exercise_name == input$exercise_name,],
              aes(x = date, y = reps, fill = set_order)) +
         geom_bar(stat = "identity", position = "dodge", width = 2) +
         scale_y_continuous(breaks = seq(0,20, by = 5)) +
         scale_x_date(expand = c(0, 0)) +
         theme_bw() +
         labs(x = "Date",
              y = "Reps",
              title = "Number of Reps",
              colour = "Rep Number") +
         theme(plot.caption = element_text(hjust = 0, face = "bold")) +
         scale_fill_viridis_d(name = "Set")
     }
   })
   
   output$heatMap <- renderPlot({
       ggplot(df2, aes(weekday, hour, fill = totalexercises)) + 
         geom_tile(color = "white", size = 0.1) +
         scale_fill_gradient(high = "#D88300", low = "#F9D398", breaks = seq(0,10, by = 1)) +
         scale_y_continuous(trans = "reverse") + 
         labs(x= "Weekday", y= "Hour of the day (24-Hour, CST)",
              fill = "Exercise \nSessions",
              title = "Weekly Exercise Heatmap") +
         theme_bw() +
         theme(panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank())
    
   })
 }
 
 # Run the application 
 shinyApp(ui = ui, server = server)
