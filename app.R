
 library(shiny)
 library(tidyverse)
 library(here)
 library(janitor)
 library(dplyr)
 library(ggplot2)
 library(tidyr)
 library(lubridate)
 library(magrittr)
 library(shinyWidgets)


 df <- read_csv(here("strong.csv")) %>% # This must be the directory of the server. Raw/ is my local
   clean_names() %>% 
   select(-8:-last_col()) %>% 
   mutate(datetime = ymd_hms(date)) %>% 
   mutate(hour = lubridate::hour(date)) %>% 
   mutate(weekday = lubridate::wday(date, label = TRUE)) %>% 
   mutate(month = month(date, label = TRUE, abbr = FALSE)) %>% 
   mutate(date = as.Date(date)) %>% 
   mutate(set_order = as.factor(set_order))
 
 df0.5 <- df %>% 
   select(-set_order) %>% 
   group_by(datetime, exercise_name) %>% 
   slice_max(weight, with_ties = FALSE)
 
 df2 <- df %>% 
   distinct(datetime, .keep_all = TRUE) %>% 
   group_by(hour, weekday) %>% 
   summarise(totalexercises = n())
 
 df2.5 <- df %>% 
   distinct(datetime, .keep_all = TRUE) %>% 
   group_by(weekday) %>% 
   summarise(totalexercises = n()) %>% 
   mutate(y = as_factor(1))
 
 df3 <- df %>% 
   mutate(weightxreps = weight * reps) %>% 
   mutate(workout_name = replace(workout_name, exercise_name == "Plank", "Core"),
          workout_name = replace(workout_name, exercise_name == "Russian Twist", "Core"),
          workout_name = replace(workout_name, exercise_name == "Decline Crunch", "Core"),
          workout_name = replace(workout_name, exercise_name == "Ab Wheel", "Core"),
          workout_name = replace(workout_name, exercise_name == "Hanging Leg Raise", "Core")) %>% 
   filter(workout_name != "Core") %>% 
   group_by(datetime, workout_name) %>% 
   summarise(total_weight = sum(weightxreps))
 
 df4 <- df %>% 
   select(-8:-last_col()) %>% 
   rename("Exercise" = "exercise_name",
          "Set" = "set_order",
          "Weight (lbs)" = "weight",
          "Reps" = "reps")
 
 dfsummary <- df4 %>% 
   select(1:3) %>% 
   distinct() %>% 
   rename("Workout" = "workout_name",
          "Duration" = "duration")
   
 
 dates <- df %>% 
   distinct(date) %>% 
   pull() # Convert to vector for airdatepicker
 
min_date <- min(df$date)
max_date <- max(df$date)


 
 linebreaks <- function(n){HTML(strrep(br(), n))}
 
dropdowncss <- "
.selectize-dropdown-content {
  max-height: 500px;
  overflow-y: auto;
  background: ghostwhite;
}
"
 transparent <- "
 .set1 form.well { 
   background: transparent;
   border: 0px;
 }
 "
 ui <- navbarPage("Cole's Gym Tracker",
                  tabPanel("Weight",
                          sidebarLayout(
                            sidebarPanel(
                              tags$style(dropdowncss),
                              selectInput("exercise_name", "Exercise:", 
                                          unique(as.character(df$exercise_name)), 
                                          selected = "Hack Squat", width="350px"),
                              h4("Notes"),
                              p("Cases where prolonged plateaus are obserbed (e.g., Hip Adductor and Hip Abductor) are due to maxing out
                    the weight of the machine. Additionally, there are some cases where exercise weight is measured per limb rather
                    than totally (e.g., lying leg curl is per leg).")
                            ),
                            mainPanel(
                              plotOutput("barPlot", width = "100%"),
                              plotOutput("barPlot2", width = "100%", height = 200),
                              plotOutput("maxweight", width = "100%"))
                            )
                          ),
     tabPanel("Total Weight",
              sidebarLayout(
                sidebarPanel(
                  checkboxGroupInput("workout_name", "Workout:", 
                                     c("Legs" = "Legs",
                                       "Push" = "Push",
                                       "Pull" = "Pull")),
                  
                ),
                mainPanel(
                  plotOutput("total_weight", width = "100%"),
              )
              )
     ),             
 
     tabPanel("Weekly Heatmap",
              sidebarLayout(
                sidebarPanel(
                  selectInput("cal_choices", "Select Time Distribution", 
                                     c("Hourly",
                                       "Weekly"),
                              selected = "Hourly")
                ),
                mainPanel(
          plotOutput("heatMap", width = 500, height = 600)
                )
      )
      ),
     tabPanel("Data",
              sidebarLayout(
                sidebarPanel(
                  airDatepickerInput("date", label = h3("Select Date"), highlightedDates = dates, 
                                     minDate = min_date, maxDate = max_date, multiple = TRUE),
                  p("Select one or multiple dates to view workout details."),
                  p(div(HTML("<em>Dates with data available are marked on the calendar with a dot.</em>"))),
                  br(),
                  h4("Workout Summary"),
                  p(div(HTML("<em>For selected date(s).</em>"))),
                  tableOutput("datatablesummary")

                ),
                mainPanel(
                  h1("Workout Details"),
                  tableOutput("datatable")
                )
              )
            
             
              ),
     tabPanel("About",
              h1("Data"),
              p("Data was logged and retrieved using the Strong App and workouts were done at Snap Fitness Kildonan."),
              p("R and the tidyverse was used to manipulate data and the app is made and hosted with Shiny.")
              )
 ) # end
   

 server <- function(input, output) {
   output$barPlot <- renderPlot({
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
     
   })
   
   output$maxweight <- renderPlot({
     ggplot(df0.5[df0.5$exercise_name == input$exercise_name,],
            aes(x = datetime, y = weight)) +
       geom_point() +
       geom_smooth(se = FALSE) +
       theme_bw() +
       labs(x = "Date",
            y = "Weight (lbs)",
            title = "Maximum Weight Lifted") +
       theme(plot.title = element_text(face = "bold"))
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
         theme(plot.title = element_text(face = "bold")) +
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
         theme(plot.title = element_text(face = "bold")) +
         scale_fill_viridis_d(name = "Set")
     }
   })
   
   output$total_weight <- renderPlot({
          ggplot(df3[df3$workout_name %in% input$workout_name, ],
            aes(x = datetime, y = total_weight)) +
                geom_point(aes(group = workout_name, colour = workout_name)) +
                geom_smooth(aes(group = workout_name, colour = workout_name), se = FALSE) +
                theme_bw() +
                labs(x = "Date",
                     y = "Weight (lbs)",
                     title = "Total Weight Lifted in Workout Sessions",
                     colour = "Workout") +
       theme(plot.title = element_text(face = "bold"))
   })
   
   output$heatMap <- renderPlot({
     if (input$cal_choices == "Hourly") {
       ggplot(df2, aes(weekday, hour, fill = totalexercises)) + 
         geom_tile(color = "white", size = 0.1) +
         scale_fill_distiller(palette = "YlOrBr", trans = "reverse") +
         scale_y_continuous(breaks = seq(0,24, by = 1),
                            trans = "reverse",
                            expand = c(0, 0)) + 
         labs(x= "Weekday", y= "Hour of the day (24-Hour, CST)",
              fill = "Exercise \nSessions",
              title = "Weekly Exercise Heatmap") +
         theme_bw() +
         theme(panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank(),
               panel.grid.minor.y = element_blank())
     } else if (input$cal_choices == "Weekly") {
       ggplot(df2.5, aes(x = weekday, y = y, fill = totalexercises)) + 
         geom_tile(color = "white", size = 0.1) +
         scale_fill_distiller(palette = "YlOrBr", trans = "reverse") + 
         scale_y_discrete(expand = c(0, 0)) +
         labs(x = "Weekday",
              y = "",
              fill = "Exercise \nSessions",
              title = "Weekly Exercise Heatmap") +
         theme_bw() +
         theme(panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank())
     }
   })
   
   output$datatable <- renderTable(
     table <- df4 %>% 
       filter(date %in% input$date) %>% 
       select(-duration, -workout_name) %>% 
       select(-date),
     digits = 0
   )
   
   output$datatablesummary <- renderTable(
     tablesummary <- dfsummary %>% 
       filter(date %in% input$date) %>% 
       select(-date),
     digits = 0
   )
 }
 
 # Run the application 
 shinyApp(ui = ui, server = server)
