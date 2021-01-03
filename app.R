library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggthemes)
library(shinythemes)
library(plotly)


rt <- read.csv("Rt_cases_2020_12_29.csv", sep = ";")


ui <- dashboardPage(title = "COVID-19 Dashboard", skin = "blue",
                    dashboardHeader(title = "COVID-19 Dashboard"),
                    
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Oversigt", tabName = "dashboard", icon = icon("dashboard"))
                        )
                    ),

        dashboardBody(
           fluidRow(
               valueBoxOutput(width = 2, "rt"), box(title = "Tendens", width = 10, plotOutput("rt_tendens"))),
           fluidRow(box(title = "Udvikling i kontakttal", width = 12, plotlyOutput("rt_plotly", height = 200)))#,
                    #fluidRow(box(title = "Vælg datoer", width = 2, status = "primary", solidHeader = TRUE, dateRangeInput("date_slider", label = "", start = Sys.Date() - 7, end = Sys.Date())))
        )
    )



server <- function(input, output) {

    output$rt <- renderValueBox({
        
        last_row <- tail(rt, n = 1)
        
        valueBox(value = last_row$estimate, "Kontakttal", subtitle = paste0("Kontakttal ", last_row$date_sample), icon = icon("people-arrows"), color = "red")
    
    })
    
    output$rt_tendens <- renderPlot({
        
        theme_set(theme_minimal())
        
        ggplot(rt, aes(as.Date(date_sample), estimate, group = 1)) +
            geom_smooth() +
            geom_hline(yintercept = 4) +
            scale_x_date(date_breaks = "1 month") +
            labs(x = "",
                 y = "Kontakttal")
            
        
      })
    
    
    output$rt_plotly <- renderPlotly({
        
        theme_set(theme_minimal())
        
        rt_plot <- ggplot(rt, aes(as.Date(date_sample), estimate, group = 1, text = paste("Kontakttal: ", estimate, "<br>Dato: ", as.Date(date_sample)))) +
            geom_line() +
            scale_x_date(date_breaks = "1 month") +
            labs(x = "",
                 y = "Kontakttal")
        
        ggplotly(rt_plot, tooltip = c("text"))
        
    })
    
    
        
}

# Run the application 
shinyApp(ui = ui, server = server)
