library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggthemes)
library(shinythemes)
library(plotly)
library(extrafont)
library(grid)
library(gridExtra)
library(readr)
library(mapDK)
library(mapproj)

rt <- read.csv("Rt_cases_2020_12_29.csv", sep = ";")

pos <- read.csv("Test_pos_over_time.csv", sep = ";")

ind <- read.csv("Newly_admitted_over_time.csv", sep = ";")

#reg <- read.csv("Region_summary.csv", sep = ";")

reg <- read_delim("Region_summary.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                           grouping_mark = "."), trim_ws = TRUE)

dead <- read.csv("Deaths_over_time.csv", sep = ";")

mun <- read_delim("Municipality_test_pos.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                           grouping_mark = "."), trim_ws = TRUE)



ui <- dashboardPage(title = "COVID-19 Dashboard", skin = "blue",
                    dashboardHeader(title = "COVID-19 Dashboard"),
                    
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Oversigt", tabName = "dashboard", icon = icon("dashboard")),
                                     menuItem(tagList(shiny::icon("linkedin"),"LinkedIn"), href = "https://www.linkedin.com/in/simon-michael-jensen/"),
                            menuItem(tagList(shiny::icon("twitter"),"Twitter"), href = "https://twitter.com/data_scimon")
                    )),

        dashboardBody(
           fluidRow(
               valueBoxOutput(width = 3, "rt"), 
               valueBoxOutput(width = 3, "pos"),
               valueBoxOutput(width = 3, "ind"),
               valueBoxOutput(width = 3, "dead")), 
               fluidRow(tabBox(title = tagList(shiny::icon("chart-line"), "Tendens"), id = "tabset1", height = "200px", width = 12, 
                               tabPanel(tagList(shiny::icon("people-arrows"),"Kontakttal"), "", box(title = "", width = 12, plotOutput("rt_tendens", height = 400))),
                               tabPanel(tagList(shiny::icon("percent"),"Positiv procent"), "", box(title = "", width = 12, plotOutput("pos_tendens", height = 400))),
                               tabPanel(tagList(shiny::icon("procedures"),"Nye indlæggelser"), box(title = "", width = 12, plotOutput("ind_tendens", height = 400))),
                               tabPanel(tagList(shiny::icon("map-signs"),"Nye indlæggelser fordelt på Region"), box(title = "", width = 12, plotOutput("ind_reg_tendens", height = 500))),
                               tabPanel(tagList(shiny::icon("cross"),"Antal døde"), box(title = "", width = 12, plotOutput("dead_tendens", height = 400))),
                               tabPanel(tagList(shiny::icon("map-marked"),"Incidenstal per 100.000"), box(title = "", width = 12, plotOutput("inc_tendens", height = 1000))))),
           fluidRow(tabBox(title = tagList(shiny::icon("hand-point-up"), "Interaktivt"), id = "tabset2", height = "200px", width = 12, 
                           tabPanel(tagList(shiny::icon("people-arrows"),"Kontakttal"), "", box(title = "", width = 12, plotlyOutput("rt_plotly", height = 500))),
                    tabPanel(tagList(shiny::icon("percent"),"Positiv procent"), "", box(title = "", width = 12, plotlyOutput("pos_plotly", height = 700))),
                    tabPanel(tagList(shiny::icon("procedures"),"Nye indlæggelser"), box(title = "", width = 12, plotlyOutput("ind_plotly", height = 500))),
                    tabPanel(tagList(shiny::icon("cross"),"Antal døde"), box(title = "", width = 12, plotlyOutput("dead_plotly", height = 500)))))) 
    )



server <- function(input, output) {

    output$rt <- renderValueBox({
        
        last_row <- tail(rt, n = 1)
        
        valueBox(value = last_row$estimate, "Kontakttal", subtitle = paste0("Kontakttal ", last_row$date_sample), icon = icon("people-arrows"), 
                 color = "red")
    
    })
    
    output$pos <- renderValueBox({
      
      pos1 <- slice(pos, 1:(n()-2))
      
      pos2 <- tail(pos1, n = 1)
    
      valueBox(value = paste0(pos2$PosPct, " %"), "Positiv %", subtitle = paste0("Positiv % "), icon = icon("percent"),
               color = "blue")
      
    })
    
    output$ind <- renderValueBox({
      
      ind1 <- tail(ind, n = 1)
      
      valueBox(value = ind1$Total, "Nye indlæggelser", subtitle = paste0("Nye indlæggelser "), icon = icon("procedures"),
               color = "yellow")
      
    })
    
    output$dead <- renderValueBox({
      
      dead2 <- slice(dead, 1:(n()-2))
      
      dead1 <- tail(dead, n = 1)
      
      valueBox(value = dead1$Antal_døde, "Antal døde", subtitle = paste0("Antal døde "), icon = icon("cross"),
               color = "orange")
    
    })
    
    output$rt_tendens <- renderPlot({
        
        theme_set(theme_minimal())
   
        
        ggplot(rt, aes(as.Date(date_sample), estimate, group = 1)) +
            geom_smooth() +
            geom_hline(yintercept = 4) +
          scale_x_date(date_breaks = "1 month", date_labels = "%b %y", expand = c(0,22)) +
            labs(x = "",
                 y = "Kontakttal") +
          theme(panel.grid.minor = element_blank())
            
        
      })
    
    output$pos_tendens <- renderPlot({
      
      theme_set(theme_minimal())
      
      pos1 <- slice(pos, 1:(n()-2))
      
      ggplot(pos1, aes(as.Date(Date), PosPct, group = 1)) +
        geom_smooth(se = FALSE) +
        coord_cartesian(ylim = c(0,50)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %y" , expand = c(0,22)) +
        labs(x = "",
             y = "Positiv %") +
        theme(panel.grid.minor = element_blank())
      
    })
      
    output$ind_tendens <- renderPlot({
    
    ggplot(ind, aes(as.Date(Dato), Total, group = 1)) +
      geom_smooth(se = FALSE) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %y" , expand = c(0,22)) +
      labs(title = "Totalt for Danmark",
           x = "",
           y = "Nye indlæggelser") +
      theme(panel.grid.minor = element_blank())
    
    })
    
    
    output$ind_reg_tendens <- renderPlot({
      
      phoved <- ggplot(ind, aes(as.Date(Dato), Hovedstaden)) +
        geom_smooth(se = FALSE) +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Hovedstaden",
             x = "",
             y = "Nye indlæggelser") +
        theme(panel.grid.minor = element_blank())
      
      pnord <- ggplot(ind, aes(as.Date(Dato), Nordjylland)) +
        geom_smooth(se = FALSE) +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Nordjylland",
             x = "",
             y = "Nye indlæggelser") +
        theme(panel.grid.minor = element_blank())
      
      psjæl <- ggplot(ind, aes(as.Date(Dato), Sjælland)) +
        geom_smooth(se = FALSE) +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Sjælland",
             x = "",
             y = "Nye indlæggelser") +
        theme(panel.grid.minor = element_blank())
      
      psyd <- ggplot(ind, aes(as.Date(Dato), Syddanmark)) +
        geom_smooth(se = FALSE) +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Syddanmark",
             x = "",
             y = "Nye indlæggelser") +
        theme(panel.grid.minor = element_blank())
      
      pmidt <- ggplot(ind, aes(as.Date(Dato), Midtjylland)) +
        geom_smooth(se = FALSE) +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Midtjylland",
             x = "",
             y = "Nye indlæggelser") +
        theme(panel.grid.minor = element_blank())
      
      
      grid.arrange(phoved, pmidt, pnord, psjæl, psyd, ncol = 5)
      
      
    })
    
    output$dead_tendens <- renderPlot({
      
      dead1 <- slice(dead, 1:(n()-1))
      
      ggplot(dead1, aes(as.Date(Dato), Antal_døde, group = 1)) +
        geom_smooth(se = FALSE) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Totalt for Danmark",
             x = "",
             y = "Antal døde") +
        theme(panel.grid.minor = element_blank())
      
    })
    
    output$inc_tendens <- renderPlot({
      
      mapDK(values = "Kumulativ_incidens_.per_100000.", detail = "municipal", id = "Kommune_.navn.", data = mun, 
            guide.label = "Bekræftede smittede \nper 100.000 indbyggere")
  

    })
    
    output$rt_plotly <- renderPlotly({
        
        theme_set(theme_minimal())
        
        rt_plot <- ggplot(rt, aes(as.Date(date_sample), estimate, group = 1, text = paste("Kontakttal: ", estimate, "<br>Dato: ", as.Date(date_sample)))) +
            geom_line() +
          scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
            labs(x = "",
                 y = "Kontakttal")
        
        ggplotly(rt_plot, tooltip = c("text"))
        
    })
    
    output$pos_plotly <- renderPlotly({
      
      theme_set(theme_minimal())
      
      pos1 <- slice(pos, 1:(n()-2))
      
      pos_plot <- ggplot(pos1, aes(as.Date(Date), PosPct, group = 1, text = paste("Positiv %: ", PosPct, "<br>Dato: ", as.Date(Date)))) +
        geom_line() +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
        labs(x = "",
             y = "Procentuel andel positive prøver")
      
      ggplotly(pos_plot, tooltip = c("text"))
      
    })
    
    output$ind_plotly <- renderPlotly({
      
      ind_plot <- ggplot(ind, aes(as.Date(Dato), Total, group = 1, text = paste("Dato: ", as.Date(Dato),
                                                                                "<br>Nye indlæggelser: ", Total,
                                                                                "<br>Hovedstaden: ", Hovedstaden,
                                                                                "<br>Midtjylland: ", Midtjylland,
                                                                                "<br>Nordjylland: ", Nordjylland,
                                                                                "<br>Sjælland: ", Sjælland,
                                                                                "<br>Syddanmark:", Syddanmark))) +
        geom_line() +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %y" , expand = c(0,22)) +
        labs(x = "",
             y = "Nye indlæggelser") +
        theme(panel.grid.minor = element_blank())
      
      ggplotly(ind_plot, tooltip = c("text"))
      
      
      
    })
    
    output$dead_plotly <- renderPlotly({
      
    dead1 <- slice(dead, 1:(n()-2))
        
    dead_plot <- ggplot(dead1, aes(as.Date(Dato), Antal_døde, group = 1, text = paste("Antal døde: ", Antal_døde, "<br>Dato: ", as.Date(Dato)))) +
      geom_line() +
      ylim(c(0,35)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %y" , expand = c(0,22)) +
      labs(title = "Totalt for Danmark",
           x = "",
           y = "Antal døde") +
      theme(panel.grid.minor = element_blank())
    
    ggplotly(dead_plot, tooltip = c("text"))
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
