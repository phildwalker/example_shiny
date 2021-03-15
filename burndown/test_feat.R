#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# The goal of this app is to provide an interactive environment for users to investigate potential pent up demand
# Developer: P. Walker
# Mon Apr 13 20:51:26 2020 ------------------------------


library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(shinyjs)
library(prophet)

# Prepopulate data
SL_drop <- readRDS(here::here("data", "SL_toDrop.rds"))
OR_SurgCent <- readRDS(here::here("data", "OR_SurgCnt_Names.rds"))
Surg_Clean <- readRDS(here::here("data", "Surgical_Aggreg.rds"))
FilterDate <- as.Date(c("2020-03-08"))

UniqueSL <-
  Surg_Clean %>% 
  distinct(ServiceDSC) %>% 
  filter(!ServiceDSC %in% SL_drop)

# Define UI for application that draws a histogram

Placeholder_list <- list(
  placeholder = 'Please select an option below',
  onInitialize = I('function() { this.setValue(""); }')
)

ui <- 
  dashboardPagePlus(
    skin = "blue",
    sidebar_fullCollapse = TRUE,
    
    dashboardHeader(title = "COVID-19::Pent-Up Demand Visualizer"),
    
    dashboardSidebar(
      width = 300,
      useShinyjs(),
      sidebarMenu(id = "sidebarmenu", 
                  menuItem("Overview",tabName = "Overview", icon = icon("user-circle") ),
                  menuItem( "Demand Simulator", tabName = "Demand-Sim", icon = icon("external-link-alt")),
                  selectizeInput(inputId = "serviceLine", 
                                 label= "Select Service Line",
                                 choices=UniqueSL,
                                 options = Placeholder_list
                                 # ,selected = "Orthopedics"
                              )
      )
    ),
    
    dashboardBody(useShinyjs(), useShinyalert(),
                  tabItems(
                    # id = "tabs",
                    tabItem(tabName = "Overview",
                            h2("Welcome!"),
                            h4("Thank you for engaging with this interactive app to better understand pent-up demand."), #'<br/>',
                            h5("Please fill out every section completely and press submit to utilize the app....")
                    ),
                            tabItem(tabName = "Demand-Sim",
                                    h2("Select the Service Line"),
                                    h4("This selection with build a forecast"),
                                    
                                    fluidRow(
                                      # plotOutput("FCSummary")
                                      tableOutput('DatExampl')
                                    )
                            )
                    ))
  )




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  Dat_For_FC <- reactive({
    
    # Surg_Clean <- readRDS(here::here("data", "Surgical_Aggreg.rds"))
    
    readRDS(here::here("data", "Surgical_Aggreg.rds")) %>%
      filter(ServiceDSC %in% input$serviceLine,
             SchedMo < FilterDate) %>% 
      group_by(ServiceDSC, SchedMo) %>% 
      summarise(SumN = sum(n, na.rm=TRUE)) %>%
      ungroup() %>%
      rename(y = SumN,
             ds = SchedMo) %>%
      mutate(floor = 0,
             cap = max(y)+1) %>%
      ungroup()
    
  })
  
  output$DatExampl <- renderTable({
    dat <- Dat_For_FC()
  
    dat %>% 
      head(6)
    
  })
  
  
  FCData <- reactive({
    # code to build the output.
    # req(input$sidebarmenu)
    
    fc_data <- Dat_For_FC()
    
    m_logistic <- prophet(fc_data
                          ,daily.seasonality = TRUE
                          ,weekly.seasonality = TRUE
                          # ,yearly.seasonality = TRUE
                          ,growth = 'logistic'
    )
    
    # Forecast out 12 weeks
    future_logistic <- make_future_dataframe(m_logistic, periods = 12, freq = "week") %>%
      mutate(floor = 0,
             cap = max(fc_data$y)+1)
    
    forecast_logistic <- predict(m_logistic, future_logistic)
    
    list(model = m_logistic, fc = forecast_logistic)
    
  })
  
  output$FCSummary <- renderPlot({
    ActualDat <- Dat_For_FC()
    
    FC_Comp <- plot(FCData()$model, FCData()$fc) +
      labs(title = glue::glue("Weekly Forecast of {input$SL-input} Procedures"))
    
    FC_Comp +
      geom_point(data= ActualDat %>% filter(SchedWeek >= FilterDate) %>% rename(y = n, ds = SchedMo) %>% mutate(ds = as.POSIXct(ds)), 
                 aes(ds, y), color = "maroon", size = 1) +
      geom_line(data= ActualDat %>% filter(SchedWeek >= FilterDate) %>% rename(y = n, ds = SchedMo) %>% mutate(ds = as.POSIXct(ds)), 
                aes(ds, y), color = "maroon", size = 1) +
      labs(subtitle = "Red points are actual surgeries during COVID",
           y = NULL, x= NULL)
    
    
  })
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)



