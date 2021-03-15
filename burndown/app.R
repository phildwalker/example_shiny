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
library(shinycssloaders)
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
    shinydashboardPlus::dashboardPagePlus(
        skin = "blue",
        sidebar_fullCollapse = TRUE,
        
        dashboardHeader(title = "COVID-19::Pent-Up Demand Visualizer",
                        titleWidth = 500),
        
        dashboardSidebar(
            width = 300,
            useShinyjs(),
            sidebarMenu(id = "sidebarmenu", 
                        menuItem("Overview",tabName = "Overview", icon = icon("user-circle") ),
                        menuItem( "Demand Simulator", tabName = "Demand-Sim", icon = icon("external-link-alt")),
                        selectizeInput(inputId = "serviceLine", 
                                       label= "Select Service Line",
                                       choices=UniqueSL,
                                       # options = Placeholder_list
                                       selected = "Orthopedics"
                        )
            )
        ),
        
        dashboardBody(useShinyjs(), useShinyalert(),
                      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                      tabItems(
                          # id = "tabs",
                          tabItem(tabName = "Overview",
                                  h2("Welcome!"),
                                  h4("Thank you for engaging with this interactive app to better understand pent-up demand."), #'<br/>',
                                  h5("Goal is to expand upon the POC developed which built a forecast from historical surgery trends and compared the projection to that of the actuals post elective procedures being cancelled."),
                                  h5(""),
                                  h5("The POC only used the Orthopedics SL for the the first analysis."),
                                  h5(""),
                                  h5(HTML("The process was to: <br> 
                                          * pull in historical data from 2017 to now <br> 
                                          * aggregate by week <br> 
                                          * drop data from 3/12/2020 onwards from the forecast model <br> 
                                          * build model <br> 
                                          * compare forecast results to that of the actuals from 3/12 onwards <br> 
                                          * The cumulative difference between the two is what we are calling pent-up demand")),
                          ),
                          tabItem(tabName = "Demand-Sim",
                                  h2("Select the Service Line"),
                                  h4("This selection will build a forecast"),
                                  
                                  fluidRow(
                                      textOutput("ProjDemand"),
                                      plotOutput('FCSummary') %>% withSpinner(),
                                      tableOutput('SummaryTable') %>% withSpinner()
                                      # tableOutput('DatExampl')
                                      # conditionalPanel("$('#FCSummary').hasClass('recalculating')", 
                                      #                  tags$div('Loading ... '))
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
        dat <- Dat_For_FC()   # FCData()$fc
        
        dat %>% head(5)
        
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
        # ActualDat <- Dat_For_FC()
        
        ActualDat <- 
            readRDS(here::here("data", "Surgical_Aggreg.rds")) %>%
            filter(ServiceDSC %in% input$serviceLine,
                   SchedMo >= FilterDate) %>% 
            group_by(ServiceDSC, SchedMo) %>% 
            summarise(SumN = sum(n, na.rm=TRUE)) %>%
            ungroup() %>%
            rename(y = SumN,
                   ds = SchedMo) %>% 
            mutate(ds = as.POSIXct(ds))
        
        
        FC_Comp <-
            plot(FCData()$model, FCData()$fc) +
            labs(title = glue::glue("Weekly Forecast of Procedures for {input$serviceLine}"))
        
        FC_Comp +
            geom_point(data= ActualDat,
                       aes(ds, y), color = "maroon", size = 1) +
            geom_line(data= ActualDat,
                      aes(ds, y), color = "maroon", size = 1) +
            labs(subtitle = "Red points are actual surgeries during COVID",
                 y = NULL, x= NULL)
        
        
    })
    
   SummaryDiff <- reactive({
        fcDat <- FCData()$fc
        
        ActualDat <- 
            readRDS(here::here("data", "Surgical_Aggreg.rds")) %>%
            filter(ServiceDSC %in% input$serviceLine,
                   SchedMo >= FilterDate) %>% 
            group_by(ServiceDSC, SchedMo) %>% 
            summarise(SumN = sum(n, na.rm=TRUE)) %>%
            ungroup() %>%
            rename(y = SumN,
                   ds = SchedMo) %>% 
            mutate(ds = as.Date(ds))
        
        
        # Potential_Demand <-
        fcDat %>% 
            filter(ds >= FilterDate) %>% 
            mutate(ds = as.Date(ds)) %>% 
            select(ds, yhat) %>% 
            left_join(.,
                      ActualDat,
                      by = c("ds")) %>% 
            mutate(Diff = yhat - y) 
    })
    
    output$SummaryTable <- renderTable({
        fcDat <- SummaryDiff()
        
        head(fcDat)
    })
    
    output$ProjDemand <- renderText({
        dat <- SummaryDiff()
        
        totalDemand <- round(sum(dat$Diff, na.rm = TRUE), 1)
        
        glue::glue("The total projected demand is: {totalDemand} people")
        
    })
    
}




# Run the application 
shinyApp(ui = ui, server = server)



