# ui portion
# Thu Oct 08 10:16:14 2020 ------------------------------

source("global.R")

options(shiny.maxRequestSize=2000*1024^2)


tags$head(
  tags$style(HTML("hr {border-top: 1px solid #000000;}
                  .sidebar-menu .treeview-menu{     padding: 0 0 0 0px;}"))
)

dashboardPage(
  dashboardHeader(titleWidth = 300,
                  title = "Missing 5% Review"),
  dashboardSidebar(
    width= 300,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon= icon("dashboard")),
      menuItem("Drill In", tabName = "drill", icon = icon("filter")),
      menuItem("Summary of Differences", tabName = "rawdata", icon = icon("th")),
      # menuItem("Full Tree", tabName =  "fullTree"),
    # ),
    hr(),
    menuItem("Filter Down Selections", icon = icon("adjust"), startExpanded = F,
             menuSubItem(icon = NULL,
               selectizeGroupUI(
                 id = "my-filters",
                 btn_label = "Reset Filters",
                 inline = FALSE,
                 params = list(
                   DptGroup = list(inputId = "DptGroup", title = "Select Service Group", placeholder = 'click to select'),
                   Prefix = list(inputId = "Prefix", title = "Select Department Prefix", placeholder = 'click to select'),
                   DPT = list(inputId = "DPT", title = "Select Department Name", placeholder = 'click to select'),
                   SpecialtyNM = list(inputId = "SpecialtyNM", title = "Select Specialty", placeholder = 'click to select'),
                   ComparisonFC = list(inputId = "ComparisonFC", title = "Select Comparison to FC", placeholder = 'click to select')
                   # DptEncGroup = list(inputId = "DptEncGroup", title = "Select Department Encounter Type", placeholder = 'select')
                 )
               )
             )
    ),
    hr(),
    menuItem("Drill In Grouping", icon = icon("filter"), startExpanded = F,
             menuSubItem(icon = NULL,
               selectInput("drillGroup","Drill-down grouping variable:",
                           c("No Grouping" = "Overall",
                             "Service Group" = "DptGroup",
                             "Department" = "DPT",
                             "Specialty" = "SpecialtyNM"),
                           selected = "Overall")
             )
    ),
    hr(),
    actionButton("expandFS", " Expand Treemap to Full Screen")
  )
  ),
  dashboardBody(
    tags$head(tags$script('
      // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 30;

        $("#drill").height(boxHeight);
        $("#plotSex").height(boxHeight - 250);
        $("#plotRace").height(boxHeight - 250);
        $("#plotFin").height(boxHeight - 250);
        $("#treemap_enc_full").height(boxHeight - 200);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    '),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    
    
    
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("count", width = 3),
                valueBoxOutput("countDPT", width = 3),
                valueBoxOutput("countEnc", width = 3),
                valueBoxOutput("PercDiff", width = 3)
              ),
              fluidRow(
                box(
                  title = "Treemap", status = "primary",
                  # height = "595",width = "12",
                  solidHeader = T, collapsible = TRUE,

                  column(width = 12,
                         plotOutput("treemap_enc")

                         )
                ),
                box(
                  title = "Overall Trends Summary", status = "primary",
                  # height = "595",width = "12",
                  solidHeader = T, collapsible = TRUE,
                  column(width = 12,
                         plotOutput("plot2")
                  )
                ),
                tags$head(tags$style(".modal-dialog{ width:1500px;}")), 
                bsModal("modalExample",
                        "Expanded View: Treemap",
                        "expandFS", # <----set the observer to the right button
                        size = "large",
                        plotOutput("treemap_enc_full"))
              )
      ),
      tabItem("rawdata",
              fluidRow(
                gt_output('summaryTabl')
              )
      ),
      tabItem("fullTree",
              # plotOutput("treemap_enc_full")
      ),
      tabItem("drill",
              fluidRow(
                box(
                  title = "Trends: Sex", status = "primary",
                  # height = "595",
                  width = 12,
                  solidHeader = T, collapsible = TRUE,
                  column(width = 12,
                         plotOutput("plotSex")
                  )
                  ),
                box(
                    title = "Trends: Race/Ethnicity Encounters", status = "primary",
                    # height = "595",
                    width = 12,
                    solidHeader = T, collapsible = TRUE,
                    column(width = 12,
                           plotOutput("plotRace")
                    )
                    ),
                box(
                  title = "Trends: Financial Class", status = "primary",
                  # height = "595",
                  width = 12,
                  solidHeader = T, collapsible = TRUE,
                  column(width = 12,
                         plotOutput("plotFin")
                  )
                )
                )
      )
    )
  )
)
