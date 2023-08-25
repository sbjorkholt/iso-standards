
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(dbplyr)
library(dplyr)
library(tidyr)
library(stringr)
library(DBI)
library(RSQLite)
library(DT)
library(ggplot2)
library(plotly)
library(knitr)

con <- dbConnect(RSQLite::SQLite(), "./data/iso_standards.sqlite")

source("codebook-descriptions.R")

ui <- dashboardPage(

  dashboardHeader(
  
    title = "StanDat - International Standards Database",
    titleWidth = 500),

  dashboardSidebar(
    
    sidebarMenu(
      id = "mysidebar",
      menuItem("Database", tabName = "tab1", icon = icon("chart-pie")),
      menuItem("Codebook", tabName = "tab2", icon = icon("book"))
    ),
    
    br(),
    
    div("This is a database with several datasets. You can either select a dataset or download the whole database.",
        style = "style = width:90%;", style = "margin: 10px"),
    
    br(),
    
    div("The database is categorized into 'Standards', 'Participation', 'Historical' and 'Certifications', each category with 2-3 datasets.
    Once you have selected a dataset, you can click on the 'Download dataset' button to get a csv-file.", 
        style = "style = width:90%;", style = "margin: 10px"),
    
    br(),
    
    div(HTML("To get a more detailed understanding each dataset, please consult the codebook."), # <a href='https://www.dropbox.com/scl/fi/wnsczwmn09a1xsl58xf3t/codebook.pdf?rlkey=3b3hhhwzdh6m9lfl2hlz3poln&dl=1'>the codebook</a>."), 
        style = "style = width:90%;", style = "margin: 10px"),
    
    br(),

                selectizeInput(inputId = "table", 
                               label = "Select a dataset:", 
                               choices = list(
                                 Standards = list(`Status` = "standards_status", `SDGs` = "standards_sdgs", `Life cycle` = "standards_life_cycle"),
                                 Participation = list(`Countries` = "participants", `Organizations` = "liaison"),
                                 Historical = list(`Membership` = "historical_memberships", `Technical committees` = "historical_tc_creation"),
                                 Certifications = list(`Per country` = "country_certifications",
                                                       `Per industry` = "industry_certifications",
                                                       `Per country and industry` = "country_per_industry_certifications")), 
                               multiple = FALSE,
                               selected = "standards_status"),
                
                br(),
                
                downloadButton('download', "Download the dataset", style = "width:90%;", style = "margin-left: 10px"),
                
                br(),
                br(),
                br(),
                
                p(HTML("The entire database can be downloaded <a href='https://www.dropbox.com/s/0b2dpsjne85vsoz/iso_standards.sqlite?dl=1'>here</a>."),
                  style = "margin-left: 10px")

    ),
                
  
  dashboardBody(
    
    tags$script('$(".sidebar-menu a[data-toggle=\'tab\']").click(function(){window.scrollTo({top: 0});})'),
    tags$script(HTML("$('body').addClass('fixed');")), 
    useShinyjs(),
    
    tabItems(
      tabItem(tabName = "tab1",
    
          column(width = 12,
                 fluidRow(
                 box(h3("Data coverage"),
                     plotlyOutput(outputId = "observations")),
                 
                   box(h3("Variable definitions"),
                       br(),
                       tableOutput("description")))
                 
                 ), 
          
          br(),
          br(), 
          
          h3("Data table output"),
          DTOutput(outputId = "table_output")
      ),
      
      tabItem(tabName = "tab2",
              fluidRow(
                column(width=10, offset=1,
                       fluidRow(
                         includeHTML("codebook.html")
                       )))))
                
  )
)

server <- function(input, output) {
  
  # Load the data into a reactive object
  data <- reactive(con)
  
  output$description <- renderTable({
    text <- switch(input$table,
                   "standards_status" = standards_cb,
                   "standards_sdgs" = sdgs_cb,
                   "standards_life_cycle" = life_cycle_cb,
                   "participants" = participants_cb,
                   "liaison" = liaison_cb,
                   "historical_memberships" = member_historical_cb,
                   "historical_tc_creation" = tc_historical_cb,
                   "country_certifications" = country_certifications_cb,
                   "industry_certifications" = industry_certifications_cb,
                   "country_per_industry_certifications" = country_per_industry_certifications_cb
                   
    )
  })
  
  # Set up the table output
  output$observations <- renderPlotly({
    # Get the selected table
    table_selected <- input$table
    
    # Get the table from the data object
    table_data <- tbl(con, table_selected)
    
    validate(
      need(input$table != "sectors", "No years in this dataset.")
    )
    
    # Return the table data
    table_data %>%
      group_by(year) %>%
      count(name = "Number of observations") %>%
      rename("Year" = "year") %>%
      ungroup() %>%
      select("Year", "Number of observations") %>%
      as_tibble() %>%
      plot_ly(x = ~Year, y = ~`Number of observations`,
      type = "bar")
  
      # arrange(desc(Year)) %>%
      # ungroup() %>%
      # as_tibble() %>%
      # datatable(options = list(lengthMenu = c(3, 50)))

  })
  
  # Set up the table output
  output$table_output <- renderDT({
    # Get the selected table
    table_selected <- input$table
    
    # Get the table from the data object
    table_data <- tbl(con, table_selected)
    
    # Return the table data
    table_data   %>%
      as_tibble() %>%
      datatable()
    
  })
  
  # Wrap the table_selected inside a reactive expression
  table_selected <- reactive({
    input$table
  })
  
  # Use a single reactive expression for table_data
  table_data <- reactive({
    tbl(con, table_selected())
  })
  
  # Update the downloadHandler
  output$download <- downloadHandler(
    filename = function() {
      paste0(table_selected(), ".csv")  # Use the reactive table_selected here
    },
    content = function(fname) {
      write.csv(table_data(), fname)
    }
  )
}

shinyApp(ui = ui, server = server)
