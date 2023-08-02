
library(shiny)
library(shinythemes)
library(shinydashboard)
library(dbplyr)
library(dplyr)
library(tidyr)
library(stringr)
library(DBI)
library(RSQLite)
library(DT)

con <- dbConnect(RSQLite::SQLite(), "./data/iso_standards.sqlite")

ui <- dashboardPage(

  dashboardHeader(title = "ISO standards dataset"),

  dashboardSidebar(

                selectInput(inputId = "table", 
                            label = "Select a table:", 
                            choices = dbListTables(con), 
                            selected = NULL),
                
                br(),
                
                downloadButton('download',"Download the dataset", style = "width:90%;", style = "margin-left: 10px"),
                
                br(),
                br(),
                
                p(HTML("The database can also be downloaded <a href='https://www.dropbox.com/s/0b2dpsjne85vsoz/iso_standards.sqlite?dl=1'>here</a>."),
                  style = "margin-left: 10px")

    ),
                
  
  dashboardBody(
    
    h3("Data coverage"),
    DTOutput(outputId = "summary_table"),
    
    h3("Data table output"),
    DTOutput(outputId = "table_output")
                
                
  )
)

server <- function(input, output) {
  
  # Load the data into a reactive object
  data <- reactive(con)
  
  # Set up the table output
  output$summary_table <- renderDT({
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
      select(Year, `Number of observations`) %>%
      arrange(desc(Year)) %>%
      ungroup() %>%
      as_tibble() %>%
      datatable(options = list(lengthMenu = c(3, 50)))

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
