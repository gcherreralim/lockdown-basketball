library(shiny)
library(tidyverse)
library(DT)

df <- data.frame(Gene1 = LETTERS[1:5], Gene2 = LETTERS[6:10], stringsAsFactors = FALSE)
Expression <- data.frame(matrix(sample(1:50), ncol = 10, byrow = TRUE)) %>%
  `colnames<-`(LETTERS[1:10])

# Define UI for application that creates a datatables
ui <- fluidPage(fluidRow(selectInput("TheFile", "Select Cohort", 
                                     choices = c("Test_Dataset1.csv", "Test_Dataset2.csv", "Test_Dataset3.csv.csv"))),
                fluidRow(column(12, div(dataTableOutput("dataTable")))),
                
                # Application title
                titlePanel("Download Datatable")
                
                # Show a plot of the generated distribution
                , mainPanel(
                  DT::dataTableOutput("fancyTable"),
                  plotOutput("plot")
                  
                ) # end of main panel
                
) # end of fluid page

# Define server logic required to create datatable
server <- function(input, output, session) {
  
  myCSV <- reactive({
    # read.csv(input$TheFile)
    df
  })
  
  
  
  output$fancyTable <- DT::renderDataTable(
    datatable( data = myCSV()
               , extensions = 'Buttons',
               selection = 'single'
               , options = list( 
                 dom = "Blfrtip"
                 , buttons = 
                   list("copy", list(
                     extend = "collection"
                     , buttons = c("csv", "excel", "pdf")
                     , text = "Download"
                   ) ) # end of buttons customization
                 
                 # customize the length menu
                 , lengthMenu = list( c(10, 20, -1) # declare values
                                      , c(10, 20, "All") # declare titles
                 ) # end of lengthMenu customization
                 , pageLength = 10
               ) # end of options
               
    ) # end of datatables
  )
  
  observe({
    req(input$fancyTable_rows_selected)
    selRow <- myCSV()[input$fancyTable_rows_selected,]
    print(selRow[[1]])
    output$plot <- renderPlot({
      ggplot(Expression, aes_string(x=selRow[[1]], y=selRow[[2]])) + 
        geom_point(size=2) + 
        geom_smooth(method=lm, size=2) +  
        geom_rug(col="darkred", size=0.9, sides="bl", alpha = 0.3)
    }) 
  })
} # end of server

# Run the application 
shinyApp(ui = ui, server = server)