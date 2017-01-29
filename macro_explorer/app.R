library(shiny)
library(plotly)
library(DT)
library(formattable)
library(RPostgreSQL)

# pull data from Postgres DB
conn <- dbConnect(dbDriver('PostgreSQL'), dbname='compose', host='aws-us-east-1-portal.5.dblayer.com', port='16513', user='admin', password='THRRRJQXFNTFWHDH')
on.exit(dbDisconnect(conn))
popvar = dbGetQuery(conn, "SELECT * FROM quandl_test LIMIT 100")

# define plotly visuals 

f1 <- list(
  family = "sans-serif",
  size = 12
)
f2 <- list(
  family = "Old Standard TT, serif",
  size = 14,
  color = "black"
)
a <- list(
  title = "AXIS TITLE",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = 45,
  tickfont = f2,
  exponentformat = "E"
)

b <- list(
      color = "rgb(61, 133, 198)", 
      dash = "solid", 
      shape = "spline", 
      smoothing = 1, 
      width = 0.5)

# UI code 

ui <- fluidPage(
  
    br(),

sidebarLayout(position = "right",
  
  sidebarPanel(
               selectInput("country","Select Country", c("All Countries","KOR", "AFG")),
               
               conditionalPanel(
                 condition = "input.country == 'All Countries'",
                 selectInput(
                   "breaks", "X Dimension",
                   c("Sturges",
                     "Scott",
                     "Freedman-Diaconis",
                     "[Custom]" = "custom")),
                 selectInput(
                   "breaks", "Y Dimension",
                   c("Sturges",
                     "Scott",
                     "Freedman-Diaconis",
                     "[Custom]" = "custom")),
                 selectInput(
                   "breaks", "Z Dimension",
                   c("Sturges",
                     "Scott",
                     "Freedman-Diaconis",
                     "[Custom]" = "custom")),
                 selectInput(
                   "breaks", "Size Dimension",
                   c("Sturges",
                     "Scott",
                     "Freedman-Diaconis",
                     "[Custom]" = "custom")),
                 selectInput(
                   "breaks", "Color Dimension",
                   c("Sturges",
                     "Scott",
                     "Freedman-Diaconis",
                     "[Custom]" = "custom"))
                 ),
               
               conditionalPanel(
                 condition = "input.country !== 'All Countries'",
                 selectInput(
                   "breaks", "Select Time Series",
                   c("Sturges",
                     "Scott",
                     "Freedman-Diaconis",
                     "[Custom]" = "custom")),
                 selectInput(
                   "breaks", "Compare",
                   c("Sturges",
                     "Scott",
                     "Freedman-Diaconis",
                     "[Custom]" = "custom")),
                 dateRangeInput("dates",NULL)
               )
  ),
  
  mainPanel(
    fluidRow(uiOutput("ui2")),
    fluidRow(uiOutput("ui"))
    
  )
)
)



# server code 

server <- function(input, output) {
  
  output$value1 <- renderPrint({ input$dates })
  output$value2 <- renderPrint({ input$radio })
  
  output$ui <- renderUI({
    if (input$country == "All Countries"){
      formattableOutput('ex1')
    } else {
      plotlyOutput("TestPlot1")
    }
  })
  
  output$ui2 <- renderUI({
    if (input$country == "All Countries"){
      plotlyOutput("TestPlot2")
    } else {
      NULL
    }
  })
    
  output$ex1 <- renderFormattable({
    formattable(iris, list(
      Sepal.Length = color_tile("white", "orange")
    ))
  })
  
  output$TestPlot1 <- renderPlotly({
    NomFXts <- plot_ly(popvar, x = index, y = popvar[,input$country], type = 'scatter', mode = 'lines', name = 'Population', line = b, fill = "tonexty", fillcolor = "rgba(19, 27, 88, 0.5)")
    NomFXts <- layout(NomFXts, title = 'Population Over Time', yaxis = list(type = "log", showgrid = FALSE, tickfont = f1, title = ""), xaxis = list(showgrid = FALSE, tickfont = f1, title = ""))
    NomFXts
  })
    
    output$TestPlot2 <- renderPlotly({
      p <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, 
                   marker = list(color = ~mpg, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
      p
      
  })
  
}

shinyApp(ui, server)
