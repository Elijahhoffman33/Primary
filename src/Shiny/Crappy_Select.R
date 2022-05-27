

ui <- fluidPage(
  plotlyOutput('myPlot'),
  verbatimTextOutput("se")
)

server <- function(input, output, session){
  output$myPlot = renderPlotly({
    plot_ly(data = plot1, x = ~BD, y = ~SOC) %>%
      layout(dragmode = "select")
  })
  
  output$se <- renderPrint({
    d <- event_data("plotly_selected")
    d
  })
}

shinyApp(ui, server)

plot1[,cl:=.I%in%c(0
                   ,1,
                   2,
                   3,
                   7,
                   9,
                   17,
                   22)]