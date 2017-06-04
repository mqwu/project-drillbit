library(plotly)


source("./load_data.R", local = T)
max_plots <- length(dat)
wells <- unlist(lapply(1:max_plots, function(i) sub("(.*csv/)(.*)(.csv)","\\2",eval(as.name(paste0("d",i,"_path"))))))

server <- function(input, output) {
  
  output$timeplot <- renderPlotly({
    my_i <- match(input$well, wells)
    plot_ly(dat[[my_i]], 
            x=~Time,
            y=~WF,
            type='scatter',
            mode='markers+lines'
    ) %>% layout(title = wells[my_i])
  })
  
}

ui <- fluidPage(
  titlePanel("Drill bit wearing"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        selectInput("well", 
                    "Select Well", 
                    choices = wells))
    ),  
    mainPanel(
      wellPanel(
        plotlyOutput("timeplot",height='750px'))
    ))
)

shinyApp(ui = ui, server = server)
