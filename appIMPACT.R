# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)

folder="C:/EPTD/Modeling/IMPACT Non Mod Git/OutputFiles/Scenarios"

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
files <- grep(pattern = ".rds",x = list.files(path = folder),value = TRUE)

choice <- select.list(choices = files, title = "Please Select IMPACT runs:")

cat("Reading\n",choice, "\nfrom\n",folder)

df_prep <- NULL

for(file_vector in paste0(folder,"/",choice)){
    df <- readRDS(file_vector)
    df$flag <- gsub(pattern = ".rds",replacement = "",x = choice)
    df_prep <- rbind(df_prep,df)
}

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("IMPACT Standard Results"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Select type of trend to plot
            selectInput(inputId = "indicator", label = strong("Indicator"),
                        choices = unique(df_prep$indicator),
                        selected = "Population"),
            
            sliderInput(inputId = "year", label = strong("Year"),
                        min = min(df$yrs),
                        value = max(df$yrs),
                        max = max(df$yrs),
                        step=1),
            
            checkboxInput("free_y", label = strong("Free Y-axis"), 
                          value = FALSE, 
                          width = NULL)
    ),
    # Output: Description, lineplot, and reference
    mainPanel(
        plotOutput(outputId = "impact_plot", height = "1200px"),
        # textOutput(outputId = "desc"),
        # tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
        #tableOutput("DataTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dfx <- reactive({
        df_prep[df_prep$indicator == input$indicator,
        ]
    })
    
    output$DataTable <- renderTable({
        dt <- df_prep[df_prep$indicator == input$indicator,]
        dt
    },include.rownames=FALSE)

        
    output$impact_plot <- renderPlot({
        p <- ggplot(dfx(), aes(x = dfx()$yrs, y = dfx()$value)) + 
            theme_minimal(base_size = 12) +
            facet_wrap(.~region) + 
            {if(input$free_y) facet_wrap(.~region, scales = "free_y")}+
            geom_line(group=dfx()$flag, aes(color=dfx()$flag)) +
            geom_point(shape=1) + 
            ylab(unique(dfx()$unit)) +
            ggtitle(unique(dfx()$indicator)) + 
            theme(legend.position = "bottom") + 
            theme(axis.text.x = element_text(angle = 90))

        return(p)
    }, res = 96)
}

# Run the application 
runApp(shinyApp(ui = ui, server = server),launch.browser=FALSE)