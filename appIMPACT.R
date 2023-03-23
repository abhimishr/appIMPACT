# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)

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

df_prep$yrs <- as.factor(as.character(df_prep$yrs))

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("IMPACT Standard Results"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(

        sidebarPanel(
                 selectInput(inputId = "indicator", label = strong("Indicator"),
                                                 choices = unique(df_prep$indicator),
                                                 selected = "Population"),
                 
                 sliderInput(inputId = "year", label = strong("Year"),
                                                 min = min(df$yrs),
                                                 value = c(min(df$yrs),max(df$yrs)),
                                                 max = max(df$yrs),
                                                 step=1,sep = ""),
                 
                 checkboxInput("donum1", "Line plot", value = T),
                 checkboxInput("donum2", "Bar plot", value = T),
                 
                 sliderInput("wt1","Lineplot Weight",min=1,max=3,value=3),
                 sliderInput("wt2","Barplot weight",min=1,max=3,value=3),
                 checkboxInput("free_y", label = strong("Free Y-axis"), 
                                                      value = FALSE, 
                                                      width = NULL),
                 
                 width=3
                 ),

    # Output: Description, lineplot, and reference
    # mainPanel(
    #     plotOutput(outputId = "impact_plot", height = "1200px"),
    #     # textOutput(outputId = "desc"),
    #     # tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
    #     #tableOutput("DataTable")
    #     ),
    
    mainPanel(plotOutput(outputId="plotgraph", width="1400px",height="900px"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dfx <- reactive({
        df_prep[df_prep$indicator == input$indicator,]
    })

    
    p_line <-  reactive({
        if (!input$donum1) return(NULL)
        ggplot(dfx(), aes(x = dfx()$yrs, y = dfx()$value)) + 
            theme_minimal(base_size = 15) +
            facet_wrap(.~region) + 
            {if(input$free_y) facet_wrap(.~region, scales = "free_y")}+
            geom_line(group=dfx()$flag, aes(color=dfx()$flag)) +
            geom_point(shape=1) + 
            ylab(unique(dfx()$unit)) +
            xlab("Years") +
            ggtitle(unique(dfx()$indicator)) + 
            theme(legend.position = "bottom",legend.direction = "vertical") + 
            theme(axis.text.x = element_text(angle = 90)) +
            guides(color=guide_legend(title="IMPACT Run")) 
    })
    
    p_bar <-  reactive({
        if (!input$donum2) return(NULL)
        ggplot(dfx(), aes(x = dfx()$yrs, y = dfx()$value)) + 
            theme_minimal(base_size = 15) +
                facet_wrap(.~flag) + 
            #    {if(free_y) facet_wrap(.~region, scales = "free_y")}+
            geom_area(position='stack',aes(fill=dfx()$region,group=dfx()$region),color="black") +
            ylab(unique(dfx()$unit)) +
            xlab("Years") +
            ggtitle(unique(dfx()$indicator)) + 
            theme(legend.position = "bottom") + 
            theme(axis.text.x = element_text(angle = 90)) +
            guides(fill=guide_legend(title="Regions")) 
    })
    
    output$plotgraph = renderPlot({
        ptlist <- list(p_line(),p_bar())
        wtlist <- c(input$wt1,input$wt2)
        # remove the null plots from ptlist and wtlist
        to_delete <- !sapply(ptlist,is.null)
        ptlist <- ptlist[to_delete] 
        wtlist <- wtlist[to_delete]
        if (length(ptlist)==0) return(NULL)
        
        grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
    })
}

# Run the application 
runApp(shinyApp(ui = ui, server = server),launch.browser=FALSE)
