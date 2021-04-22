library(shiny)
library(tidyverse)
library(mdsr) 
library(fivethirtyeight)
library(colourpicker)
library(shinythemes)

# data
bechdeldata <- bechdel %>%
  select(year, title, test, clean_test, binary, budget:intgross, budget_2013:intgross_2013)%>%
  filter(budget < 130000000)

levels(bechdeldata$clean_test) <- c("There are fewer than 2 women", 
                                     "Women don't talk to each other", "Women only talk about men",
                                     "Passes (contested) Bechdel Test",
                                     "Passes Bechdel Test")

y_choice_values = names(bechdeldata)[6:11]
y_choice_names <- c("Budget", "Domestic Gross", 
                    "International Gross", 
                    "Budget (corrected for 2013 Inflation)", 
                    "Domestic Gross (corrected for 2013 Inflation", 
                    "International Gross (corrected for 2013 Inflation")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
  h1("Bechdel Tests and Movies"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "years", label = "Include movies from the years: ", min = 1970, max = 2013,
                  value = c(1970, 2013)),
      checkboxInput(inputId = "showoutcome", label = "Check to show Bechdel Test specifics for Count Plot", value = TRUE),
      radioButtons(inputId = "y"
                 , label = "Choose an outcome variable of interest for Budget Boxplot:"
                 , choiceValues = y_choice_values
                 , choiceNames = y_choice_names)
      
      ),
    mainPanel(
      tabsetPanel(type = "tabs"
        ,tabPanel("Count Plot", plotOutput(outputId = "plot")),
        tabPanel("Budget Boxplot", plotOutput(outputId = "plottwo")),
        tabPanel("Table", dataTableOutput(outputId = "table"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  use_data <- reactive({
    data <- filter(bechdeldata, (year >= (input$years[1]) & year <= (input$years[2])))
  })
  
  x <- reactive({
    input$showoutcome 
  })
  
  yvar <- reactive({
    input$y
  })
  
  startyear <- reactive({
    input$years[1]
  })
  
  endyear <- reactive({
   input$years[2]
  })

  output$plot <- renderPlot({
    if(x() == TRUE){
      ggplot(data = use_data(), aes(x = binary, fill = clean_test)) + 
        geom_bar() + 
        geom_text(stat = 'count', aes(label=..count..), position = position_stack(vjust = 0.5), size = 6) +
        scale_fill_brewer(palette="Set1") +
        labs(fill = "Bechdel Test Results") +
        ggtitle(paste("Count Plot of Movies from", startyear(), "to", endyear())) +
        xlab("Bechdel Test Result") + ylab("Count") +
        theme(
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = "black"), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent")) # get rid of legend panel bg
    }
    else{
      ggplot(data = use_data(), aes(x = binary, fill = binary)) + geom_bar() +
      geom_text(stat = 'count', aes(label=..count..), position = position_stack(vjust = 0.5), size = 6) +
      scale_fill_brewer(palette="Set1") +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank(), # get rid of major grid
            panel.grid.minor = element_blank(), # get rid of minor grid
            legend.background = element_rect(fill = "transparent"), # get rid of legend bg
            legend.box.background = element_rect(fill = "transparent"))+ # get rid of legend panel bg) +
      ggtitle(paste("Count Plot of Movies from", startyear(), "to", endyear())) +
      xlab("Pass or Fail") + ylab("Count")
    }
  })
  
  output$plottwo <- renderPlot({
      ggplot(data = use_data(), aes_string(x = "binary", y = yvar(), fill = "binary")) + geom_boxplot() +
      scale_fill_brewer(palette="Set1")+
      theme(legend.position = "none",
            panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank(), # get rid of major grid
            panel.grid.minor = element_blank(), # get rid of minor grid
            legend.background = element_rect(fill = "transparent"), # get rid of legend bg
            legend.box.background = element_rect(fill = "transparent"))+ # get rid of legend panel bg) +
      ggtitle(paste("Budget Boxplot of Movies from", startyear(), "to", endyear())) 
  })
  
  output$table <- renderDataTable({
    use_data()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

