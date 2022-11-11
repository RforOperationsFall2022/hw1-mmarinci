#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)
library(tools)
library(tidyverse)
library(ggplot2)
library(ggalt)
library(DT)
library(wesanderson)

ob2020 <- read.csv(file = "2020-operating.csv")
ob2021 <- read.csv(file = "2021-amended-operating.csv")
ob2022 <- read.csv(file = "2022-amended-operating.csv")
budget <- rbind(ob2020, ob2021, ob2022)

data <- budget %>% 
  filter(Type == "Expenditure") %>%
  select(Year, Department, Amount) %>%
  group_by(Year, Department) %>%
  summarize(Total = sum(Amount)) %>%
  arrange(Total) %>%
  group_by(Year) %>%
  mutate(Percentage = round(Total / sum(Total) * 100, 1))

data_wide <- data %>%
  pivot_wider(names_from = Year, values_from = c(Total, Percentage)) %>%
  arrange(Percentage_2020)

data_wide$Department <- factor(data_wide$Department, levels = as.character(data_wide$Department))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Operating Budget for Pittsburgh 2020-2022"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          # Select a year of data to show ----------------------------------
          selectInput(inputId = "year", 
                      label = "Choose a budget year:",
                      choices = c(2020, 2021, 2022), 
                      selected = "2022"),
        
          # Select a department to show ----------------------------------
          checkboxGroupInput(inputId = "selected_dept",
                           label = "Select department(s):",
                           choices = unique(data$Department),
                           selected = c("ETHICS BOARD","OFFICE OF EQUITY","PS - FIRE BUREAU","PS - POLICE BUREAU"))
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("deptBarplot"),
           br(),
           br(),
           br(),
           plotOutput("dumbbell"),
           br(),
           br(),
           br(),
           plotOutput("piechart"),
           br(),
           br(),
           br(),
           DT::dataTableOutput(outputId = "budgetTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  year_subset <- reactive({
    req(input$year, input$selected_dept)
    filter(data, Year %in% input$year & Department %in% input$selected_dept)
  })
  
  dumbbell_subset <- reactive({
    req(input$selected_dept)
    filter(data_wide, Department %in% input$selected_dept)
  })

  # generate barplot--------------------------------------------------------  
  output$deptBarplot <- renderPlot({
      ggplot(year_subset(), aes(Department, Total)) +
        geom_col(fill="lightblue") +
        labs(x=NULL,
             y=NULL,
             title = "Budget by Department and Year") +
        theme(axis.text.x = element_text(angle = 90, size = 10)) +
        scale_y_continuous(labels = scales::comma) + 
        ylab("Total Budget ($)") +
        theme(plot.title = element_text(hjust=0.5, face="bold"),
            plot.background=element_rect(fill="#FFFFFF"),
            panel.background=element_rect(fill="#FFFFFF"),
            panel.grid.minor=element_blank(),
            panel.grid.major.y=element_blank(),
            panel.grid.major.x=element_line(),
            panel.border=element_blank()
        )
        })

    # Generate a dumbbel chart----------------------------------------------------
    output$dumbbell <- renderPlot({  
      ggplot(dumbbell_subset(), aes(x=Percentage_2020, xend=Percentage_2022, y=Department, group=Department)) + 
        geom_dumbbell(color="lightblue", 
                      size=2,
                      colour_xend="blue",
                      show.legend = TRUE) + 
        labs(x=NULL, 
             y=NULL, 
             title="Departments by Percent of Total Budget for 2020 vs. 2020", 
             subtitle="Percentage Point Change: 2020 vs 2022") +
        theme(plot.title = element_text(hjust=0.5, face="bold"),
              plot.background=element_rect(fill="#FFFFFF"),
              panel.background=element_rect(fill="#FFFFFF"),
              panel.grid.minor=element_blank(),
              panel.grid.major.y=element_blank(),
              panel.grid.major.x=element_line(),
              axis.ticks=element_blank(),
              panel.border=element_blank()
          )
          })
    
    # Generate a pie chart----------------------------------------------------
    output$piechart <- renderPlot({  
      ggplot(year_subset(), aes(x = '', y = Total, fill = Department)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        labs(x=NULL,
             y=NULL,
             title = "Pie Chart of Budget by Department and Year") +
        scale_y_continuous(labels = scales::comma) + 
        scale_fill_manual(values = wes_palette("Darjeeling2", 30, type = "continuous")) +
        theme(plot.title = element_text(hjust=0.5, face="bold"),
              plot.background=element_rect(fill="#FFFFFF"),
              panel.background=element_rect(fill="#FFFFFF"),
              panel.grid.minor=element_blank(),
              panel.grid.major.y=element_blank(),
              panel.grid.major.x=element_line(),
              panel.border=element_blank()
        )
    })
    
    # Print data table----------------------------------------------------------
    output$budgetTable <- DT::renderDataTable(
        DT::datatable(data = year_subset(), 
                      options = list(pageLength = 10), 
                      rownames = FALSE)
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
