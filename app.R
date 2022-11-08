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

ob2020 <- read.csv(file = "2020-operating.csv")
ob2021 <- read.csv(file = "2021-amended-operating.csv")
ob2022 <- read.csv(file = "2022-amended-operating.csv")
budget <- rbind(ob2020, ob2021, ob2022)

data <- budget %>% 
  filter(Type == "Expenditure") %>%
  select(Year, Department, Amount) %>%
  group_by(Year, Department) %>%
  summarize(Total = sum(Amount)) %>%
  arrange(Total)

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
                      choices = c("2020" = 2020, 
                                  "2021" = 2021, 
                                  "2022" = 2022), 
                      selected = "2020"),
        
        # Select a department to show ----------------------------------
        checkboxGroupInput(inputId = "selected_dept",
                           label = "Select department(s):",
                           choices = unique(data$Department),
                           selected = c("ETHICS BOARD","OFFICE OF EQUITY","PS - FIRE BUREAU","PS - POLICE BUREAU"))
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("deptBarplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  year_subset <- reactive({
    req(input$year, input$selected_dept)
    filter(data, Year == input$year & Department %in% input$selected_dept)
  })

    output$deptBarplot <- renderPlot({
        # generate barplot
      ggplot(year_subset(), aes(Department, Total)) +
        geom_col() +
        theme(axis.text.x = element_text(angle = 90, size = 10)) +
        scale_y_continuous(labels = comma) + 
        ylab("Total Budget ($)")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
