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
    titlePanel("Operating Budget for PIttsburgh 2020-2022"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           # sliderInput("bins",
            #            "Number of bins:",
             #           min = 1,
              #          max = 50,
               #         value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("deptBarplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$deptBarplot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x <- budget[, c('Department', 'Amount')]
        x <- aggregate(x$Amount, by=list(Category = x$Department), FUN = sum)
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        barplot(x, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
