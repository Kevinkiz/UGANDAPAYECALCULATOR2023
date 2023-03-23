#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("Ug PAYE CLassification"),
    tabPanel("Ug VAT CLassification"),
    tabPanel("Ug Income Tax CLassification")),
  
            
  tags$head(
    tags$style(
      HTML("body { background-color: green; }")
    )
  ),
  
  tags$script(
    "setInterval(function() {
      var colors = ['lightgreen', 'lightred', 'lightblue', 'yellow', 'pink'];
      var color = colors[Math.floor(Math.random() * colors.length)];
      document.body.style.backgroundColor = color;
    }, 5000);"
  ),
    tags$head(
      tags$style(HTML("
      /* CSS styles for the snowflakes */
      .snowflake {
        position: absolute;
        width: 10px;
        height: 10px;
        background-color: white;
        border-radius: 50%;
        transform: rotate(45deg);
        animation: snowflake 10s linear infinite;
      }
      @keyframes snowflake {
        0% {
          transform: translateY(0) rotate(0);
        }
        100% {
          transform: translateY(100%) rotate(360deg);
        }
      }
    "))
    ),
    tags$body(
      tags$script(HTML("
      // JavaScript code to add snowflakes to the page
      function addSnowflake() {
        var snowflake = document.createElement('div');
        snowflake.classList.add('snowflake');
        snowflake.style.left = Math.random() * 100 + '%';
        document.body.appendChild(snowflake);
      }
      setInterval(addSnowflake, 500);
    "))
    ),
    # Your other UI elements go here
  
  tags$style(HTML("body { background-color: lightblue; }")),
  titlePanel(div(
    HTML("<img src='https://flagpedia.net/data/flags/h80/ug.png' style='vertical-align:middle; padding-right:10px;'>"),
    "UGANDA PAYE CALCULATOR"
  )
  ),
  radioButtons("status", "Resident Status", c("Resident", "Non-Resident")),
  numericInput("input", "ENTER NET AMOUNT :", 0),
  textOutput("result"),
  textOutput("result2"),
  textOutput("result3"),
  textOutput("result4"),
  
)

server <- function(input, output) {
 
  result2=reactive({
    if (input$input) {
      format((0.1*input$input),scientific=FALSE,big.mark=",",decimal.mark=".")
    }  
  })
  result3=reactive({
    if (input$input) {
      format((input$input*0.05),scientific=FALSE,big.mark=",",decimal.mark=".")
    }  
  })
  result4=reactive({
    if (input$input) {
      input$input+(input$input*0.1)+(input$input*0.05)
    }  
  })
  result <- reactive({
    if (input$status == "Resident") {
      if (input$input <= 235000) {
        0
      } else if (input$input > 235000 & input$input <= 335000) {
       0.1 * (input$input - 235000)
      } else if (input$input > 335000 & input$input <= 410000) {
      0.2 * (input$input - 335000) + 10000
      } else if (input$input > 410000 & input$input <= 10000000) {
        0.3 * (input$input - 410000) + 25000
      } else if (input$input > 10000000) {
        25000 + 0.3 * (input$input - 410000) + 0.1 * (input$input - 10000000)
      } else {
        NA
      }
    } else if (input$status == "Non-Resident") {
      if (input$input <= 335000) {
        0.1 * input$input
      } else if (input$input > 335000 & input$input <= 410000) {
        0.2 * (input$input - 335000) + 33500
      } else if (input$input > 410000 & input$input <= 10000000) {
        0.3 * (input$input - 410000) + 48500
      } else if (input$input > 10000000) {
        48500 + 0.3 * (input$input - 410000) + 0.1 * (input$input - 10000000)
      } else {
        NA
      }
    }
  })
  
  output$result <- renderText({
    paste("PAYE UGX:",format(result(),scientific=FALSE,big.mark=",",decimal.mark="."))
  })
  output$result2 <- renderText({
    paste("NSSF 10% UGX:",result2())
   
  })
  output$result3 <- renderText({
    paste("NSSF 5% UGX:",result3())
    
  })
  output$result4 <- renderText({
    paste("Gross Pay UGX:",format(result4() + result(),scientific=FALSE,big.mark=",",decimal.mark="."))
    
  })

}

shinyApp(ui, server)


