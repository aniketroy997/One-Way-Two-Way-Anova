library(shiny)
library(shinyWidgets)
library(shinythemes)
ui <- navbarPage(theme = shinytheme("united"),"One Way Anova",
                 tags$head(
                     tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Play&display=swap');
        @import url('https://fonts.googleapis.com/css2?family=Monda&display=swap');
        
      body{
        padding-bottom:60px;
        font-family:Monda;
        font-size:16px;
      }
      .tab-pane{
        margin-right:2%;
        margin-left:2%;
      }
      h1, h2, h3{
        color: orange;
        text-align:center;
        font-family:Play;
      }
      p{
        color: orange;
        font-family:Play;
      }
      h2{
        font-family:Play;
        font-size: 50px;
        text-align: left;
        padding-bottom:30px;
        padding-left:24px;
      }
      h4{
      
        font-family:Play;
        font-size: 30px;
        text-align: left;
        padding-top:24px;
      }
      
      h2::first-letter {
        font-size: 70px;
      }
      h3{
        font-size: 35px;
      }
      table, th, td {
        color: orange;
        }
      .col-sm-8{
        border-radius: 5px;
        border: 1px solid black;
        border-color:#b0b0b0;
        padding: 6px;
      }
      li{
        color: orange;
      }
      .well{
        border-radius: 10px;
      }
    "))
                 ),
                 sidebarLayout(
                     sidebarPanel(
                         fileInput(
                             "myfile",
                             "Choose CSV File",
                             multiple = F,
                             accept = ".csv"
                         ),
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Data",
                                      h3("Data from CSV File :"),
                                      dataTableOutput("content")
                             ),
                             tabPanel("Q-Q PLOT",
                                      br(),
                                      plotOutput("plot_cba"),
                                      br(),
                                      plotOutput("plot_cs"),
                                      br(),
                                      plotOutput("plot_bda"),
                                      br()
                             ),
                             tabPanel("Testing Homogenity& Variance",
                                      br(),
                                      verbatimTextOutput("bartlett")
                             ),
                             tabPanel("Anova",
                                      br(),
                                      verbatimTextOutput("anova")
                             ),
                             tabPanel("TukeyHSD",
                                      br(),
                                      verbatimTextOutput("tukey")
                             )
                         )
                     )
                 )
)

server <- function(input, output) {
    
    var1 <- reactive(read.csv(input$myfile$datapath))
    data1 <- reactive(as.data.frame(var1()))
    stack1 <- reactive(as.data.frame(stack(data1())))
    
    output$content <- renderDataTable({
        var1()
    })
    output$plot_cba <- renderPlot({
        qqnorm(data1()$CBA)
        qqline(data1()$CBA,distribution = qnorm)
    })
    output$plot_cs <- renderPlot({
        qqnorm(data1()$CS)
        qqline(data1()$CS,distribution = qnorm)
    })
    output$plot_bda <- renderPlot({
        qqnorm(data1()$BDA)
        qqline(data1()$BDA,distribution = qnorm)
    })
    output$bartlett <- renderPrint({
        bartlett.test(values~ind,data=stack1())
    })
    output$anova <- renderPrint({
        d1 <- lm(values~ind,data=stack1())
        anova(d1)
    })
    output$tukey <- renderPrint({
        d2 <- aov(values~ind,data=stack1())
        TukeyHSD(d2)
    })
    
}
shinyApp(ui = ui, server = server)