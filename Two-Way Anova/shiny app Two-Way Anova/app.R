library(shiny)
library(plotly)
library(ggplot2)
library(shinythemes)
ui<-navbarPage(theme = shinytheme("united"),"Two-Way Anova",
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
                                fileInput("file3","Upload your CSV",multiple = FALSE),
                                tags$hr(),
                                checkboxInput(inputId = 'header1', label = 'Header', value = TRUE),
                                radioButtons(inputId = 'sep1', label = 'Separator',
                                             choices = c(Comma=','), selected = ','),
                                uiOutput("var11_select"),
                                uiOutput("var22_select"),
                                uiOutput("var33_select")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Data",
                                         h3("Data from CSV File :"),
                                         uiOutput("tb2"),
                                ),
                                tabPanel("Testing Homogenity& Variance",
                                         br(),
                                         verbatimTextOutput("bartlett_test2")
                                ),
                                tabPanel("Anova",
                                         br(),
                                         verbatimTextOutput("ANOVA2")
                                ),
                                tabPanel("graph output",
                                         br(),
                                         plotOutput("MyPlot44")

                            )
                        )
               ))
)
                      
server<-function(input,output) { 
    data <- reactive({
        file1 <- input$file
        if(is.null(file1)){return()}
        tt1=read.table(file=file1$datapath, sep=input$sep, header = input$header)
    })  
    output$table <- renderTable({
        data()
    })
    output$tb1 <- renderUI({
        tableOutput("table")
    })
    
    data1 <- reactive({
        file4 <- input$file3
        if(is.null(file4)){return()}
        tt3=read.table(file=file4$datapath, sep=input$sep1, header = input$header1)
    })
    
    output$table1 <- renderTable({
        data1()
    })
    output$tb2 <- renderUI({
        tableOutput("table1")
    })
    
    output$var11_select<-renderUI({
        selectInput("var11_select","1st Var", choices =as.list(names(data1())),multiple = FALSE)
    })
    output$var22_select<-renderUI({
        selectInput("var22_select","2nd Var", choices =as.list(names(data1())),multiple = FALSE)
    })
    output$var33_select<-renderUI({
        selectInput("var33_select","3rd Var", choices =as.list(names(data1())),multiple = FALSE)
    })
    
    
    output$bartlett_test2<-renderPrint({
        moth<-data1()
        dataset <- data.frame("Moth"=moth[,input$var11_select] ,"position"=moth[,input$var22_select] ,"lure"=moth[,input$var33_select])
        res.var <- bartlett.test(Moth~position,data=dataset)
        print(res.var)
        res.var <- bartlett.test(Moth~lure,data=dataset)
        print(res.var)
        
    })
    
    output$ANOVA2<-renderPrint({
        moth<-data1()
        dataset <- data.frame("Moth"=moth[,input$var11_select] ,"position"=moth[,input$var22_select] ,"lure"=moth[,input$var33_select] )
        stck_test <- aov(Moth ~ position+lure,data = dataset)
        print(stck_test)
        tk <- TukeyHSD(stck_test,as.character(input$var22_select))
        print(tk)
    })
    output$MyPlot44 <- renderPlot({
        moth<-data1()
        dataset <- data.frame("Moth"=moth[,input$var11_select] ,"position"=moth[,input$var22_select] ,"lure"=moth[,input$var33_select] )
        stck_test <- aov(Moth ~ position+lure,data = dataset)
        tk <- TukeyHSD(stck_test,'position')
        plot(tk) 
    })
    
    output$otherone_val_show<-renderPrint({
        f<-data()
        str(f)
    })
    
    
    output$bartlett_test<-renderPrint({
        
        f<-data()
        
        y <- f[,input$var1]
        x <- f[,input$var2] 
        z <- f[,input$var3]
        
        res <- bartlett.test(list(f[,input$var1],f[,input$var2],f[,input$var3]))
        print(res)
        
    })
    
    output$ANOVA<-renderPrint({
      
      f<-data()
      
      stck <- stack(f)
      
      stck_aov<-aov(values ~ ind ,data=stck)
      tk <- TukeyHSD(stck_aov)
      print(tk)
    })
    
    
    output$lm_model<-renderPrint({
      
      f<-data()
      
      stck <- stack(f)
      
      model1 <- lm(values ~ ind,data=stck)
      print(anova(model1))
      
      tk1 <- TukeyHSD(aov(model1))
      print(tk1)
    })
    output$MyPlot4 <- renderPlot({
      f<-data()
      stck <- stack(f)
      
      model1 <- lm(values ~ ind,data=stck)
      print(anova(model1))
      
      tk1 <- TukeyHSD(aov(model1))
      plot(tk1)    
    })
    
    
}
shinyApp(ui=ui,server=server)