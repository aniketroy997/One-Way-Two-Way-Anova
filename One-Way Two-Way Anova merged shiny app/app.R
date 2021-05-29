library(shiny)
library(shinythemes)
library(caTools)
library(ROCR)
library(plotly)
ui<-navbarPage(theme = shinytheme("united"),
               tags$head(
                   tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Play&display=swap');
        @import url('https://fonts.googleapis.com/css2?family=Monda&display=swap');
        
      body{
        font-family:Monda;
        font-size:16px;
      }
      .tab-pane{
        margin-right:2%;
        margin-left:2%;
      }
      h1, h2, h3{
        color: #FF5E13;
        text-align:center;
        font-family:Play;
      }
      p{
        color: #FF5E13;
        font-family:Play;
      }
      h2{
        font-family:Play;
        font-size: 50px;
        text-align: left;
        padding-left:24px;
      }
      h4{
      color: #FF5E13;
        font-family:Play;
        font-size: 30px;
        text-align: left;
        padding-top:24px;
      }
      h5{
        color: #FF5E13;
        font-family:Play;
        font-size: 20px;
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
        color: #FF5E13;
        }
      .col-sm-8{
        border-radius: 5px;
        border: 1px solid black;
        border-color:#b0b0b0;
      }
      li{
        color: #FF5E13;
      }
      .well{
        border-radius: 10px;
      }
    "))
               ),
               tabPanel("Anova One Way",
                        sidebarLayout(
                            sidebarPanel(
                                fileInput("file","Upload your CSV",multiple = FALSE),
                                tags$hr(),
                                checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
                                radioButtons(inputId = 'sep', label = 'Separator',
                                             choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                                uiOutput("var1_select"),
                                uiOutput("var2_select"),
                                uiOutput("var3_select")
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Data Set",
                                             uiOutput("tb1")
                                    ),
                                    tabPanel("Normality Test",
                                             tags$h3("var1_select"),
                                             verbatimTextOutput("var1_show"),
                                             plotOutput('MyPlot1'),
                                             tags$h3("var2_select"), 
                                             verbatimTextOutput("var2_show"),
                                             plotOutput('MyPlot2'),
                                             tags$h3("var3_select"), 
                                             verbatimTextOutput("var3_show"),
                                             plotOutput('MyPlot3')
                                             
                                    ),
                                    tabPanel("One Way Anova",
                                             tags$h3("bartlett_test"),
                                             verbatimTextOutput("bartlett_test"),
                                             tags$h3("ANOVA"),
                                             verbatimTextOutput("ANOVA"),
                                             tags$h3("lm_model"),
                                             verbatimTextOutput("lm_model"),
                                             tags$h3("graph output"),
                                             plotOutput('MyPlot4')
                                    )
                                )
                            )
                        )
               ),
               tabPanel("Anova Two Way",
                        sidebarLayout(
                            sidebarPanel(
                                fileInput("file3","Upload your CSV",multiple = FALSE),
                                tags$hr(),
                                checkboxInput(inputId = 'header1', label = 'Header', value = TRUE),
                                radioButtons(inputId = 'sep1', label = 'Separator',
                                             choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                                uiOutput("var11_select"),
                                uiOutput("var33_select"),
                                uiOutput("var22_select")
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Data Set",
                                             uiOutput("tb2")
                                    ),
                                    tabPanel("Normality Test",
                                             tags$h3("var1_select"),
                                             verbatimTextOutput("var11_show"),
                                             plotOutput('MyPlot11')
                                    ),
                                    tabPanel("Two Way Anova",
                                             tags$h3("bartlett_test"),
                                             verbatimTextOutput("bartlett_test2"),
                                             tags$h3("ANOVA"),
                                             verbatimTextOutput("ANOVA2"),
                                             tags$h3("graph output"),
                                             plotOutput('MyPlot44')
                                    )
                                )
                            )
                        )
               ),
               tabPanel("Contact Us",h3("Conatct us by Mail"),h5("Aniket Roy: aniketroy997@gmail.com"),h5("Krutik Shah: krutikyshah@gmail.com"),br(),h3("Conatct us on Linkdin"),h5(tags$a("Aniket Roy",href="https://www.linkedin.com/in/aniket-roy-407214126/")),h5(tags$a("Krutik Shah",href="https://www.linkedin.com/in/krutik-shah-825264190/")))
)
server<-function(input,output) { 
    data <- reactive({
        file1 <- input$file
        if(is.null(file1)){return()}
        tt1=read.table(file=file1$datapath, sep=input$sep, header = input$header)
    })  
    output$tb1 <- renderUI({
        DT::dataTableOutput("table")
    })
    output$table <-  DT::renderDataTable({
        DT::datatable(data(), filter = 'top', options = list( autoWidth = TRUE))
    })
    
    
    
    data1 <- reactive({
        file4 <- input$file3
        if(is.null(file4)){return()}
        tt3=read.table(file=file4$datapath, sep=input$sep1, header = input$header1)
    })
    output$table1 <-  DT::renderDataTable({
        DT::datatable(data1(), filter = 'top', options = list( autoWidth = TRUE))
    })
    output$tb2 <- renderUI({
        DT::dataTableOutput("table1")
    })
    
    data2 <- reactive({
        file6 <- input$file5
        if(is.null(file6)){return()}
        tt1=read.table(file=file6$datapath, sep=input$sep2, header = input$header2)
    })  
    output$tb3 <- renderUI({
        DT::dataTableOutput("table2")
    })
    output$table2 <-  DT::renderDataTable({
        DT::datatable(data2(), filter = 'top', options = list( autoWidth = TRUE))
    })
    
    data3 <- reactive({
        file8 <- input$file7
        if(is.null(file8)){return()}
        tt1=read.table(file=file8$datapath, sep=input$sep3, header = input$header3)
    })  
    output$tb4 <- renderUI({
        DT::dataTableOutput("table3")
    })
    output$table3 <-  DT::renderDataTable({
        DT::datatable(data3(), filter = 'top', options = list( autoWidth = TRUE))
    })
    
    output$var1_select<-renderUI({
        selectInput("var1_select","Select First Varible", choices =as.list(names(data())),multiple = FALSE)
    })
    output$var2_select<-renderUI({
        selectInput("var2_select","Select Second Varible", choices =as.list(names(data())),multiple = FALSE)
    })
    output$var3_select<-renderUI({
        selectInput("var3_select","Select Thrid Varible", choices =as.list(names(data())),multiple = FALSE)
    })
    
    
    output$var11_select<-renderUI({
        selectInput("var11_select","indipendet varible", choices =as.list(names(data1())),multiple = FALSE)
    })
    output$var22_select<-renderUI({
        selectInput("var22_select","Anova dependent varible", choices =as.list(names(data1())),multiple = TRUE)
    })
    output$var33_select<-renderUI({
        selectInput("var33_select","Select Varible", choices =as.list(names(data1())),multiple = FALSE)
    })
    
    
    output$var111_select<-renderUI({
        selectInput("var111_select","indipendet varible", choices =as.list(names(data2())),multiple = FALSE)
    })
    output$var222_select<-renderUI({
        selectInput("var222_select","dipendet varible", choices =as.list(names(data2())),multiple = FALSE)
    })
    
    output$var1111_select<-renderUI({
        selectInput("var1111_select","indipendet varible", choices =as.list(names(data3())),multiple = FALSE)
    })
    output$var2222_select<-renderUI({
        selectInput("var2222_select","dipendet varible", choices =as.list(names(data3())),multiple = TRUE)
    })
    
    output$lmr2<-renderPrint({
        
        f<-data3()
        
        y <- f[,input$var1111_select]
        x <- f[,input$var2222_select]
        logreg <- lm(y~ . ,data = x)
        print(summary(logreg))
    })
    
    output$lmr3 <- renderPlot({
        f<-data3()
        
        pairs(f[,input$var2222_select])
    })
    
    
    output$lmr<-renderPrint({
        
        f<-data2()
        
        y <- f[,input$var111_select]
        x <- f[,input$var222_select]
        logreg <- lm(y~x)
        print(summary(logreg))
        
    })
    
    output$lmr1 <- renderPlotly({
        f<-data2()
        
        ggplot(f, aes(x = f[,input$var111_select], y = f[,input$var222_select])) +
            geom_point() +
            stat_smooth(method = lm)
    })
    
    
    output$var1_show<-renderPrint({
        f<-data()
        y <- f[,input$var1_select]
        res1 <- shapiro.test( f[,input$var1_select])
        print(res1)
    })
    output$var2_show<-renderPrint({
        f<-data()
        x <- f[,input$var2_select]
        res2 <- shapiro.test(f[,input$var2_select] ) 
        print(res2) 
        
    })
    output$var3_show<-renderPrint({
        f<-data()
        z <- f[,input$var3_select]
        res3 <- shapiro.test(f[,input$var3_select] ) 
        print(res3)
    })
    
    output$MyPlot1 <- renderPlot({
        f<-data()
        y <- f[,input$var1_select]
        qqnorm(f[,input$var1_select])
        qqline(f[,input$var1_select],distribution = qnorm)
    })
    
    output$MyPlot2 <- renderPlot({
        f<-data()
        x <- f[,input$var2_select] 
        qqnorm(f[,input$var2_select])
        qqline(f[,input$var2_select] ,distribution = qnorm)
    })
    
    output$MyPlot3 <- renderPlot({
        f<-data()
        z <- f[,input$var3_select]
        qqnorm(f[,input$var3_select])
        qqline(f[,input$var3_select],distribution = qnorm)
    })
    
    
    output$var11_show<-renderPrint({
        f1<-data1()
        z <- f1[,input$var11_select]
        res3 <- shapiro.test(f1[,input$var11_select] ) 
        print(res3)
    })
    
    output$MyPlot11 <- renderPlot({
        f1<-data1()
        z <- f1[,input$var11_select]
        qqnorm(f1[,input$var11_select])
    })
    
    output$bartlett_test2<-renderPrint({
        f <-data1()
        y <- f[,input$var11_select]
        x <- f[,input$var33_select]
        res.var <- bartlett.test(y~x)
        print(res.var)
        
    })
    
    output$ANOVA2<-renderPrint({
        f <-data1()
        y <- f[,input$var11_select]
        x <- f[,input$var22_select]
        if(is.vector(x))
        {
            x <- f[,input$var22_select]
            stck_test <- aov(y~x)
        }
        else
        {
            x <- f[,input$var22_select]
            stck_test <- aov(y~ . ,data = x)
        }
        print(stck_test)
        tk <- TukeyHSD(stck_test,input$var33_select)
        print(tk)
    })
    output$MyPlot44 <- renderPlot({
        f <-data1()
        y <- f[,input$var11_select]
        x <- f[,input$var22_select]
        if(is.vector(x))
        {
            x <- f[,input$var22_select]
            stck_test <- aov(y~x)
        }
        else
        {
            x <- f[,input$var22_select]
            stck_test <- aov(y~ . ,data = x)
        }
        tk <- TukeyHSD(stck_test,input$var33_select)
        plot(tk) 
    })
    
    
    output$bartlett_test<-renderPrint({
        f<-data()
        y <- f[,input$var1_select]
        x <- f[,input$var2_select] 
        z <- f[,input$var3_select]
        
        res.var <- bartlett.test(list(f[,input$var1_select],f[,input$var2_select],f[,input$var3_select]))
        print(res.var)
        
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
