#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages(C("shiny","DT","dplyr","tseries","forecast","ggplot2","gridExtra","readxl","xlsx"), repos = 'http://cran.rstudio.com/',dependencies =TRUE)
library(shiny)
library(DT)
library(dplyr)
library(tseries)
library(forecast)
library(ggplot2)
#library(gridExtra)
library(readxl)# this Wickham's package to read excel file part of tidyverse package
library(xlsx)

#da<-read.csv("Data.csv", header=TRUE,stringsAsFactors = FALSE)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Forecasting Material Qty on basis of Cost data"),
  
  # Sidebar with a slider input for number of bins 
  # note main panel needs to be inside sidebarLayout, else the program will throw error
  sidebarLayout(
    sidebarPanel(
      em( " Note: Since Excel files is a Microsoft specific format; ensure that the data is in the first sheet and 
             Headers are on the first row "),
      
      radioButtons(
        "fileType_Input",
        label = h4("Choose File type"),
        choices = list(".csv/txt" = 1, ".xlsx" = 2),
        selected = 1,
        inline = TRUE
      ),
      
      fileInput('file1',h4('Upload Items List'),
                accept = c(
                  'text/csv',
                  'text/comma-separated-values,text/plain',
                  '.csv',
                  '.xlsx')),
            em("Select Dependent & Independent Variables"),
      br(),
      uiOutput("moreControls"),
      
      br(),
      em("Select Period for Time Series Analysis"),
      dateRangeInput("periodRange","Date Range :",start="2014-05-01",end="2016-12-31",
                     min    = "2001-01-01",
                     max    = "2020-12-21"
                     ),
      
      #selectInput('xcol', 'Independent Variable', names(da),multiple = F,selected = names(da)[2]),
      #selectInput('ycol', 'Dependent Variable', names(da),selected = names(da)[7]),
      
      em("Input Values to Determine the Predicted Values"),
      numericInput('Month1', 'Input_Month1_Cost(KSEK):',100,min=NA,max=NA,width=NULL),
      numericInput('Month2', 'Input_Month2_Cost(KSEK):',100,min=NA,max=NA,width = NULL),
      numericInput('Month3', 'Input_Month3_Cost(KSEK):',100,min=NA,max=NA,width=NULL)
      
      
    ),
      
    #fileInput('file1','Choose InputFile'),
    
    
    # Show a plot of the generated distribution
    mainPanel( tabsetPanel(tabPanel("LinearRegression",
                                    plotOutput("contents"),
                                    br(),br(),
                                    strong("Summary Statistics"),
                                    verbatimTextOutput("results"),
                                    br(),br(),
                                    strong("Model Summary"),
                                    
                                    verbatimTextOutput("model"),
                                    br(),
                                    
                                    strong("DataView"),
                                    
                                    dataTableOutput("tabl"),
                                    br(),
                                    strong("Quarter Prediction"),
                                    tableOutput("pred"),
                                    br(),
                                    verbatimTextOutput("chk")
                                    
    ),
    tabPanel("ARIMA-Time Series",fluidRow(plotOutput("t_series"),
                                        
                                        #plotOutput("out_ARIMAX")
                                        strong("Model Residual Diagnostics"),
                                        br(),
                                        verbatimTextOutput("results1"),
                                        br(),
                                        column(8,strong("Model Accuracy")),
                                        
                                        column(8,tableOutput("Accuracy")),
                                        br(),
                                        
                                        strong("Prediction: Qty"),
                                        tableOutput("pred1")
                                        
    )
    ),
    tabPanel("ARIMAX -with User Inputs",fluidRow(plotOutput("t_series1"),
                                        
                                        #plotOutput("out_ARIMAX")
                                        strong("Model Output"),
                                        br(),
                                        verbatimTextOutput("results_xreg"),
                                        strong("Model Residual Diagnostics"),
                                        verbatimTextOutput("results2"),
                                        br(),
                                        column(8,strong("Model Accuracy")),
                                        
                                        column(8,tableOutput("Accuracy1")),
                                        br(),
                                        
                                        strong("Prediction: Qty"),
                                        tableOutput("pred2")
    )
    ),
    
    tabPanel("Notes",
             h3(strong("Introduction")),
             br(),
             p("This App can be used as a simple Quantitative Forecasting  tool, using LINEAR REGRESSION,TIME SERIES techniques",align="Justify"),
             p("The initial point to get started is that the user should upload the file with Quantities and Cost data. Then the user should choose the column of data frame that contains the data requiring analysis.The DEPENDENT VARIABLE is the Variable being forecasted and the INDEPENDENT VARIABLE is the input variable ",align="Justify"),
             p("For Time Series, Please select the period of data to be analysed; default periods are already displayed by the tool",align="Justify"),
             p("The Tool currently only supports only one independent varable ; Mutiple Inputs variables can also be included if there is a demand for the same and the user to identify the further features that impact the Varibale under consideration.",align="Justify"),
             
             p("Diagnostic & Interpreation:- LINEAR REGRESSION: Look for Adjusted R-squared , if this is high the Model explains that much variability in the data & Low P value in the Coefficients- means that Coefficient is significant in the Model; assuimg that the fundamental conditions of Linear Regression are held.",align="Justify"),
             p("Diagnostic & Interpreation:- TIME SERIES MODEL: Look for Higher P-values in the Box-Ljung Test, basically this means that the model has factored all the information and there is no information in the Residuals ( Actuals-Fitted) to be exploited.",align="Justify"),
             p("As this tool uses mutiple Algorithm it would be prudent to select Model with low RMSE base our forecast",align="Justify"),
             br(),
             h3(strong("Author")),
             p("Author: Abdul Hameed"),
             p("CoE SME")
             
    )
  )
)
)
)




# Define server logic required to draw a histogram
server <- function(input, output,session){
  # observe function can be used to read IO
  mydata<-reactive({
    files<-input$file1
    if(is.null(files)){
       return( NULL)}
    
    if (input$fileType_Input == 1) {
      data1<-read.csv(files$datapath,
               header = TRUE,
               stringsAsFactors = FALSE)
    } else {
        #data1<-read_excel(files$datapath)
      data1<-read.xlsx(files$datapath,
                header = TRUE,sheetIndex = 1,
                stringsAsFactors = FALSE)
    }
    #data1<-read.csv(files$datapath)
    #assign('data',data1,envir=.GlobalEnv)
    return(data1)
    
    })
  output$moreControls<-renderUI({tagList(selectInput('iv', 'Independent Variable', names(mydata()),multiple = F,selected = names(mydata())[7]),
                                selectInput('dv', 'Dependent Variable', names(mydata()),selected = names(mydata())[2])
                               ) })
  
  selectedData<-reactive({mydata()[,c(input$dv,input$iv)]})
  
  
  
  linearReg=reactive({lm(reformulate(input$iv,response=input$dv,intercept=FALSE),data=mydata())})
  
  
  output$contents <- renderPlot({
    
    #linearReg<-lm(input$ycol~ input$xcol-1,contrasts = NULL) 
    
    plot(mydata()[,c(input$iv,input$dv)])
    abline(linearReg())
    title(main="Fitted Line Plot")
  })
 
  #new<-reactive({data.frame(Tot.Cost.Consol=c(input$Month1,input$Month2,input$Month3))})
  # nothing worked, the below code assinged col "name" to the dataframe programatically
  # via medium was choosen by converting it from list to dataframe
  new<-reactive({
  lst<-list(c(input$Month1,input$Month2,input$Month3))
  
  names(lst)<-input$iv
  
  as.data.frame(lst)})
  
# new<-reactive({
#   col.names<-grep("Tot.Cost.Consol",names(mydata()),value=TRUE)
#   df<-data.frame(rbind(input$Month1,input$Month2,input$Month3))
#   colnames(df)<-col.names
# })  
  lm_val<-reactive({predict.lm(linearReg(),new())})
  
 
  
  # this helps in accessing the object inside reactive function
  # ,0 or -1 is included in the equation to do away with the interceptc("#FFFFFF")
  #output$chk<-renderText(new())
  output$model<-renderPrint({summary(linearReg())})
  # to render more pleasing table output use DT package 
  #https://blog.rstudio.org/2015/06/24/dt-an-r-interface-to-the-datatables-library/
  # doesnot work here since it requires df as input 
  #http://rstudio.github.io/DT/shiny.html
  output$tabl<-renderDataTable({mydata()})#,bordered=TRUE)
  output$pred<-renderTable({lm_val()},bordered=TRUE)
  output$Accuracy<-renderTable({accuracy(fcast_AR())})
  ## time series analysis
  Start_Yr<-reactive({as.numeric(format(input$periodRange[1],"%Y"))})
  End_Yr<-reactive({as.numeric(format(input$periodRange[2],"%Y"))})
  Start_Mth<-reactive({as.numeric(format(input$periodRange[1],"%m"))})
  End_Mth<-reactive({as.numeric(format(input$periodRange[2],"%m"))})
  startR=reactive({c(Start_Yr(),Start_Mth())})
  endR=reactive({c(End_Yr(),End_Mth())})
  #analy_ts1<-reactive({ts(mydata()[input$dv],start=c(Start_Yr(),Start_Mth()),end=c(End_Yr(),End_Mth()),deltat=1/12 )})
  analy_ts1<-reactive({ts(mydata()[input$dv],start=startR(),end=endR(),deltat=1/12 )})
  #analy_ts1<-reactive({ts(mydata()[input$dv],start=c(2014,004),end=c(2016,012),deltat=1/12 )})
  fcast_AR<-reactive({

    a<-auto.arima(analy_ts1())#ARIMA(0,0,0)(0,1,0)[12]
    rmea_mat<-Arima(analy_ts1(),order=c(a$arma[1],a$arma[6],a$arma[2]),seasonal = list(order=c(a$arma[3],a$arma[7],a$arma[4]),period=a$arma[5]),include.mean=FALSE,include.drift = FALSE)

  })
  acc_tabl<-reactive({ac<-accuracy(fcast_AR())
  df<-round(subset.matrix(ac,select=c(2,5)),2)
  })

  fcast_val<-reactive({forecast(fcast_AR(),h=3)})

  output$Accuracy<-renderTable({acc_tabl()})
  output$results<-renderPrint({summary(selectedData())})
  output$t_series<-renderPlot({autoplot(fcast_val(),ylab=input$dv,main="Forecasting (ARIMA Univariate)")})# to plot normal time series use analy_ts() in the render function
  
  #output$chk<-renderPrint({startR()})
  output$results1<-renderPrint({Box.test(residuals(fcast_AR()),fitdf=5,lag=10,type="Ljung")}) 
  output$pred1<-renderTable({fcast_val()$mean},bordered = TRUE)
  # 
  #output$t_series<-renderPlot({grid.arrange(autoplot(fcast_AR()),ggtsdisplay(fcast_AR()),ncol=1)
  #})
  
  ## ARIMAX with Xreg argument
  
  analy_ts1_xreg<-reactive({ts(mydata()[input$dv],start=startR(),end=endR(),deltat=1/12 )})
  #analy_ts1<-reactive({ts(mydata()[input$dv],start=c(2014,004),end=c(2016,012),deltat=1/12 )})
  fcast_AR_xreg<-reactive({
    
    a<-auto.arima(analy_ts1_xreg(),xreg=mydata()[input$iv])#ARIMA(0,0,0)(0,1,0)[12]
    rmea_mat<-Arima(analy_ts1(),xreg=mydata()[input$iv],order=c(a$arma[1],a$arma[6],a$arma[2]),seasonal = list(order=c(a$arma[3],a$arma[7],a$arma[4]),period=a$arma[5]),include.mean=FALSE,include.drift = FALSE)
    
  })
  acc_tabl_xreg<-reactive({ac<-accuracy(fcast_AR_xreg())
  df<-round(subset.matrix(ac,select=c(2,5)),2)
  })
  
  fcast_val_xreg<-reactive({forecast(fcast_AR_xreg(),xreg=new())})
  
  output$Accuracy1<-renderTable({acc_tabl_xreg()})
  output$results_xreg<-renderPrint({(fcast_AR_xreg())})
  output$t_series1<-renderPlot({autoplot(fcast_val_xreg(),ylab=input$dv,main="Time Series Forecasting with User Input ")})# to plot normal time series use analy_ts() in the render function
  output$results2<-renderPrint({Box.test(residuals(fcast_AR_xreg()),fitdf=5,lag=10,type="Ljung")}) 
  output$pred2<-renderTable({fcast_val_xreg()$mean},bordered = TRUE)
  
  
}

#shiny::runApp(ui = ui, server = server,display.mode="showcase")


# Run the application 
#shinyApp(ui = ui, server = server)
# using this to access the file in LAN by other users
# just ask them to use my ip address  followed by the port number, for instance IP is.acf(
#   192.168.1.1:5050

shinyApp(ui = ui, server = server,options(host="0.0.0.0",port=5050))

