#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DT)
library(shiny)
library(gbm)
library(ranger)
library(tidyverse)
library(shinythemes)
load(file="final_gbm.rda")
load(file="final_rf.rda")
load(file="mp_ratio_tbl.rda")
load(file="empty_tbl.rda")
# Define UI for application that draws a histogram
ui <- shinyUI(
    fluidPage(
        theme = shinytheme("cosmo"),
        
        # Application title
        titlePanel(h1("Pilot/full scale BV10 estimation for recalcitrant organic micropollutant (MP) removal by GAC",
                      style="font-size:45px;font-weight: bold;")),
        sidebarLayout(sidebarPanel(
            numericInput("DOC", label="DOC (mg/L) of water matrix",value=0.1,min=0.1,max=11.4) ,
            numericInput("pH", label="pH of water matrix",value=7,min=0) ,
            numericInput("UV254", label="UV254 (abs/cm) of water matrix",value=0.05,min=0,max=0.248) ,
            numericInput("C0", label="Initial MP concentration (ug/L)",value=0.05,min=0) ,
            numericInput("charge", label="Charge of MP at specified pH",value=-1, min=-1) ,
            numericInput("L", label="Abraham param. L",value=2,min=1) ,
            numericInput("B", label="Abraham param. B",value=2,min=0) ,
            numericInput("E", label="Abraham param. E",value=2,min=-1.5) ,
            numericInput("S", label="Abraham param. S",value=2,min=-.9) ,
            numericInput("A", label="Abraham param. A",value=2,min=0) ,
            numericInput("V", label="Abraham param. V",value=2,min=0.6) ,
            numericInput("BET", label="BET (m2/g) pf GAC",value=700,min=573, max=1468) ,
            numericInput("pzc", label="pzc of GAC",value=7,min=5.5,max=10) ,
            numericInput("mp_ratio", label="Micropore to total pore volume ratio of GAC",value=0.5,min=0,max=1) ,
            numericInput("EBCT", label="Empty bed contact time (min)",value=7,min=4.6,max=24),
            br(),
            actionButton("goButton", "Calculate"),
            p("Click to simulate BV10."),
            
            actionButton("goButton2", "Record"),
            p("Click to record simulation to the 'Results' tab." )
        ),
        mainPanel (
            tabsetPanel(
                tabPanel("About",
                         fluidRow(
                             br(),
                             p(" This web application is based on a manuscript currently under revision and provide
                             two machine learning models to estimate early breakthrough of recalcitrant organic MPs in GAC.
                             Users can easily calculated the BV10s under various conditions by entering a few water quality, MP property, 
                             and GAC characteristic parameters. Additionally, this web application also provides a function for users to record 
                             the estimated BV10s along with the corresponding input parameters.
                        
                        ", style="text-indent: 2em;font-size:20px;")
                         )),
                tabPanel("GBM output",
            ##all about output
            tags$style("#txt {font-size:30px;
               color:blue;
               display:block; }"),
            tags$style("#bv10_value {font-size:30px;
               color:blue;
               display:block;
               bottom: 40px; 
               position:absolute;
               width: 100%;
               left:0px;}"),
            
            div(style="text-align:center;
               width:500px;
        height:350px;
        padding-top:70px;
        position:relative;",
                textOutput("txt"),
                textOutput("bv10_value"))
        ),
        tabPanel("RF output", 
                 ##all about output
                 tags$style("#txt2 {font-size:30px;
               color:blue;
               display:block; }"),
                 tags$style("#bv10_value_2 {font-size:30px;
               color:blue;
               display:block;
               bottom: 70px; 
               position:absolute;
               width: 100%;
               left:0px;}"),
                 
                 div(style="text-align:center;
               width:500px;
        height:350px;
        padding-top:70px;
        position:relative;",
                     textOutput("txt2"),
                     textOutput("bv10_value_2"))),
        tabPanel("Results",
                 fluidRow(
                     dataTableOutput("bv10s"),
                     downloadButton("downloadData", "Download"),
                     p("Download simualtions in csv format. Please make sure the file extension is '.csv'."))
                 ),
        tabPanel("Abraham parameter reference", 
                 fluidRow(
                     tags$head(tags$style(HTML("a {color: navy;font-size:26px}"))),
                     h3("Please refer to the link below (UFZ-LSER database v 3.2.1) for Abraham solvation parameters for MPs:"),
                      
                     tags$a("https://www.ufz.de",
                       href="https://www.ufz.de/index.php?en=31698&contentonly=1&m=0&lserd_data[mvc]=Public/start",
                       style = "font-size:20pt"),
                      br(),
                     h3("About the Six Abraham parameters:"),
                      
                     p(" Abraham solvation parameters were developed by Abraham et al (2004) to parameterize 
                       the intermolecular forces between solutes and solvents and are applicable to gas, liquid, and solid phases.",
                        style="text-indent: 2em;font-size:20px;"),
                    
                      
                     p(" The meaning of these six parameters follows.
                        A represents the hydrogen bonding acidity of solute; 
                        B represents the hydrogen bonding basicity; S represents the polirazability of solute;
                        L represents the partitioning coefficient between gas phase and hexadecane;
                        V represents the McGowan volume of solute;
                        E represents the excessive molar refraction of solute.
                        
                        ", style="text-indent: 2em;font-size:20px;")
                     )),
        tabPanel("GAC property references", 
                 fluidRow(
                h3("Please refer to this table if you have not measured your GAC characteristics."),
                 tableOutput("table")))
        )
        )
        )
    )
    
)

# Define server logic 
server <- function(input, output,session) {
    ntext <- eventReactive(input$goButton, {
        newdt<-data.frame(DOC=input$DOC, pH=input$pH, UV254=input$UV254, C0=input$C0, charge=input$charge,
                          L=input$L, B=input$B,E=input$E, S=input$S, A=input$A, V=input$V,
                          BET=input$BET, pzc=input$pzc, mp_ratio=input$mp_ratio, EBCT=input$EBCT, CD=0,PD=0)
        logbv10<-predict(final_gbm ,newdt)
        predicted<-10^logbv10
       
        return(predicted)
    }
    
    )
    ntext2 <- eventReactive(input$goButton, {
        newdt<-data.frame(DOC=input$DOC, pH=input$pH, UV254=input$UV254, C0=input$C0, charge=input$charge,
                          L=input$L, B=input$B,E=input$E, S=input$S, A=input$A, V=input$V,
                          BET=input$BET, pzc=input$pzc, mp_ratio=input$mp_ratio, EBCT=input$EBCT, CD=0,PD=0)
        logbv10_2<-predict(final_rf ,newdt)$predictions
        predicted2<-10^logbv10_2
        return(predicted2)}
    )
    
    #add row upon click
    values <- reactiveValues()
    empty_tbl->result
    values$df <- result
     observeEvent(input$goButton2, {
        newdt<-data.frame(DOC=input$DOC, pH=input$pH, UV254=input$UV254, C0=input$C0, charge=input$charge,
                          L=input$L, B=input$B,E=input$E, S=input$S, A=input$A, V=input$V,
                          BET=input$BET, pzc=input$pzc, mp_ratio=input$mp_ratio, EBCT=input$EBCT, CD=0,PD=0)
        logbv10<-predict(final_gbm ,newdt)
        GBM_bv10<-round(10^logbv10,digits=0)
        logbv10_2<-predict(final_rf ,newdt)$predictions
        RF_bv10<-round(10^logbv10_2,digits=0)
        newRow<-cbind(newdt,GBM_bv10,RF_bv10)
        #colnames(newRow)[c(16,17)]<-c("GBM_BV10","RF_BV10")
        values$df <- rbind(values$df,newRow)
     
    }  )
    
    output$txt<-renderText({
        paste( "Estimated BV10 by gradient boosting machine (GBM) model is " )
    })
    output$bv10_value<-renderText({
        ntext()
    })
    output$txt2<-renderText({
        paste( "Estimated BV10 by random forest (RF) model is " )
    })
    output$bv10_value_2<-renderText({
        ntext2()
    })
    output$table <- renderTable(mp_ratio_tbl)
    
    #thedata <- reactive(iris)
    
    #output$dto <- renderDataTable({thedata()})
    thedata<-reactive(values$df)
    output$bv10s <- renderDataTable({
        thedata()
    })
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('SimulatedBV10s', Sys.Date(), '.csv', sep = '')
        },
        content = function(file) {
            write.csv(thedata(), file, row.names = FALSE)
        }
    )
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
