library(shiny)
library(dplyr)
setwd("/Users/stantaov/")
shinyUI(fluidPage(
  
  titlePanel(title="Client profiling vs. chance to reach"),
  sidebarLayout(position ="left",
    sidebarPanel(("Enter the parameters selection"),
                 radioButtons("sex","Select the gender ", list("Male", "Female"), "Male"),
                 radioButtons("type","Select the type of line ", list("Landline", "Mobile"), "Mobile"),
                 sliderInput("age", "Select the age of the person",min=10,max=100,value=40),
                 sliderInput("income", "Select the income of the person",min=24000,max=200000,value=100000),
                 sliderInput("score", "Select the credit score of the person",min=1,max=10,value=5),
                 ##sliderInput("recen", "Number of transactions with the bank wihtin month ",min=1,max=10,value=5),
                 ##sliderInput("tela", "Number of prodcuts with the bank ",min=1,max=10,value=5),
                 ##sliderInput("telo", "Number of calls within the month ",min=1,max=10,value=5),
                 ##sliderInput("enrri", "How many tries to sell within the month ",min=1,max=10,value=5),
                 selectInput("zip","Select the zipcoder ",c("93700000", "42700000", "55900000","68600000","52020060","85884000","22411002","","20921030","79150000","11277619","30380430"),selected="30380430",selectize = FALSE),
                 
               
                 submitButton("Update")
                 
                 ),
   
     mainPanel(
      textOutput("mysex"),
      textOutput("mytype"),
      textOutput("myage"),
      textOutput("myincome"),
      textOutput("myscore"),
      ##textOutput("myrecen"),
      ##textOutput("mytela"),
      ##textOutput("mytelo"),
      ##textOutput("myenrri"),
      textOutput("myzip"),
      br(),
      gaugeOutput("test"),

      br(),
    ##  plotOutput("hist"),
      br(),
    ## tableOutput("view"),
     
    
    )

  )
    
  )
)


