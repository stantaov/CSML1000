library(shiny)
library(dplyr)


rsconnect::setAccountInfo(name='stan-t',
                          token='B842348616839D92F1393315A6DF7794',
                          secret='9nWnzpH15pSqC8o4Ig/ZbvMLyC4zy3srgYdjnOlf')


shinyUI(fluidPage(
  
  titlePanel(title="Credit card fraud detection"),
  sidebarLayout(position ="left",
    sidebarPanel(("Enter the parameters selection"),
                 radioButtons("sex"  ,"Select the gender                                   ",list("Male", "Female"), "Male"),
                 sliderInput("age"   ,"Select the age of the person                        ",min=10,max=100,value=40),
                 sliderInput("income","Select the Income of the person                     ",min=10000,max=100000,value=48000),
                 selectInput("local" ,"Select the location of the seller regards the buyer ",c("Same city and same country", "Different city same country", "Different city and country"),selected="Same city and same country",selectize = FALSE),
                 sliderInput("score" ,"Select the score of the seller                      ",min=1,max=100,value=50),
                 selectInput("type"  ,"Select the type of transaction                      ",c("Recurrent", "In person goods", "In person services", "On-line goods", "On-line services"), "Recurrent"),
                 sliderInput("value" ,"Select the value of the transaction                 ",min=50,max=2000,value=1000),
            
         
                 submitButton("Update")
                 
                 ),
   
     mainPanel(
      textOutput("mysex"),
      textOutput("myage"),
      textOutput("myincome"),
      textOutput("mylocal"),
      textOutput("myscore"),
      textOutput("mytype"),
      textOutput("myvalue"),
      br(),
      tableOutput("testx"),

      br(),
    ##  plotOutput("hist"),
      br(),
    ## tableOutput("view"),
     
    
    )

  )
    
  )
)


