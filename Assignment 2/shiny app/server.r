library(shiny)
library(dplyr)
library(tidyr)
library(flexdashboard)
library(shinydashboard)
library(plotly)
library(ggplot2)
shinyServer(
  function(input,output){
  
    output$mysex      <- renderText(paste("You selected the sex as: ",input$sex))
    output$mytype     <- renderText(paste("You selected the type of line as: ",input$type))
    output$myage      <- renderText(paste("You selected the age as: ",input$age," years"))
    output$myincome   <- renderText(paste("You selected the Income as: ",input$income," CAD"))
    output$myscore    <- renderText(paste("You selected the credit score as: ",input$score," points"))
 ##   output$myrecen    <- renderText(paste("You selected the number of transactions with the bank as: ",input$recen," per month"))
 ##   output$mytela     <- renderText(paste("You selected the number of call within the month  as: ",input$tela," Calls"))
 ##   output$mytelo     <- renderText(paste("You selected the number of products as: ",input$telo," Products"))
  ##  output$myenrri    <- renderText(paste("You selected the number of selling calls as: ",input$enrri," Calls"))
    output$myzip      <- renderText(paste("You selected the zipcode as: ",input$zip," "))
    # Reactive expression to create data frame of all input values ----
    inputData <- reactive({
      
       
        value = (c(input$sex, input$type, input$age, input$income, input$score, input$zip)) ##input$recen,input$tela,input$telo,input$enrri
      
    })
    
    # Use model on input data
    
    df_5 <- reactive({
      data1 <- (inputData())
      setwd("/Users/stantaov/")
      data2 <- read.csv("clustered.csv") ## Items
     
      if (data1[1]=="Male"){
        tsex1 <- 1
      }
      else{
        tsex1 <- 2
      }
      if (data1[2]=="Mobile"){
        ttype1 <- 0
      }
      else{
        ttype1 <- 1
      }
      
      tincom1 <- as.numeric(data1[4])
      
      if (tincom1 <= (1874*12)){
        tclasse <- 5
      }
      else{
        if (tincom1 > (1874*12) & tincom1<=(3748*12)){
          tclasse <- 4
        }
        else{
        if (tincom1>(3748*12) & tincom1<=(9370*12)){
          tclasse <- 3
        }
        else{
        if (tincom1>(9370*12) & tincom1<=(18740*12)){
          tclasse <- 2
        }
        else{
          tclasse <- 1
          }
         }
        }
      }
      tincom1 <- tclasse
      class(tsex1)
      tage1   <- as.numeric(data1[3])/80
      tscor1  <- as.numeric(data1[5])/5
      ##trece1  <- as.numeric(data1[6])/13
      tzip1   <- (as.numeric(data1[6]))/100000000
     ## ttelo1  <- as.numeric(data1[8])/10
     ## ttela1  <- as.numeric(data1[7])/10
     ## tenrri1 <- as.numeric(data1[9])/8
      trece1 <- .5
      ttelo1 <- .5
      ttela1 <-.5
      tenrri1 <- .4
      
      print(tsex1)
      print(ttype1)
      print(tage1)
      print(tincom1)
      print(tscor1)
     ## print(trece1)
    ##  print(ttela1)
    ##  print(ttelo1)
    ##  print(tenrri1)
    ##  print(tzip1)
      
      menor <- 10000000
      len2 <- nrow(data2)
      ##print(len2)
      contador2 <- 1
      while (contador2 <= len2){ ## nodes
        ##print(contador2)
        tsex2   <- data2[contador2,2]
        tage2   <- data2[contador2,3]
        tscor2  <- data2[contador2,4]
        trece2  <- data2[contador2,5]
        tzip2   <- data2[contador2,6]
        ttype2  <- data2[contador2,7]
        ttelo2  <- data2[contador2,8]
        ttela2  <- data2[contador2,9]
        tenrri2 <- data2[contador2,10]
        tincom2 <- data2[contador2,11]
        
        ## Subtraction section 
        
        
        tsex3     <-  tsex1      - tsex2
        tage3     <-  tage1      - tage2
        tscor3    <-  tscor1     - tscor2
        trece3    <-  trece1     - trece2
        tzip3     <-  tzip1      - tzip2
        ttype3    <-  ttype1     - ttype2
        ttelo3    <-  ttelo1     - ttelo2
        ttela3    <-  ttela1     - ttela2
        tenrri3   <-  tenrri1    - tenrri2
        tincom3   <-  tincom1    - tincom2
        
        quadrado <- ((tsex3**2)+(tage3**2)+(tscor3**2)+(trece3**2)+(tzip3**2)+(ttelo3**2)+(ttela3**2)+(tenrri3**2)+(tincom3**2)+(ttype3**2))
        
        d <- (quadrado**1/2)
        
        if (d<menor & d!=0){ ## selection of the best cluster (small distance)
          registro1 <- data2[contador2,1] 
          registro  <- contador2
          menor <- d
        }
        
        contador2 <- contador2 + 1
      } ## end loop contador 2
      ##value1 = tibble(event = c("% positive sell","% neg sell","% positive reach","% negative reach"), percentage = c(data2[registro,12]),(1-data2[registro,12]),data2[registro,14],(1-data2[registro,14]))
      value1 =  c(data2[registro,12]) ##,(1-data2[registro,12]),data2[registro,14],(1-data2[registro,14])
      print(registro)
      print(value1)

      })
  

       output$test <- renderGauge({
     
      gauge(round((df_5()*100),2), min = 0, max = 100, symbol = '%',  label = paste("Sucess reaching"),
      
            gaugeSectors( success = c(30, 100), warning = c(10, 29), danger = c(0, 9) ))
     
      
      })

  
 
  
})

