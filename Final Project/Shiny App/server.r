library(shiny)
library(MLmetrics)
library(VIM)
library(ggplot2)
library(dplyr)
library(readr)
library(caTools)
library(caret)
library(ROSE)
library(DMwR)
library(data.tree)
library(party)
library(randomForest)
library(pROC)
library(e1071) 
library(xgboost) 
# loading the datasets
data2x0 <- read.csv("clustered.csv") ## Items
data2x  <- select(data2x0,X,V1,V2,V3,V4,V5,V7,V8,V9,V10,V11)
## Loading the models
tb_model1 <- readRDS("model1.rda")
tb_model2 <- readRDS("model2.rda")
tb_model3 <- readRDS("model3.rda")
tb_model4 <- readRDS("model4.rda")
tb_model5 <- readRDS("model5.rda")
##summary(train)


##str(train)
##summary(train)
str(train)

##str(test)
shinyServer(####1
  function(input,output){####2
  
    output$mysex      <- renderText(paste("You selected the sex as: ",input$sex))
    output$myage      <- renderText(paste("You selected the age as: ",input$age))
    output$myincome   <- renderText(paste("You selected the Income as: ",input$income," CAD"))
    output$mylocal    <- renderText(paste("You selected the Location of the buyer and the seller as: ",input$local))
    output$myscore    <- renderText(paste("You selected the credit score as: ",input$score," points"))
    output$mytype     <- renderText(paste("You selected the type of the transaction as: ",input$type))
    output$myvalue    <- renderText(paste("You selected the value of the transaction as: ",input$value," CAD"))
 
    # Reactive expression to create data frame of all input values ----
    inputData <- reactive({####3
      
       
        value = (c(input$sex,input$age,input$income,input$local,input$score,input$type, input$value )) 
    })####3
    
    # Use model on input data
    
    df_5 <- reactive({####4
      
      data1 <- (inputData())
      print(data1)

      if (data1[1]=="Male"){##1
        tsexF <- 0
        tsexM <- 1
        ttsex <- 0
      }##1
      else{##2
        tsexF <- 1
        tsexM <- 0
        ttsex <- 1
      }##2
     
      
      tincom1 <- as.numeric(data1[3])
      
      if (tincom1 <= (1874*12)){###1
        tclasse0.2 <- 0
        tclasse0.4 <- 0
        tclasse0.6 <- 0
        tclasse0.8 <- 0
        tclasse1   <- 1
        ttclasse   <- 1
      }###1
      else{###2
        if (tincom1 > (1874*12) & tincom1<=(3748*12)){###3
          tclasse0.2 <- 0
          tclasse0.4 <- 0
          tclasse0.6 <- 0
          tclasse0.8 <- 1
          tclasse1   <- 0
          ttclasse   <- 0.8
        }###3
        else{###4
        if (tincom1>(3748*12) & tincom1<=(9370*12)){###5
          tclasse0.2 <- 0
          tclasse0.4 <- 0
          tclasse0.6 <- 1
          tclasse0.8 <- 0
          tclasse1   <- 0
          ttclasse   <- 0.6
        }###5
        else{###6
        if (tincom1>(9370*12) & tincom1<=(18740*12)){###7
          tclasse0.2 <- 0
          tclasse0.4 <- 1
          tclasse0.6 <- 0
          tclasse0.8 <- 0
          tclasse1   <- 0
          ttclasse   <- 0.4
        }###7
        else{###8
          tclasse0.2 <- 1
          tclasse0.4 <- 0
          tclasse0.6 <- 0
          tclasse0.8 <- 0
          tclasse1   <- 0
          ttclasse   <- 0.2
          }###8
         }###6
        }###4
      }###2
      
      
      if (data1[4]=="Same city and same country"){####1
        tsamecity  <- 0
        tsamecount <- 0
      }####1
      else{####2
        if (data1[4]=="Different city same country"){####3
          tsamecity  <- 1
          tsamecount <- 0
        }####3
        else{####4
          tsamecity  <- 1
          tsamecount <- 1 
          
        }####4
      }####2
      
      tscore <- as.numeric(data1[5])/100
      
      ttype  <- data1[6]
      tvalue <- as.numeric(data1[7])
      
      if (ttype == "Recurrent"){
        ttype.0  <- 1
        ttype.25 <- 0
        ttype.5  <- 0
        ttype.75 <- 0
        ttype1   <- 0
        dispersion1 <- (abs(tvalue-79.13)/2000)
        tttype <- 0
      }
      else{
        if (ttype == "In person goods"){
          ttype.0  <- 0
          ttype.25 <- 1
          ttype.5  <- 0
          ttype.75 <- 0
          ttype1   <- 0
          dispersion1 <- (abs(tvalue-80.14)/2000)
          tttype <- 0.25
        }
        else{
          if (ttype == "In person services"){
            ttype.0  <- 0
            ttype.25 <- 0
            ttype.5  <- 1
            ttype.75 <- 0
            ttype1   <- 0
            dispersion1 <- (abs(tvalue-50.03)/2000)
            tttype <- 0.5
          }
          else{
            if (ttype == "On-line goods"){
              ttype.0  <- 0
              ttype.25 <- 0
              ttype.5  <- 0
              ttype.75 <- 1
              ttype1   <- 0
              dispersion1 <- (abs(tvalue-84.95)/2000)
              tttype <- 0.75
            }
            else{
              ttype.0  <- 0
              ttype.25 <- 0
              ttype.5  <- 0
              ttype.75 <- 0
              ttype1   <- 1
              dispersion1 <- (abs(tvalue-106.64)/2000)
              tttype <- 1
            }
          }
        }
      }
      #################################################Calculation of the sysntetic variables V10 and V11###############

        tincome1x  <- ttclasse
        tcity1x    <- tsamecity
        tcount1x   <- tsamecount
        tseller1x  <- tscore
        tdisper1x  <- dispersion1
        ttype1x    <- tttype
        
        menor <- 10000000
        len2 <- nrow(data2x)
        ##print(len2)
        contador2 <- 1
        ##print("Inicio ciclo clusters")
        while (contador2 <= len2){ ## nodes
          ##print(contador2)
          
          tincome2  <- data2x[contador2,2]
          tcity2    <- data2x[contador2,3]
          tcount2   <- data2x[contador2,4]
          tseller2  <- data2x[contador2,5]
          tdisper2  <- data2x[contador2,6]
          ttype2    <- data2x[contador2,7]
          
          ## Subtraction section 
          
          
          
          tincome3    <-  tincome1x     - tincome2
          tcity3      <-  tcity1x       - tcity2
          tcount3     <-  tcount1x      - tcount2
          tseller3    <-  tseller1x     - tseller2
          tdisper3    <-  tdisper1x     - tdisper2
          ttype3      <-  ttype1x       - ttype2
          

          quadrado <- ((tincome3**2)+(tcity3**2)+(tcount3**2)+(tseller3**2)+(tdisper3**2)+(ttype3**2))
          
          d <- (quadrado**1/2)
          
          if (d<menor & d!=0){ ## selection of the best cluster (small distance)
            ##print(d)
            registro1 <- data2x[contador2,1] 
            registro  <- contador2
            menor <- d
          }
          
          contador2 <- contador2 + 1
        } ## end loop contador 2
        
        TV10 <- data2x[registro,8]## positivo
        TV11 <- data2x[registro,9]## negativo
        print(TV10)
        print(TV11)
   
      #################################################Calculation of the synthetic variable point######################
      mean4  <- 0.2306
      mean5  <- 0.7927
      mean6  <- 0.8965
      mean7  <- 0.4906
      mean8  <- 0.0310
      mean9  <- 0.5084
      mean10 <- 0.0404
      mean11 <- 0.950
     
      pontos <- 0
      
      if (ttclasse<= mean4){
        pontos=pontos+1
      }
      if (tsamecity<= mean5){
        pontos<-pontos+1
      }
      if (tsamecount<= mean6){
        pontos<-pontos+1
      }
      if (tscore<= mean7){
        pontos<-pontos+1
      }
      if (dispersion1>= mean8){
        pontos<-pontos+1
      }
      if (tttype>= mean9){
        pontos<-pontos+1
      }
      if (TV10>= mean10){
        pontos<-pontos+1
      }
      if (TV11<= mean11){
        pontos<-pontos+1
      }
      pontos <- pontos+2
      tsex.. <- 0
      tfraud <- 0 
      df <- data.frame(tsex..,tsexF,tsexM,tclasse0.2,tclasse0.4,tclasse0.6,tclasse0.8,tclasse1,tsamecity,tsamecount,tscore,dispersion1,ttype.0,ttype.25,ttype.5,ttype.75,ttype1,TV10,TV11,pontos,tfraud)
      colnames(df)<- c("sex..","sex.F","sex.M","income1.0.2","income1.0.4","income1.0.6","income1.0.8","income1.1","same_city","same_count","seller_sco","dispersao","type.0","type.0.25","type.0.5","type.0.75","type.1","V10","V11","V12","fraud")
      test <- df
      
     
      ##print(test)
      ################ Decision Tree #####################################
    
      predicao1.1 <- predict(tb_model1, test)
      predicao1.1 <- unlist(predicao1.1)
      str(predicao1.1)
      predicao2.1 <- as.numeric(predicao1.1)
      str(predicao2.1)
      if (predicao2.1==0){
        predicao3.1 = "FRAUD"
      }
      else{
        predicao3.1 = "NOT A FRAUD"
      }
      ##confusionMatrix(predicao, test$fraud)
      test1 <- paste("Result decision tree ")
      ###############################Random Forest ########################
      predicao1.2 <- predict(tb_model2, test)
      str(predicao1.2)
      predicao2.2 <- as.numeric(predicao1.2)-1
      str(predicao2.2)
      if (predicao2.2==0){
        predicao3.2 = "FRAUD"
      }
      else{
        predicao3.2 = "NOT A FRAUD"
      }
      test2 <- paste("Result Random Forest ")
      ############### Random Forest + Bagging #############################
      predicao1.3 <- predict(tb_model3, test)
      str(predicao1.3)
      predicao2.3 <- as.numeric(predicao1.3)-1
      str(predicao2.3)
      if (predicao2.3==0){
        predicao3.3 = "FRAUD"
      }
      else{
        predicao3.3 = "NOT A FRAUD"
      }
      test3 <- paste("Result Random Forest + Bagging ")
      ############## Support Vector Machine Classifier ###############
      predicao1.4 <- predict(tb_model4, test)
      str(predicao1.4)
      predicao2.4 <- as.numeric(predicao1.4)-1
      str(predicao2.4)
      if (predicao2.4==0){
        predicao3.4 = "FRAUD"
      }
      else{
        predicao3.4 = "NOT A FRAUD"
      }
      test4 <- paste("Support Vector Machine Classifier ")
      ######################## XGBoost ################################
      data_test  <- subset(test, select = -c(fraud))
      dtest <- xgb.DMatrix(data = as.matrix(data_test), label= (test$fraud))
      predicao1.5 <- predict(tb_model5, dtest)
      str(predicao1.5)
      predicao2.5 <- as.numeric(predicao1.5)-1
      str(predicao2.5)
      if (predicao2.5==0){
        predicao3.5 = "FRAUD"
      }
      else{
        predicao3.5 = "NOT A FRAUD"
      }
      test5 <- paste("XGBoost Classifier ")
      Method <- c(test1,test2,test3,test4,test5)
      Result <- c(predicao3.1,predicao3.2,predicao3.3,predicao3.4,predicao3.5)
      resultadofinal3 <- data.frame(Method,Result)
      
      })####4
    
       output$testx = renderTable({df_5()})


    
      
      }####2
  )####1

  
 
  


