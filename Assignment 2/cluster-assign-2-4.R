setwd("/Curso-ML/Assignment-2/")

data1 <- read.csv("test.csv")      ## Items
data2 <- read.csv("clustered.csv") ## Items
## Making sure all are numeric
data1[1]  <- lapply(data1[1],  as.numeric)
data1[2]  <- lapply(data1[2],  as.numeric)
data1[3]  <- lapply(data1[3],  as.numeric)
data1[4]  <- lapply(data1[4],  as.numeric)
data1[5]  <- lapply(data1[5],  as.numeric)
data1[6]  <- lapply(data1[6],  as.numeric)
data1[7]  <- lapply(data1[7],  as.numeric)
data1[8]  <- lapply(data1[8],  as.numeric)
data1[9]  <- lapply(data1[9],  as.numeric)
data1[10] <- lapply(data1[10], as.numeric)
##data2 <- read.csv("pontos.csv") ## Potential nodes

## Making sure all are numeric

data2[1]  <- lapply(data2[1],  as.numeric)
data2[2]  <- lapply(data2[2],  as.numeric)
data2[3]  <- lapply(data2[3],  as.numeric)
data2[4]  <- lapply(data2[4],  as.numeric)
data2[5]  <- lapply(data2[5],  as.numeric)
data2[6]  <- lapply(data2[6],  as.numeric)
data2[7]  <- lapply(data2[7],  as.numeric)
data2[8]  <- lapply(data2[8],  as.numeric)
data2[9]  <- lapply(data2[9],  as.numeric)
data2[10] <- lapply(data2[10], as.numeric)

limitador <- 1 ## Initial value of the loop

numbernodes <- 9


print(nrow(data1))
print(nrow(data2))
print(step)
print(limit)


 
  len1 <- nrow(data1)
  print(len1)
  contador1 <- 1
  page <- 100
  contador0 <- 0 ## paginacao
  
  while (contador1<=len1){ ## Loop items test set
    
    ##print(contador1) 
    
    
    
    
    if (contador0 == page){
      page = page+100
      print(limitador)
      print(contador1)
      print(Sys.time())
    }
    contador0=contador0+1
    
    tsex1   <- data1[contador1,1]
    tage1   <- data1[contador1,2]
    tscor1  <- data1[contador1,3]
    trece1  <- data1[contador1,4]
    tzip1   <- data1[contador1,5]
    ttype1  <- data1[contador1,6]
    ttelo1  <- data1[contador1,7]
    ttela1  <- data1[contador1,8]
    tenrri1 <- data1[contador1,9]
    tincom1 <- data1[contador1,10]
    
    
    
    
    menor <- 10000000
    len2 <- nrow(data2)
    ##print(len2)
    contador2 <- 1
    while (contador2 <= len2){ ## nodes
      ##print(contador2)
      tsex2   <- data2[contador2,1]
      tage2   <- data2[contador2,2]
      tscor2  <- data2[contador2,3]
      trece2  <- data2[contador2,4]
      tzip2   <- data2[contador2,5]
      ttype2  <- data2[contador2,6]
      ttelo2  <- data2[contador2,7]
      ttela2  <- data2[contador2,8]
      tenrri2 <- data2[contador2,9]
      tincom2 <- data2[contador2,10]
      
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
        registro1 <- data2[contador2,23] 
        registro  <- contador2
        menor <- d
      }
      
      contador2 <- contador2 + 1
    } ## end loop contador 2
    
    data2[registro,22]  <- data2[registro,22]+1
    data1[contador1,23] <- registro1
    
    contador1 <- contador1+1
    ##print(contador1)
    
  } ## end loop items contador1
  
  xxa    <- toString(limitador)
  if (limitador<10){
    var = paste("/Curso-ML/Assignment-2/cluster-000",xxa,".csv")
  }
  else{
    if (limitador<100){
      var = paste("/Curso-ML/Assignment-2/cluster-00",xxa,".csv")
    }
    else{
      if (limitador<1000){
        var = paste("/Curso-ML/Assignment-2/cluster-0",xxa,".csv")
      }
      else{
        var = paste("/Curso-ML/Assignment-2/cluster-",xxa,".csv")
      }
    }
  }
  var  <- str_replace_all(string=var, pattern=" ", replace="")
  ### drop rows for cluster<limitador and != 0
  data2<-subset(data2, data2[,22]>limitador)
  
  write.csv(data2,var) 
  limitador <- limitador+step
  print(limitador)

write.csv(data1,"/Curso-ML/Assignment-2/clustered.csv")