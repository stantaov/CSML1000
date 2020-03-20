setwd("/Curso-ML/Assignment-3/")

data1x0 <- read.csv("test.csv")      ## Items
print(data1x0)
data1x <- select(data1x0,fraud,sex,age,income1,same_city,same_count,seller_sco,dispersao,type)
print(data1x)
data2x0 <- read.csv("clustered.csv") ## Items
data2x <- select(data2x0,X,V1,V2,V3,V4,V5,V7,V8,V9,V10,V11)
print(data2x)
## Making sure all are numeric


data1x[4]  <- lapply(data1x[4],  as.numeric)
data1x[5]  <- lapply(data1x[5],  as.numeric)
data1x[6]  <- lapply(data1x[6],  as.numeric)
data1x[7]  <- lapply(data1x[7],  as.numeric)
data1x[8]  <- lapply(data1x[8],  as.numeric)
data1x[9]  <- lapply(data1x[9],  as.numeric)



## Making sure all are numeric
print(data1x)

data2x[1]   <- lapply(data2x[1],   as.numeric)
data2x[2]   <- lapply(data2x[2],   as.numeric)
data2x[3]   <- lapply(data2x[3],   as.numeric)
data2x[4]   <- lapply(data2x[4],   as.numeric)
data2x[5]   <- lapply(data2x[5],   as.numeric)
data2x[6]   <- lapply(data2x[6],   as.numeric)
data2x[7]   <- lapply(data2x[7],   as.numeric)
data2x[8]   <- lapply(data2x[8],   as.numeric)
data2x[9]   <- lapply(data2x[9],   as.numeric)
data2x[10]  <- lapply(data2x[10],   as.numeric)
data2x[11]  <- lapply(data2x[11],   as.numeric)
data2x[,12] <- 0.00

print(data2x)
print(nrow(data1x))
print(nrow(data2x))

print(data2x[2])

 
  len1 <- nrow(data1x)
  print(len1)
  contador1 <- 1
  page <- 100
  contador0 <- 0 ## paginacao
  
  while (contador1<=len1){ ## Loop items test set
    
    ##print(contador1) 
    
    
    
    
    if (contador0 == page){
      page = page+100
      print(contador1)
      print(Sys.time())
    }
    contador0=contador0+1
    
 
    tincome1  <- data1x[contador1,4]
    tcity1    <- data1x[contador1,5]
    tcount1   <- data1x[contador1,6]
    tseller1  <- data1x[contador1,7]
    tdisper1  <- data1x[contador1,8]
    ttype1    <- data1x[contador1,9]

    
    
    
    
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
      
      

      tincome3    <-  tincome1     - tincome2
      tcity3      <-  tcity1       - tcity2
      tcount3     <-  tcount1      - tcount2
      tseller3    <-  tseller1     - tseller2
      tdisper3    <-  tdisper1     - tdisper2
      ttype3      <-  ttype1       - ttype2

      ## print(tsex3)
      ##print(tage3)
      ##print(tincome3)
      ##print(tcity3)
      ##print(tcount3)
      ##print(tseller3)
      ##print(tdisper3)
      ##print(ttype3)
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
    
    data2x[registro,12]  <- data2x[registro,12]+1

    data1x[contador1,10] <- data2x[registro,8]## positivo
    data1x[contador1,11] <- data2x[registro,9]## negativo
    data1x[contador1,12] <- registro
    contador1 <- contador1+1
    ##print(contador1)
    
  } ## end loop items contador1
  
 
write.csv(data1x,"/Curso-ML/Assignment-3/classified.csv")

