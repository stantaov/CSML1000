
setwd("/Curso-ML/Assignment-2/")

data1 <- read.csv("assig-2-norm.csv") ## Items

## Making sure all are numeric
data1[1]  <- lapply(data1[1], as.numeric)
data1[2]  <- lapply(data1[2], as.numeric)
data1[3]  <- lapply(data1[3], as.numeric)
data1[4]  <- lapply(data1[4], as.numeric)
data1[5]  <- lapply(data1[5], as.numeric)
data1[6]  <- lapply(data1[6], as.numeric)
data1[7]  <- lapply(data1[7], as.numeric)
data1[8]  <- lapply(data1[8], as.numeric)
data1[9]  <- lapply(data1[9], as.numeric)
data1[10] <- lapply(data1[10], as.numeric)



data3 <- cbind(data1[1],data1[2],data1[3],data1[4],data1[5],data1[6],data1[7],data1[8],data1[9],data1[10])
##print(data3)

valor <- kmeans(data3, 11, iter.max = 10, nstart = 1,algorithm = c("Hartigan-Wong",
                                                                   "Lloyd",
                                                                   "Forgy",
                                                                   "MacQueen"), trace=FALSE)
# S3 method for kmeans

## cluster centers "fitted" to each obs.:
print(valor)
data2 <- head(fitted.valor) ##  nodes

data3 <- fitted(valor, method = c("centers"))
print(data3)
print(data3[,1])
str(data3)
class(data3)
data4 <- fitted(valor, method = c("classes"))
print(data4)
str(data4)
class(data4)
print(data1)

for (i in 1:nrow(data1)){
  data1[i,22]<-data4[i]
  
}


m <- matrix(0:0, nrow = nrow(data3), ncol = 15)
str(m)
print(data3[1,3])
for (i in 1:10){
  for (ix in 1:nrow(data3)){
   
     ##print(data3[ix,i])
     m[ix,i]<-data3[ix,i]
  }
}

print(m) 
data3 <- m

for (i in 1:nrow(data1)){
  for (ix in 1:nrow(data2)){
    
    if (data1[i,22]==data3[ix,1]]){
      
     if (data1[i,14] == 1) {
      data3[ix,11] <- data3[ix,11]+1 ## positive for spoken
     }
     else{
      data3[ix,12] <- data3[ix,12]+1 ## negative for spoken
     }
     if (data1[i,15] == 1) {
      data3[ix,13] <- data3[ix,13]+1 ## positive for sold
     }
     else{
       data3[ix,14] <- data3[ix,14]+1 ## negative for sold
     }
    
     data3[ix,15] <- data3[ix,15]+1
    }
    
  }  
