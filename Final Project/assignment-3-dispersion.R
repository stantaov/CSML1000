##### Creates variable dispersion

setwd("/Curso-ML/Assignment-3/")

data2 <- read.csv("transactions.csv") ## Items
summary(data2)
dim(data2)
print(data2)
### Create variable dispersion which is a measure of how far form the average of the type of the item de value is
data2$dispersao <- 0.00
str(data2)
tail(data2)
len2 = nrow(data2)
contador1 <-1
print(len2)
print(contador1)
while (contador1<=len2){
  if (data2$type[contador1] == 0){
    data2$dispersao[contador1] <- abs(data2$avr_recur[contador1]-data2$value[contador1])/data2$avr_recur[contador1]
  }
  if (data2$type[contador1] == 1){
    data2$dispersao[contador1] <- abs(data2$avr_servic[contador1]-data2$value[contador1])/data2$avr_servic[contador1]
  }
  if (data2$type[contador1] == 2){
    data2$dispersao[contador1] <- abs(data2$avr_go-data2$value)/data2$avr_go[contador1]
  }
  if (data2$type[contador1] == 3){
    data2$dispersao[contador1] <- abs(data2$avr_int_go[contador1]-data2$value[contador1])/data2$avr_int_go[contador1]
  }
  if (data2$type[contador1] == 4){
    data2$dispersao[contador1] <- abs(data2$avr_int_se[contador1]-data2$value[contador1])/data2$avr_int_se[contador1]
  }
  
  
  if (data2$income[contador1]=="A"){
    data2$income1[contador1] <- 5
  }
  if (data2$income[contador1]=="B"){
    data2$income1[contador1] <- 4
  } 
  if (data2$income[contador1]=="C"){
    data2$income1[contador1] <- 3
  }
  if (data2$income[contador1]=="D"){
    data2$income1[contador1] <- 2 
  }
  if (data2$income[contador1]=="E"){
    data2$income1[contador1] <- 1
  }
  
  
  contador1 <- contador1+1
  print(data2$dispersao[contador1])
  print(contador1)
}
write.csv(data2,"/Curso-ML/Assignment-3/transactions-1.csv")