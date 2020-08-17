setwd(?????)

#required libraries

library(plyr)

#read the data set 
recording.data <- read.csv('recording-data.csv',header=T)
treat.assig.data <- read.csv('treatment-id-assignation.csv',header=T)
treat.assig.data[,2] <- factor(treat.assig.data[,2])


#remove all the rows without data
datos <- subset(recording.data,recording.data[,"Trial"]!="NA")
datos <- subset(datos,datos[, "Ear.Tag"]!="NE")
datos <- subset(datos,datos[,"Ear.Tag"]!="")
datos[,"Ear.Tag"] <- factor(datos[,"Ear.Tag"])

#base on the Worksheet colour blinded and without key file we
#merge the information to get the treatments for each cow
dat.id <- merge(datos,treat.assig.data,by='Ear.Tag')
aux <- dat.id[,c(1,7,9:40,44,47)]

aux2 <- aggregate(aux[,-1],by=list(aux$Ear.Tag,aux$Trial.Day),sum)
aux3 <- aggregate(dat.id[,c(6,7,54)],by=list(dat.id$Ear.Tag,aux$Trial.Day),unique)

final <- data.frame(aux3[,3:5],aux2)
final$time.obs <- final$td..tAnalyzed.duration-final$td.Out.of.viex-final$td.Disturbance


#we are separating the data base on day
day0.f <- subset(final,Trial.Day==-1)
day1.f <- subset(final,Trial.Day==0)

#all the plots for day 0
sp.lis <- split(day0.f[,38],day0.f$Treatment.2)

###3 descriptives
#day 0
aux <-c(1,2,3,4,5,6,7,8,10,12,14,16,18,20,22,24,26,28,30,32,33,35,37,39,40)
aux2 <-day0.f[,-aux]
aux3 <-round(aux2[,-c(13,14,15)]/60,1)
day0.data <- data.frame(Treatment.2=day0.f$Treatment.2,aux3,aux2[,c(13,14,15)])

#combine lying right and left
day0.data$td.lying.rf <- day0.data$td.Lying2+day0.data$td.Lying3
day0.data$tn.head.rub <- day0.f$tn.Standing2+day0.f$tn.Lying5

levels(day0.data$Treatment.2) <- c("Scarification.M.bovis","Scarification.M.Bovoculi","Scarification.only")

summ.aux0 <- ddply(day0.data, .(Treatment.2), function(x){
median <- apply(x[,-1],2,median) 
min <- apply(x[,-1],2,min)
q1 <- apply(x[,-1],2,quantile,probs=0.25)
q3 <- apply(x[,-1],2,quantile,probs=0.75)
max <- apply(x[,-1],2,max)
return(data.frame(median,min,q1,q3,max))
  } 
)

#Information for table 2 and 3 (table said -1 instead of 0)
summary.day0 <- data.frame(variable=names(day0.data)[-1],summ.aux0)


######## data for day 1
aux <- c(1,2,3,4,5,6,7,8,10,12,14,16,18,20,22,24,26,28,30,32,33,35,37,39,40)
aux2 <- day1.f[,-aux]
aux3 <- round(aux2[,-c(13,14,15)]/60,1)
day1.data <- data.frame(Treatment.2=day1.f$Treatment.2,aux3,aux2[,c(13,14,15)])

#combine lying right and left
day1.data$td.lying.rf <- day1.data$td.Lying2+day1.data$td.Lying3
day1.data$tn.head.rub <- day1.f$tn.Standing2+day1.f$tn.Lying5

levels(day1.data$Treatment.2) <- c("Scarification.M.bovis","Scarification.M.Bovoculi","Scarification.only")

summ.aux1 <- ddply(day1.data, .(Treatment.2), function(x){
  median <- apply(x[,-1],2,median) 
  min <- apply(x[,-1],2,min)
  q1 <- apply(x[,-1],2,quantile,probs=0.25)
  q3 <- apply(x[,-1],2,quantile,probs=0.75)
  max <- apply(x[,-1],2,max)
  return(data.frame(median,min,q1,q3,max))
} 
)


#Infrmation for table 2 and  4 for day 1 (in the table said day 0 instead of 1)
summary.day1 <- data.frame(variable=names(day1.data)[-1],summ.aux1)


