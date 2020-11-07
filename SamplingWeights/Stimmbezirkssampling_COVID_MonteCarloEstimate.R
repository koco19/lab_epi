#COVID-Studie
#Stimmbezirk-Sampling

#Load data
#setwd("E:/COVID-Studie/Stimmbezirke")
input <- read.table(file="Kopie von Wahlbezirke_2020_Strukturindikatoren_mstatistik.csv", header=T, sep=";")

#Select relevant variables...
input <- input[,c("Gebietsnummer","Anzahl.Einwohner","Anteil.Migrationshintergrund","Anteil.18.24.Jährige",
                  "Anteil.25.34.Jährige","Anteil.35.44.Jährige","Anteil.45.59.Jährige","Anteil.60.Jährige.und.Ältere",
                  "Anteil.Einpersonenhaushalten","Anteil.Haushalte.mit.Kindern")]
#...and rows (of the Stimmbezirke)
data <- input[c(1:755),]

#Row 756 contains the sums for all of Munich
gesamt <- input[756,]
row.names(gesamt) <- c("Wert")

#Calculate allowed ranges for the mean of the 100 selected Stimmbezirke 
gesamt["untereGrenze","Anzahl.Einwohner"] <- round((gesamt["Wert","Anzahl.Einwohner"]/nrow(data))-0.1*(gesamt["Wert","Anzahl.Einwohner"]/nrow(data)),0)
gesamt["obereGrenze","Anzahl.Einwohner"] <- round((gesamt["Wert","Anzahl.Einwohner"]/nrow(data))+0.1*(gesamt["Wert","Anzahl.Einwohner"]/nrow(data)),0)

gesamt["untereGrenze","Anteil.Migrationshintergrund"] <- round((gesamt["Wert","Anteil.Migrationshintergrund"]-0.1*gesamt["Wert","Anteil.Migrationshintergrund"]),3)
gesamt["obereGrenze","Anteil.Migrationshintergrund"] <- round((gesamt["Wert","Anteil.Migrationshintergrund"]+0.1*gesamt["Wert","Anteil.Migrationshintergrund"]),3)

gesamt["untereGrenze","Anteil.18.24.Jährige"] <- round((gesamt["Wert","Anteil.18.24.Jährige"]-0.1*gesamt["Wert","Anteil.18.24.Jährige"]),3)
gesamt["obereGrenze","Anteil.18.24.Jährige"] <- round((gesamt["Wert","Anteil.18.24.Jährige"]+0.1*gesamt["Wert","Anteil.18.24.Jährige"]),3)

gesamt["untereGrenze","Anteil.25.34.Jährige"] <- round((gesamt["Wert","Anteil.25.34.Jährige"]-0.1*gesamt["Wert","Anteil.25.34.Jährige"]),3)
gesamt["obereGrenze","Anteil.25.34.Jährige"] <- round((gesamt["Wert","Anteil.25.34.Jährige"]+0.1*gesamt["Wert","Anteil.25.34.Jährige"]),3)

gesamt["untereGrenze","Anteil.35.44.Jährige"] <- round((gesamt["Wert","Anteil.35.44.Jährige"]-0.1*gesamt["Wert","Anteil.35.44.Jährige"]),3)
gesamt["obereGrenze","Anteil.35.44.Jährige"] <- round((gesamt["Wert","Anteil.35.44.Jährige"]+0.1*gesamt["Wert","Anteil.35.44.Jährige"]),3)

gesamt["untereGrenze","Anteil.45.59.Jährige"] <- round((gesamt["Wert","Anteil.45.59.Jährige"]-0.1*gesamt["Wert","Anteil.45.59.Jährige"]),3)
gesamt["obereGrenze","Anteil.45.59.Jährige"] <- round((gesamt["Wert","Anteil.45.59.Jährige"]+0.1*gesamt["Wert","Anteil.45.59.Jährige"]),3)

gesamt["untereGrenze","Anteil.60.Jährige.und.Ältere"] <- round((gesamt["Wert","Anteil.60.Jährige.und.Ältere"]-0.1*gesamt["Wert","Anteil.60.Jährige.und.Ältere"]),3)
gesamt["obereGrenze","Anteil.60.Jährige.und.Ältere"] <- round((gesamt["Wert","Anteil.60.Jährige.und.Ältere"]+0.1*gesamt["Wert","Anteil.60.Jährige.und.Ältere"]),3)

gesamt["untereGrenze","Anteil.Einpersonenhaushalten"] <- round((gesamt["Wert","Anteil.Einpersonenhaushalten"]-0.1*gesamt["Wert","Anteil.Einpersonenhaushalten"]),3)
gesamt["obereGrenze","Anteil.Einpersonenhaushalten"] <- round((gesamt["Wert","Anteil.Einpersonenhaushalten"]+0.1*gesamt["Wert","Anteil.Einpersonenhaushalten"]),3)

gesamt["untereGrenze","Anteil.Haushalte.mit.Kindern"] <- round((gesamt["Wert","Anteil.Haushalte.mit.Kindern"]-0.1*gesamt["Wert","Anteil.Haushalte.mit.Kindern"]),3)
gesamt["obereGrenze","Anteil.Haushalte.mit.Kindern"] <- round((gesamt["Wert","Anteil.Haushalte.mit.Kindern"]+0.1*gesamt["Wert","Anteil.Haushalte.mit.Kindern"]),3)


#Create function for random draw
sample.stimmbezirke.fun <- function(data,n,seed){
  #assuring repeatability of random selection by setting a seed
  #set.seed(seed)
  
  #sampling Stimmbezirke (n rows from data without replacement and equal probability for every row)
  out <- data[sample(c(1:nrow(data)),size=n,replace=F),]
  
  #checking if means are in allowed ranges
  if(mean(out$Anzahl.Einwohner)>gesamt["untereGrenze","Anzahl.Einwohner"] & mean(out$Anzahl.Einwohner)<gesamt["obereGrenze","Anzahl.Einwohner"]){
    check_Anzahl.Einwohner <- 1
    out_Anzahl.Einwohner <- "Mittelwert von Anzahl.Einwohner liegt im vorgesehenen Bereich."
  }else{
    check_Anzahl.Einwohner <- 0
    out_Anzahl.Einwohner <- paste0("Mittelwert von Anzahl.Einwohner liegt außerhalb des vorgesehenen Bereiches: ",mean(out$Anzahl.Einwohner))
  }
  
  if(mean(out$Anteil.Migrationshintergrund)>gesamt["untereGrenze","Anteil.Migrationshintergrund"] & mean(out$Anteil.Migrationshintergrund)<gesamt["obereGrenze","Anteil.Migrationshintergrund"]){
    check_Anteil.Migrationshintergrund <- 1
    out_Anteil.Migrationshintergrund <- "Mittelwert von Anteil.Migrationshintergrund liegt im vorgesehenen Bereich."
  }else{
    check_Anteil.Migrationshintergrund <- 0
    out_Anteil.Migrationshintergrund <- paste0("Mittelwert von Anteil.Migrationshintergrund liegt außerhalb des vorgesehenen Bereiches: ",mean(out$Anteil.Migrationshintergrund))
  }
  
  if(mean(out$Anteil.18.24.Jährige)>gesamt["untereGrenze","Anteil.18.24.Jährige"] & mean(out$Anteil.18.24.Jährige)<gesamt["obereGrenze","Anteil.18.24.Jährige"]){
    check_Anteil.18.24.Jährige <- 1
    out_Anteil.18.24.Jährige <- "Mittelwert von Anteil.18.24.Jährige liegt im vorgesehenen Bereich."
  }else{
    check_Anteil.18.24.Jährige <- 0
    out_Anteil.18.24.Jährige <- paste0("Mittelwert von Anteil.18.24.Jährige liegt außerhalb des vorgesehenen Bereiches: ",mean(out$Anteil.18.24.Jährige))
  }
  
  if(mean(out$Anteil.25.34.Jährige)>gesamt["untereGrenze","Anteil.25.34.Jährige"] & mean(out$Anteil.25.34.Jährige)<gesamt["obereGrenze","Anteil.25.34.Jährige"]){
    check_Anteil.25.34.Jährige <- 1
    out_Anteil.25.34.Jährige <- "Mittelwert von Anteil.25.34.Jährige liegt im vorgesehenen Bereich."
  }else{
    check_Anteil.25.34.Jährige <- 0
    out_Anteil.25.34.Jährige <- paste0("Mittelwert von Anteil.25.34.Jährige liegt außerhalb des vorgesehenen Bereiches: ",mean(out$Anteil.25.34.Jährige))
  }
  
  if(mean(out$Anteil.35.44.Jährige)>gesamt["untereGrenze","Anteil.35.44.Jährige"] & mean(out$Anteil.35.44.Jährige)<gesamt["obereGrenze","Anteil.35.44.Jährige"]){
    check_Anteil.35.44.Jährige <- 1
    out_Anteil.35.44.Jährige <- "Mittelwert von Anteil.35.44.Jährige liegt im vorgesehenen Bereich."
  }else{
    check_Anteil.35.44.Jährige <- 0
    out_Anteil.35.44.Jährige <- paste0("Mittelwert von Anteil.35.44.Jährige liegt außerhalb des vorgesehenen Bereiches: ",mean(out$Anteil.35.44.Jährige))
  }
  
  if(mean(out$Anteil.45.59.Jährige)>gesamt["untereGrenze","Anteil.45.59.Jährige"] & mean(out$Anteil.45.59.Jährige)<gesamt["obereGrenze","Anteil.45.59.Jährige"]){
    check_Anteil.45.59.Jährige <- 1
    out_Anteil.45.59.Jährige <- "Mittelwert von Anteil.45.59.Jährige liegt im vorgesehenen Bereich."
  }else{
    check_Anteil.45.59.Jährige <- 0
    out_Anteil.45.59.Jährige <- paste0("Mittelwert von Anteil.45.59.Jährige liegt außerhalb des vorgesehenen Bereiches: ",mean(out$Anteil.45.59.Jährige))
  }
  
  if(mean(out$Anteil.60.Jährige.und.Ältere)>gesamt["untereGrenze","Anteil.60.Jährige.und.Ältere"] & mean(out$Anteil.60.Jährige.und.Ältere)<gesamt["obereGrenze","Anteil.60.Jährige.und.Ältere"]){
    check_Anteil.60.Jährige.und.Ältere <- 1
    out_Anteil.60.Jährige.und.Ältere <- "Mittelwert von Anteil.60.Jährige.und.Ältere liegt im vorgesehenen Bereich."
  }else{
    check_Anteil.60.Jährige.und.Ältere <- 0
    out_Anteil.60.Jährige.und.Ältere <- paste0("Mittelwert von Anteil.60.Jährige.und.Ältere liegt außerhalb des vorgesehenen Bereiches: ",mean(out$Anteil.60.Jährige.und.Ältere))
  }
  
  if(mean(out$Anteil.Einpersonenhaushalten)>gesamt["untereGrenze","Anteil.Einpersonenhaushalten"] & mean(out$Anteil.Einpersonenhaushalten)<gesamt["obereGrenze","Anteil.Einpersonenhaushalten"]){
    check_Anteil.Einpersonenhaushalten <- 1
    out_Anteil.Einpersonenhaushalten <- "Mittelwert von Anteil.Einpersonenhaushalten liegt im vorgesehenen Bereich."
  }else{
    check_Anteil.Einpersonenhaushalten <- 0
    out_Anteil.Einpersonenhaushalten <- paste0("Mittelwert von Anteil.Einpersonenhaushalten liegt außerhalb des vorgesehenen Bereiches: ",mean(out$Anteil.Einpersonenhaushalten))
  }
  
  if(mean(out$Anteil.Haushalte.mit.Kindern)>gesamt["untereGrenze","Anteil.Haushalte.mit.Kindern"] & mean(out$Anteil.Haushalte.mit.Kindern)<gesamt["obereGrenze","Anteil.Haushalte.mit.Kindern"]){
    check_Anteil.Haushalte.mit.Kindern <- 1
    out_Anteil.Haushalte.mit.Kindern <- "Mittelwert von Anteil.Haushalte.mit.Kindern liegt im vorgesehenen Bereich."
  }else{
    check_Anteil.Haushalte.mit.Kindern <- 0
    out_Anteil.Haushalte.mit.Kindern <- paste0("Mittelwert von Anteil.Haushalte.mit.Kindern liegt außerhalb des vorgesehenen Bereiches: ",mean(out$Anteil.Haushalte.mit.Kindern))
  }
  
  #if all means are fine, return selection
  if(check_Anzahl.Einwohner==1 | check_Anteil.Migrationshintergrund==1 & check_Anteil.18.24.Jährige==1 & check_Anteil.25.34.Jährige==1 & 
     check_Anteil.35.44.Jährige==1 & check_Anteil.45.59.Jährige==1 & check_Anteil.60.Jährige.und.Ältere==1 & 
     check_Anteil.Einpersonenhaushalten==1 & check_Anteil.Haushalte.mit.Kindern==1){
    return(list(out,list(out_Anzahl.Einwohner,out_Anteil.Migrationshintergrund,out_Anteil.18.24.Jährige,out_Anteil.25.34.Jährige,
                         out_Anteil.35.44.Jährige,out_Anteil.45.59.Jährige,out_Anteil.60.Jährige.und.Ältere,
                         out_Anteil.Einpersonenhaushalten,out_Anteil.Haushalte.mit.Kindern)))
  }else{
    return(list("Try again",list(out_Anzahl.Einwohner,out_Anteil.Migrationshintergrund,out_Anteil.18.24.Jährige,out_Anteil.25.34.Jährige,
                         out_Anteil.35.44.Jährige,out_Anteil.45.59.Jährige,out_Anteil.60.Jährige.und.Ältere,
                         out_Anteil.Einpersonenhaushalten,out_Anteil.Haushalte.mit.Kindern)))
  }
}

#use function
out <- sample.stimmbezirke.fun(data=data,n=100,seed=123645)
out[[2]]
#get selected dataset from output list
output <- out[[1]]

#save dataset
#setwd("E:/COVID-Studie/Stimmbezirke")
#saveRDS(output, file = "sample_Stimmbezirke.rds")

#adjust Stimmbezirk-ID to shp-file format, as described twice in previous emails 
#output <- readRDS(file="sample_Stimmbezirke.rds")
#library(stringr, lib.loc = "D:/R-3.6.1/library")
library(stringr)

qgis <- output[,c(1:2)]
qgis$Gebietsnummer_char <- as.character(output$Gebietsnummer)
qgis$Anzahl.Einwohner <- NULL
qgis$length <- str_length(qgis$Gebietsnummer_char)
for(i in 1:nrow(qgis)){
  if(qgis$length[i]==4){
    qgis$Gebietsnummer_neu[i] <-paste0(str_sub(qgis$Gebietsnummer_char[i],1,1),str_sub(qgis$Gebietsnummer_char[i],3,4))
  }else if(qgis$length[i]==5){
    qgis$Gebietsnummer_neu[i] <-paste0(str_sub(qgis$Gebietsnummer_char[i],1,2),str_sub(qgis$Gebietsnummer_char[i],4,5))
  }
}
rm(i)

qgis$Gebietsnummer_neu_num <- as.numeric(qgis$Gebietsnummer_neu)
qgis <- qgis[order(qgis$Gebietsnummer_neu_num),]


#Function for Monte Carlo estimate
estimate.cons <- function(iterations){
  #create inital matrix
  matrix_estimate <- matrix(0,nrow = 755,ncol = 2)
  colnames(matrix_estimate) <- c("Con_ID", "Column_2")
  matrix_estimate[,1] = data$Gebietsnummer
  
  N <- 0
  while (N < iterations){
    samp <- sample.stimmbezirke.fun(data=data, n= 100)
    #print(samp[[1]]$Gebietsnummer)
    if (try(is.null(samp[[1]]))){
      #sample not successful
      print("try again")
    }else{
      #sample successful
      samp_ids <- samp[[1]]$Gebietsnummer
      for (i in 1:100){
        row = which(matrix_estimate == samp[[1]]$Gebietsnummer[i])
        #print(samp[[1]]$Gebietsnummer[i])
        matrix_estimate[row, "Column_2"] <- matrix_estimate[row, "Column_2"]+1
      }
      N = N+1
    }
  }
  print(sum(matrix_estimate[,"Column_2"])) #to check of right number of cons is added (should be 100*number iterations)
  print(N)
  matrix_estimate[,"Column_2"] <- matrix_estimate[,"Column_2"]/N
  return(matrix_estimate)
}

#run the monte carlo estimate with 5000 samples
estimate <- estimate.cons(5000)

