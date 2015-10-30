
########## FLOWS BALANCING

rm(list=ls())     #reset

library(dplyr)  
library(stringr)
#library(data.table)
library(sqldf)
source('7flowCandidates.R')

err <-read.csv("./solution1/err_Manch_afl6_iter2_2015-10-08.csv", as.is=T)

sp <- read.csv( "./solution1/sp_flows_Manch_allocflow6_2015-10-08.csv", as.is=T)
ct3<- ct3 <-read.csv("./MSOA_Manch/ct3.csv",header=T,as.is=TRUE)  # ?read. crosstabs

err.total <- sum(abs(err[,2]))       # desequilibrio total

i <- 1

while (err.total > 0)  {   #SI AUN EXISTE ERROR en  SP .......

   type1A <-  subtypes1A <- as.character(err[i,1])                 #a16-f-bicycle    
   subtypes1A <- unlist(strsplit(type1A, split='.',fixed=T))       #a16  f   bicycle
   
   cat('Iter:', i,"\n") 
   writeLines('==========')
    
#   
  
  while (err[i,2] !=0)   {    #CONDITION: TYPE HAS ERROR
    print(err[,1:2])  
    balance <- err[i,2]                     #can be >0 or <0
    
    if (balance>0) {  listCandidates <- which(err[,2] < 0)  #busca negativos
                      signo <- -1
    
    } else        {   listCandidates <- which(err[,2] > 0)  #busca positivos
                      signo <- 1
    }
    
#     type1A <-  subtypes1A <- as.character(err[i,1])                 #a16-f-bicycle    
#     subtypes1A <- unlist(strsplit(type1A, split='.',fixed=T))    #a16  f   bicycle
    
    ###########Search type1B        #BUSCAR BALANCEADOR
    
    
    n=1
    repeat {       #find type1B-type2A-type2B  + no. candidates
    
    type1B <- as.character(err[listCandidates[n],1])  # loop: start w. the closest candidate (alt: best candidate)
    subtypes1B <- unlist(strsplit(type1B, split=".", fixed=T))    #a16  f   bicycle
    
    a=abs( err[which(err[,1]==type1A),2] )
    b=abs( err[which(err[,1]==type1B),2] )
    disponible <- min(a,b)
    
    genericA <- setdiff(subtypes1A,subtypes1B)  #  bicycle  (or bicycle | male)
    genericB <- setdiff(subtypes1B,subtypes1A)  #####  car 
    
    if (length(genericA)!=1) {
      genericA <- paste(genericA,collapse='.')}
    
    if (length(genericB)!=1) {
      genericB <- paste(genericB,collapse='.')}
    
    candidates <- spCandidates(type1A,type1B,signo) ### get rows in SP for ALL candidateS
    #candidates <- spCandidates(type1A,type1B,generic2A,generic2B, signo) ### get no. row in sp for ALL candidateS 
    
    n <- n+1
    if (length(candidates)>0) {break}          #EXIT LOOP
          }
    
    
    
    #MUEVE BALANCE
    # ==============
      #if (length(candidates) > abs(balance)) {
        
        transfer <- min(length(candidates),abs(balance),disponible)     # j=col1B
        target <- sample(candidates,size=transfer )    #prob=vector of popul per type
        
        sp[target,type1A] <- sp[target,type1A] + signo
        sp[target,type1B] <- sp[target,type1B] - signo  
#         sp[target,type2A] <- sp[target,type2A] + signo
#         sp[target,type2B] <- sp[target,type2B] - signo
                          #}
        
        #########RECALCULA errores totales/locales
        err[,2] <- colSums(sp[,7:86]) - ct3$total0 
        
   
         }       #CONDITION: TYPE HAS ERROR
  
  
 i <- i+1 
 err.total <- sum(abs(err[,2]))       # desequilibrio total  
   
 
}

spfile <- paste("./solution1/sp_flows_Manch_allocflow6_",Sys.Date(),"CORREG.csv",sep="")
write.csv(sp,file=spfile)

cat('All done !!')