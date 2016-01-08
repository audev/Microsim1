

########## FLOWS BALANCING
##OPTIONAL :  can be run AFTER main script, for better precision.

rm(list=ls())     #reset
##
library(dplyr)
library(stringr)
library(sqldf)
source('getCandidates2.R')  # separa los 3 términos del genericA / genericB

#read data sources
ct3 <-read.csv("./DataSources/Manch_CT3/ct3.csv",header=T,as.is=TRUE)  #

###IPF POPUL
#sp <- read.csv( "sp_IPF.csv", as.is=T)
#err <-read.csv("errorIPF.csv", as.is=T)

###I.A. POPUL
sp <- read.csv( "./DataSources/sp_allocflow6_2015-10-08.csv", as.is=T)
err <-read.csv("./DataSources/err_Manch_2015-10-08.csv", as.is=T)
sp <- sp[,6:86]

####CENSUS FLOWS (only for probs)
flow <- read.csv( "./DataSources/flows/flow_Manch.csv", as.is=T)

err.total <- sum(abs(err[,2]))       # desequilibrio total

candidates2 <- data.frame()
#log <-c('i','j','k','type1A','type1B','type2A','type2B','transfer','balance')
#log <-c('type1A','type1B','type2A','type2B','transfer','balance')
#log <-  as.data.frame(NA) 


for (i in (1:nrow(err))) {

  #read TYPE1A  #

  type1A <-  as.character(err[i,1])                               #a16-f-bicycle
  subtypes1A <- unlist(strsplit(type1A, split='.',fixed=T))       #a16,f,bicycle
  balance <- err[i,2]                     #can be >0 or <0
  ifelse(balance>0,signo <- -1,signo <- 1)

  #GET CANDIDATES types vector for type1B: 4,7,9 ...
  ifelse(signo==-1,candidates1B <- err[which(err[,2] < 0),1],candidates1B <- err[which(err[,2] > 0),1])


  j=1
  OKproceed <- 1

  while (err[i,2]!=0 & j<=length(candidates1B))   {    # 'j' LOOP, CONDITION: balance !=0  OR.. NO MORE CANDIDATES
    #E.candidates <- 0
    type1B <- as.character(candidates1B[j])  # pick candidate sequentially (ALT: pick OPTIMAL)
    subtypes1B <- unlist(strsplit(type1B, split=".", fixed=T))    #a16,f,bicycle

    #get semantic difference 1A <> 1B
    genericA <- setdiff(subtypes1A,subtypes1B)  #  bicycle  (or bicycle | male)
    genericB <- setdiff(subtypes1B,subtypes1A)  #####  car  (or car- | female)
    semdiff <- length(genericA)  #semantic difference between terms 
    

    ###########Search type1B        #BUSCAR BALANCEADOR

    if (length(genericA)<3) {  #for semdiff==3 there are no balancing terms...
      candidates2 <- NA
      getCandidates2(signo)
      k=1

      
      if (class(candidates2)=='data.frame' & !is.null(candidates2)) {
      
        OKproceed <-1
          
        repeat {

          type2A <- as.character(candidates2[k,1])
          type2B <- as.character(candidates2[k,2])
          spCandidates <- which (sp[,type1A]!=0 & sp[,type1B]!=0 & sp[,type2A]!=0 & sp[,type2B]!=0)   #FINAL

          k <- k+1
          #OKproceed <-1

          ############
          #TRANSFER BALANCE
          # ===============

         
          while (balance!=0 & length(spCandidates)>0 & nrow(candidates2)>1 & OKproceed==1)  {

            #calc. how many individuals can be transferred=min of both types 1A-1B
            a <- abs( err[which(err[,1]==type1A),2] )
            b <- abs( err[which(err[,1]==type1B),2] )
            c <- abs( err[which(err[,1]==type2A),2] )
            d <- abs( err[which(err[,1]==type2B),2] )
            disponible <- min(a,b,c,d)
            if (a==0 | b==0 | c==0 | d==0) {OKproceed <- 0}
            
            transfer <- min(length(spCandidates),disponible)     # either: no. of types / min. amount
            #log1 <-as.character(c(i,j,k,type1A,type1B,type2A,type2B,balance,transfer))
            #log <- rbind(log,log1)

            if (transfer>0) {
              
              ifelse(length(spCandidates)==1,target <-spCandidates,
                     target <- sample(spCandidates,size=transfer,prob=flow[spCandidates,'all']))
                  
              #prob=vector of popul per type

              sp[target,type1A] <- sp[target,type1A] + signo    #OK
              sp[target,type1B] <- sp[target,type1B] - signo    #OK
              sp[target,type2A] <- sp[target,type2A] - signo    #OK
              sp[target,type2B] <- sp[target,type2B] + signo    #OK
                            }

            #########RECALC errores totales/locales

            err[,2] <- colSums(sp[,2:81]) - ct3$total0
            err.total <- sum(abs(err[,2]))
            spCandidates <- which (sp[,type1A]!=0 & sp[,type1B]!=0 & sp[,type2A]!=0 & sp[,type2B]!=0)
            balance <- err[i,2]
            
                                                                                          }   #WHILE TRANSFER


          if (err[i,2]==0   | k > nrow(candidates2) | OKproceed==0    ) {break}          #EXIT REPEAT
              }  #end REPEAT

                                              }  #IF candidates2 is data.frame

                      }    #end if (ONLY running for generic A<3)

    j <- j+1

  }       #WHILE MAIN LOOP 'j: CONDITION=TYPE still HAS ERROR

 }        #END FOR

err.total <- sum(abs(err[,2]))       # desequilibrio total
spfile <- paste("sp_Manch_complex_NON-IPF",Sys.Date(),".csv",sep="")
write.csv(sp,file=spfile)

cat('All done !!')



