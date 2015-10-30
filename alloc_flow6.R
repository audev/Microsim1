########## FLOWS ALLOCATION

rm(list=ls())
#setwd("//me-filer1/home$/au232/My Documents/1.CEDAR/3_Studies !!/21-Dft bid/9-FLOWS/FLOW_ALLOC")
 
ct3 <-read.csv("./MSOA_Manch/ct3.csv",header=T,as.is=TRUE)
# Cross-tabs
flow <-read.csv(file='./flows/flow_Manch.csv',header=T)

# rows=crosstabs, columns= msoas
results <- matrix(0,nrow=nrow(flow),ncol=nrow(ct3))   # flows-v x types->
minialloc   <- matrix(0,nrow=nrow(ct3),ncol=1)               # remains calc
colnames(results) <-ct3$type
type <-vector(mode='character',length=8)

probs <- data.frame(matrix(0,nrow=1, ncol=80))
#probs <-t(probs)
colnames(probs) <- colnames(results)

###LITTLE USED (yet)
nflows <-nrow(flow)  #depends on city  
ntypes <-nrow(ct3)   #80
ntraits <- 3  #age-sex-mode
categs <- c('male','female','a16','a25','a35','a50','a65',
            'bicycle','bus','car','home','other','pass','train','walk')



for (i in 1:nflows)  { #LOOP flows
#for (i in 1:10000)    {
     
     if  (flow[i,'all']==1) {      #POP=1  >>TRIVIAL (a16=1, female=1, passenger=1)
          colsnotnull <-  which(flow[i,6:20]!=0)
          target <- colnames(flow[,6:ncol(flow)])[colsnotnull]
          typestr <-paste(target[2],target[1],target[3],sep='-')  #builds 'a16-female-pass' string
          results[i,typestr]<-1} #flow 1
     
     else {      #TOTAL FLOW >1  
          #########
          probs <-data.frame(t(ct3$total))   #probs=totals, then transpose
          colnames(probs) <- colnames(results)
          subflow <-flow[i,6:20]  #filter names & totals 
          row.names(subflow)<- NULL
          
          colnull <- colnames(subflow[,subflow==0,drop=F])
          
          
          
          subflow <-subflow[,subflow!=0] #keep only flows >0
          subflow <-sort(subflow)    #ORDENA de menor a mayor
          
     
          for (col in colnull) {   #make probabilities of impossible types=0 
                col1 <- paste('\\b',col,'\\b',sep='') #for grep
                col1 <-grep(pattern=col1,colnames(results),value=F)
                probs[col1] <- 0  
                              }   #anuladas las categs q no existen en subflow
                
                vprob <- probs
                results[i,] <-0 #reset before allocation- NOT NEEDED ??


          #prepares allocation (gets singles > 0 for sorting)
#           subflow <-flow[i,6:20]  #filter names & totals 
#           row.names(subflow)<- NULL
#           subflow <-subflow[,subflow!=0]
#           subflow <-sort(subflow)    #only cols w. values NOT 0 & sorted in increasing order
         
        
          for (j in 1:length(subflow))  { # ALLOCATION OF EACH TYPE in increasing order: a16-a25-bus-female....
               vprob <- probs
               
               #this is just for saving and checking the algorithm works fine
               ifelse(j==1,tsubflow<-t(subflow),tsubflow<-cbind(tsubflow,t(subflow)))
               ifelse(j==1,tvprob<-t(vprob),tvprob<-cbind(tvprob,t(vprob)))
               #write.csv(tvprob,file=paste("./solution/","-vprobi-",i,".csv",sep="_"))
               #write.csv(tsubflow,file=paste("./solution/","-subflowi-",i,".csv",sep=""))                
               
               #subflow <-sort(subflow)
               n <-subflow[,j]   #total no. people to allocate for the specific aggregate (a16)
               
               #run if there's people in the flow pending allocation
               if (n>0 & sum(vprob)>0)     {    ####MINI ALLOCATION & RECOUNTING
                    col <- colnames(subflow[j])  
                    col1 <- paste('\\b',col,'\\b',sep='') #for grep
                    colnotnull <-grep(pattern=col1,colnames(results))
                    vprob[-colnotnull]<-0   #pick only possible columns 
                    
                    minialloc <- rmultinom(n=1,size=n,prob=vprob)   # allocate randomly
                    
                    #for checking
                    ifelse(j==1,tminialloc <- minialloc,tminialloc<-cbind(tminialloc,minialloc))
                    #write.csv(tminialloc,file=paste("-minialloc-i-",i,".csv",sep="_"))
                    results[i,] <- results[i,] + minialloc
                    
                    ###start recount      
                   allocated <-which(minialloc!=0)  #get columns names<> 0
                                        
                    for (k in allocated)     {   #SINGLES TYPES REDUCTION (a16=a16-allocated)
                         n1 <-minialloc[k]    #no. indiv. allocated to that type
                         type <- rownames(minialloc)[k]
                         singles <- unlist(strsplit(type,split='-'))
                         
                         subflow[singles] <- subflow[singles] - n1
                         
                         for (m in 1:length(singles)) {
                          if (subflow[singles[m]]<=0) {  #SINGLES probs= 0, if null     
                              deletethis <- paste('\\b',singles[m],'\\b',sep='')
                              probs[grep(pattern=deletethis, names(probs))]<-0} #end IF  
                                                       } #SINGLES= 0
                                             } #END singles reduction          
                                        
                                        } ##END ALLOCATION & RECOUNTING (n>0)
                                   
                                   
                                   subflow <-sort(subflow)
                                   }   ##### END ALLOCATION ITSELF
                    
                    resto <-flow[i,'all']-sum(results[i,])
                    
                    #error checking (impossible, in theory)
                    if (resto!=0) {
                         write.csv(tminialloc,file=paste("./solution/",i,"-minialloc",".csv",sep="")  )
                         write.csv(tvprob,file=paste("./solution/",i,"-vprob",".csv",sep="")  )
                         write.csv(tsubflow,file=paste("./solution/",i,"-subflow",".csv",sep="")  )
                                    }
                         
               }  #### IF-ELSE (normal) POPULATIONS >1    
     
#       if (flow[i,'all']==1) {
#            ct3$total <- ct3$total-results[i,]    #reduces allocated category 
#                            }                
#         if(sum(ct3$total/ct3$total0<0.1)<5) {           
#           ct3$total <- ct3$total-results[i,]     
#           ct3$total[ct3$total<0] <-0.1    }
     
          ct3$total <- ct3$total-results[i,]    #update crosstabs   
          ct3$total[ct3$total<0] <-0.0001  ###best so far!!!! (99% accuracy)
#NOW RECALCULATE ODDS     
                              
     
     if ((i%%100)==0) {
          message('ITER: ', i)}    
     
    }  #LOOP flows -i

cat('Almost there.......')
err.singles <-data.frame()

row.names(err.singles) <- NULL

err_types  <- round(colSums(results)-ct3$total0,0)
err_typesperc <- round(100*(colSums(results)-ct3$total0 )/ ct3$total0 ,1)
err <-rbind(err_types,err_typesperc)
#names(err) <- colnames(ct3)
sp <-cbind(flow[,1:5],results)

solutionfile <- paste("./solution/sp_flows_Manch_allocflow6_",Sys.Date(),".csv",sep="")
errorfile <- paste("./solution/err_Manch_afl6_",Sys.Date(),".csv",sep="")
write.csv(sp,file=solutionfile)
write.csv(t(err),file=errorfile)

cat('All done !!')