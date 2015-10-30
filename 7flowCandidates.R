
spCandidates <- function (type1A,type1B,signo)  {
                          #type1A,type1B,generic2A,generic2B,signo
                          


  if (signo==1) {   #err rows type 'bike'
      col2A <- intersect(grep(pattern=genericA,err[,1]), which(err[,2] > 0)  )
    
      } else  { col2A <-  intersect(grep(pattern=genericA,err[,1]), which(err[,2] < 0)  ) }

  if (signo==1) {   #err rows type 'car'
        col2B <- intersect(grep(pattern=genericB,err[,1]), which(err[,2] < 0)  )
  } else  { col2B <-  intersect(grep(pattern=genericB,err[,1]), which(err[,2] > 0)  ) }
  
 
  #recupera nombre > coge el par q CONVENGA -- PENDIENTE
  col2A <- err[col2A,1]  #hay q coger aquellos q sean más similares (minima distancia)
  col2B <- err[col2B,1]
  
  type2A <<- col2A[1]     #LAZY, PICK NO.1 
  type2B  <<- col2B[1]
    

#  col2AB <- expand.grid(cols2A,cols2B,stringsAsFactors = F)    #all the combinations ??
   
#spCandidates <- which (sp[,type1A]!=0 & sp[,type1B]!=0 & sp[,type2A]!=0 & sp[,type2B]!=0)   #FINAL
spCandidates <- which (sp[,type1A]!=0 & sp[,type1B]!=0 )
  
  }