

getCandidates2  <-   function(signo) {
  
  for (m in (1:length(genericA))) {
  if (m==1) {repoA <- grep(pattern=paste('\\b',genericA[1],'\\b',sep=''),err[,1],value=T)   }  
  repoA <- grep(pattern=paste('\\b',genericA[m],'\\b',sep=''),repoA,value = T)
    }
  
  for (m in (1:length(genericB))) {
    if (m==1) {repoB <- grep(pattern=paste('\\b',genericB[1],'\\b',sep=''),err[,1],value=T)   }  
    repoB <- grep(pattern=paste('\\b',genericB[m],'\\b',sep=''),repoB,value = T)
  }
  

  #get 2A terms
  if (signo==1) {   #get err rows like '*bike', w. balance<0
    
    col2A <- intersect(repoA, err[which(err[,2] > 0),1] )
    
  } else  #idem, w. balance>0
    
  { col2A <-  intersect(repoA, err[which(err[,2] < 0),1]  ) }
  
  ##### get 2B terms
  if (signo==1) {   #err rows type 'car', balance < 0
    
    col2B <- intersect(repoB, err[which(err[,2] < 0),1]  )
    
  } else
    
  { col2B <-  intersect(repoB, err[which(err[,2] > 0),1]) }
  
  
  ###checking there are valid columns
  if (length(col2A)==0 | length(col2B)==0) {candidates2 <- NA
  
  } else  {
    
    dist <- matrix(0,nrow=length(col2A), ncol=length(col2B))
    
    rownames(dist) <- col2A
    colnames(dist) <- col2B
    
    
    #build distance matrix
    for (s in (1:length(col2A))) {
      for (t in (1:length(col2B))) {
        
        
        x<-unlist(strsplit(col2A[s], split=".", fixed=T))
        y<-unlist(strsplit(col2B[t], split=".", fixed=T))
        
        dist[s,t]<-length(setdiff(x,y))
      } }
    #pick only elements w. minimal distance
    #mindist <<- which(dist==2,arr.ind = T)  #ANALIZAR CASO PARTICULAR: 1 SOLA CELDA
    mindist <<- which(dist==semdiff,arr.ind = T)  #ANALIZAR CASO PARTICULAR: 1 SOLA CELDA
    types2 <-NA
    
    if (length(mindist) >= 1 ) {   #garantizando q hay 'soluciones'
      
      for (n in 1:nrow(mindist)) {
        t2A <- col2A[mindist[n,1]]
        t2B <- col2B[mindist[n,2]]
        types <-c(t2A, t2B)
        ifelse(n==1,types2<-types, types2<-rbind(types2, types))
      }
      
    }
    
    if (is.null(dim(types2)))
    {candidates2<<-types2
    
    } else {rownames(types2)  <- NULL
    colnames(types2)  <- c('type2A','type2B')
    candidates2 <<- as.data.frame(types2)    }
    
    
  }  #end IF(case no col2A or col2B found)  
  
}