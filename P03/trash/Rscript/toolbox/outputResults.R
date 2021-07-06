output2wayANOVA <- function(forDrawingSigANOVA) {
  fVal <- c("A","B","A x B")
  for (i in 1:3) {
    
    befSt <- paste("F(",round(forDrawingSigANOVA[(i-1)*2+2,]$df.col, digits = 3),
                   ", ",round(forDrawingSigANOVA[i*2+1,]$df.col, digits = 3)
                   ,") = ",signif(forDrawingSigANOVA[i*2,]$f.col,digits = 4),",", sep = '')
    if( forDrawingSigANOVA[i*2,]$p.col > 0.001){
      pVal<- paste("p = ",signif(forDrawingSigANOVA[i*2,]$p.col,digits = 3),",", sep = '')
    }else{
      pVal<- paste("p < 0.001,", sep = '')
    }
    etaVal<- paste("eta =",signif(forDrawingSigANOVA[i*2,]$`p.eta^2`,digits = 3))
    cat("Factor",fVal[i],": \n")
    cat( befSt, pVal, etaVal,"\n")
  }
}

output1wayANOVA <- function(forDrawingSigANOVA) {
  fVal <- c("A")
  befSt <- paste("F(",forDrawingSigANOVA[1,]$df.col,",",round(forDrawingSigANOVA[2,]$df.col, digits = 3),") = ", signif(forDrawingSigANOVA[2,]$f.col,digits = 4),",", sep = '')
  if( forDrawingSigANOVA[2,]$p.col > 0.001){
    pVal<- paste("p = ",signif(forDrawingSigANOVA[2,]$p.col,digits = 3),",", sep = '')
  }else{
    pVal<- paste("p < 0.001,", sep = '')
  }
  etaVal<- paste("eta =",signif(forDrawingSigANOVA[2,]$`p.eta^2`,digits = 3))
  
  cat("Factor",fVal[1],": \n")
  cat( befSt, pVal, etaVal,"\n")
}

makeSigPair <- function(forDrawingPost) {
  
  sigPairA = NULL
  if(length(forDrawingPost[["A"]]) > 1) {
    t = as.character(forDrawingPost[["A"]][["bontab"]][["significance"]])
    for(i in 1:length(t)){
      t0 = strsplit(t[i], " ")
      if(t0[[1]][4] == "*"){
        sigPairA = rbind(sigPairA,t0[[1]][1:3])
      }
    }
  }
  
  sigPairB = NULL
  if(length(forDrawingPost[["B"]]) > 1) {
    t = as.character(forDrawingPost[["B"]][["bontab"]][["significance"]])
    for(i in 1:length(t)){
      t0 = strsplit(t[i], " ")
      if(t0[[1]][4] == "*"){
        sigPairB = rbind(sigPairB,t0[[1]][1:3])
      }
    }
  }
  return(rbind(sigPairA,sigPairB))
}
  
  