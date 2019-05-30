makePupilDataset_mat2long <- function(dat,nameOfVar, timeLen,fNum,orderName,factors){
  
  numOfTrial = dim(dat$y)[1]
  numOfSub = length(unique(dat$sub))
  lengthOfTime = dim(dat$y)[2]
  
  sTime = timeLen[1]
  eTime = timeLen[2]
  
  x = seq(sTime,eTime,length=lengthOfTime)
 
  ind_data <- data.frame(
    sub =  rep( dat$sub, times = rep( lengthOfTime, numOfTrial)),
    data_y = t(matrix(t(dat$y),nrow=1)),
    data_x = x
    )
 
  if (length(fNum)  == 1){
    eval(parse(text=paste("ind_data$",factors[[1]],"=",
                          "rep( fNum[[1]][dat$condition], times = rep(lengthOfTime, numOfTrial))",
                          sep="")))
    
    eval(parse(text=paste("ind_data$",factors[[1]],"<-",
                          "factor(ind_data$", factors[[1]],",levels = ", orderName[[1]],")", sep="")))
    
    # eval(parse(text=paste( varName, "= aggregate( data_y ~ sub*",
    #                        factors[1,],"*data_x*",",data = ind_data, FUN = 'mean')", sep="")))
  }else{
    eval(parse(text=paste("ind_data$",factors[[1]],"=",
                          "rep( fNum[[1]][dat$condition], times = rep(lengthOfTime, numOfTrial))",
                          sep="")))
    eval(parse(text=paste("ind_data$",factors[[2]],"=",
                          "rep( fNum[[2]][dat$" , nameOfVar[[2]], "], times = rep(lengthOfTime, numOfTrial))",
                          sep="")))
    
    eval(parse(text=paste("ind_data$",factors[[1]],"<-",
                          "factor(ind_data$", factors[[1]],",levels = orderName[[1]])", sep="")))
    eval(parse(text=paste("ind_data$",factors[[2]],"<-",
                          "factor(ind_data$", factors[[2]],",levels = orderName[[2]])", sep="")))
    
    # eval(parse(text=paste( "data_e1 = aggregate( data_y ~ sub*",
    #                        factors[[1]],"*data_x*",factors[[2]], ",data = ind_data, FUN = 'mean')", sep="")))
  }
   
  return(ind_data)
  # data_e1 = aggregate( data_y ~ sub*Color*Pattern*data_x, data = ind_data, FUN = "mean")
  # save(data_e1,file = paste(currentLoc,"/dataset_e1.rda", sep = ""))
  
}