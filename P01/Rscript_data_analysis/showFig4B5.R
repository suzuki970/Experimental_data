# -------------setting path and initializing--------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "/Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------

saveLoc = currentLoc

countFigNum = 1
sTime = 0
eTime = 3

timeWin = c(1,3)

config <- list(lim_x = c(-0.2, eTime),
               lim_y = c(-20,5),
               sigBarLoc = c(-4.3,-4.4),
               alpha = 0.1,
               stride = 5,
               label_x = "Time [sec]",
               label_y = "Pupil Changes [%]",
               title = "pupil",
               grCol = rep(c("black", "gray28", "gray54", "gray75", "gray88"),2),
               lineTp = c("solid","solid")
)

# t plot  -----------------------------------------------------------------
load(paste(currentLoc,"/dataset_e1e2.rda", sep = ""))
data_e2$data_y = data_e2$data_y * 100 - 100 # to percent from propotion

ribbondata = data_e2
ribbondata = ribbondata[order(ribbondata$sub,ribbondata$Luminance,ribbondata$Pattern),]
data_e3 = data_e3[order(data_e3$sub,data_e3$Luminance,data_e3$Pattern),]

ribbondata$ft = data_e3$data_y

ribbondata = ribbondata[ribbondata$data_x >= 0,]
timeLen = length(ribbondata$data_x[ribbondata$sub == '1'])/10
ribbondata$sub = subName[ribbondata$sub]

numOfSub = length(unique(ribbondata$sub))

## normalization
max_ft = matrix(tapply(ribbondata$ft, ribbondata$sub, max), ncol=1)
ribbondata$max_ft = rep(max_ft,times=rep(timeLen*10,numOfSub))
ribbondata$ft = mapply(function(x, y) {return (x / y)}, ribbondata$ft,ribbondata$max_ft)

##### across luminance ##### 
tVal = NULL
pVal = NULL
g1_name = c("0.58","0.63","0.67","0.70","0.72")
p1_name=c("Glare","Control")

for (pat in 1:2){
  for (j in 1:5){
    for (i in 1:timeLen){
      dat = ribbondata[
        ribbondata$Luminance == g1_name[j] &
          ribbondata$Pattern == p1_name[pat] &
          ribbondata$data_x == ribbondata$data_x[i],]
      res = lm( ft  ~ data_y, data = dat)
      tVal = rbind(tVal,round(summary(res)$coefficients[2,"t value"], digits = 4))
      pVal = rbind(pVal,round(summary(res)$coefficients[2,"Pr(>|t|)"], digits = 4))
    }
  }
}

dat_t = data.frame(
  data_y = tVal,
  data_x = ribbondata$data_x[1:timeLen],
  Luminance = rep(g1_name, times = rep(timeLen,5)),
  Pattern = rep(p1_name, times = rep(timeLen,2))
)
time_p = data.frame(
  data_y = rep(0,length(tVal)),
  data_x = ribbondata$data_x[1:timeLen],
  Luminance = rep(g1_name, times = rep(timeLen,5)),
  Pattern = rep(p1_name, times = rep(timeLen,2))
)

dat_p = data.frame(
  data_y = pVal,
  data_x = ribbondata$data_x[1:timeLen],
  Luminance = rep(g1_name, times = rep(timeLen,5)),
  Pattern = rep(p1_name, times = rep(timeLen,2))
)


for (pat in 1:2){
  for (j in 1:5){
    dat = dat_p[dat_p$Pattern == p1_name[pat] &
                  dat_p$Luminance == g1_name[j],]
    ind_uncorrected = which(dat$data_y < 0.05)             # looking for the significant period
    sorted_pdata = sort(dat$data_y[ind_uncorrected])
    order_uncorrected = order(dat$data_y[ind_uncorrected]) #sorting p-value by descending order
    
    m = length(sorted_pdata)
    h<-NULL
    if(m > 0){
      for(i in 1 : m){
        if(sorted_pdata[i] <= (i/m) * 0.05){
          h[i] = 1
        }else{
          h[i] = 0
        }
      }
      t = which(h == 0)
      if(length(t) > 0){
        order_uncorrected = sort(order_uncorrected[-t])
        corrected_pVal = ind_uncorrected[order_uncorrected]
      }else{
        order_uncorrected = sort(order_uncorrected)
        corrected_pVal = ind_uncorrected[order_uncorrected]
      }
      time_p[time_p$Pattern == p1_name[pat]&
               dat_p$Luminance == g1_name[j],]$data_y[corrected_pVal] = config$sigBarLoc[pat]
    }
  }
}

time_p = time_p[time_p$data_y != 0, ]
p <- ggplot(dat_t,aes(x = data_x, y = data_y, group = Pattern)) +
  geom_line(aes(linetype = Pattern, color = Pattern), size = 0.5) +
  geom_hline(yintercept=0.05, colour="black", linetype="solid", size = 0.5)+
  # geom_line(data=time_p, aes(linetype=Pattern),color = "black", size = 0.5) +
  geom_point(data=time_p, aes(x=data_x, y = data_y, color = Pattern), size = 0.5,shape=15) +
  xlab('Time [sec]') +
  ylab('T-values on correlation') +
  # ggtitle("Correlation in each Contrast and Pattern") +
  ggtitle("A") +
  # scale_color_manual(values = config$grCol)  +
  scale_linetype_manual(values = config$lineTp) +
  scale_x_continuous(breaks = seq(0,config$lim_x[2],1),expand = c(0, 0))

p <- p + facet_grid(. ~ Luminance)
p = setFigureStyle(p)

eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1

#### make csv ----------------------------------------------------------------
tmp = ribbondata[ribbondata$data_x >= timeWin[1],]
tmp = tmp[tmp$data_x <= timeWin[2],]
e1 = aggregate( ft ~ sub*Luminance*Pattern, data = tmp, FUN = "mean")
e2 = aggregate( data_y ~ sub*Luminance*Pattern, data = tmp, FUN = "mean")
e1$data_y = e2$data_y

print("-------  in each Contrast and Pattern --------")
for (pat in 1:2){
  for (j in 1:5){
    dat = e1[e1$Pattern == p1_name[pat] &
               e1$Luminance == g1_name[j],]
    
    res = cor.test(dat$data_y,dat$ft, method="pearson")
    print(paste("R = ",round(res$estimate, digits = 3),
                # ", t = ",round(res$statistic, digits = 4),
                ", p = ",round(res$p.value, digits = 3),sep=""))
  }
}

f = data.frame(
  sub = subName[1:numOfSub],
  y = matrix(e1$ft, nrow = numOfSub, ncol=10),
  y2 = matrix(e1$data_y, nrow = numOfSub, ncol=10)
)

# names(f) <- c("sub",
#               "e1-g-0.58","e1-g-0.63","e1-g-0.67","e1-g-0.70","e1-g-0.72","e1-c-0.58","e1-c-0.63","e1-c-0.67","e1-c-0.70","e1-c-0.72",
#               "e2-g-0.58","e2-g-0.63","e2-g-0.67","e2-g-0.70","e2-g-0.72","e2-c-0.58","e2-c-0.63","e2-c-0.67","e2-c-0.70","e2-c-0.72")
# write.csv(f, paste(saveLoc,"/", "across_luminance.csv", sep = ""), row.names=F, col.names=F)

# among luminance ---------------------------------------------------------
tVal = NULL
pVal = NULL

for (pat in 1:2){
  for (i in 1:timeLen){
    dat = ribbondata[
      ribbondata$Pattern == p1_name[pat] &
        ribbondata$data_x == ribbondata$data_x[i],]
    res = lm( ft  ~ data_y, data = dat)
    tVal = rbind(tVal,round(summary(res)$coefficients[2,"t value"], digits = 4))
    pVal = rbind(pVal,round(summary(res)$coefficients[2,"Pr(>|t|)"], digits = 4))
   }
}

dat_t = data.frame(
  data_y = tVal,
  data_x = ribbondata$data_x[1:timeLen],
  Pattern = rep(p1_name, times = rep(timeLen,2))
)

time_p = data.frame(
  data_y = rep(0,length(tVal)),
  data_x = ribbondata$data_x[1:timeLen],
  Pattern = rep(p1_name, times = rep(timeLen,2))
)

dat_p = data.frame(
  data_y = pVal,
  data_x = ribbondata$data_x[1:timeLen],
  Pattern = rep(p1_name, times = rep(timeLen,2))
)

config$sigBarLoc = c(-5.8,-6.0)

for (pat in 1:2){
  dat = dat_p[dat_p$Pattern == p1_name[pat],]
  ind_uncorrected = which(dat$data_y < 0.05)             # looking for the significant period
  sorted_pdata = sort(dat$data_y[ind_uncorrected])
  order_uncorrected = order(dat$data_y[ind_uncorrected]) #sorting p-value by descending order
  
  m = length(sorted_pdata)
  if(m > 0){
    h<-NULL
    for(i in 1 : m){
      if(sorted_pdata[i] <= (i/m) * 0.05){
        h[i] = 1
      }else{
        h[i] = 0
      }
    }
    t = which(h == 0)
    if(length(t) > 0){
      order_uncorrected = sort(order_uncorrected[-t])
      corrected_pVal = ind_uncorrected[order_uncorrected]
    }else{
      order_uncorrected = sort(order_uncorrected)
      corrected_pVal = ind_uncorrected[order_uncorrected]
    }
    time_p[time_p$Pattern == p1_name[pat],]$data_y[corrected_pVal] = config$sigBarLoc[pat]
  }
}

time_p = time_p[time_p$data_y != 0, ]
p <- ggplot(dat_t,aes(x = data_x, y = data_y, group = Pattern)) +
  geom_line(aes(linetype = Pattern, color = Pattern), size = 0.5) +
  geom_hline(yintercept=0.05, colour="black", linetype="solid", size = 0.5)+
  # geom_line(data=time_p,aes(linetype=Pattern),color = "black", size = 0.5) +
  geom_point(data=time_p, aes(x=data_x, y = data_y, color = Pattern), size = 0.5,shape=15) +
  xlab('Time [sec]') +
  ylab('T-values on correlation') +
  xlab('Time [sec]') +
  # ggtitle("Correlation across Contrast in each Pattern") +
  ggtitle("B") +
  # scale_color_manual(values = config$grCol)  +
  scale_linetype_manual(values = config$lineTp) +
  scale_x_continuous(breaks = seq(0,config$lim_x[2],1),expand = c(0, 0))

p = setFigureStyle(p)

eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1

# make csv ----------------------------------------------------------------
print("-------  across Contrast in each Pattern   --------")
for (pat in 1:2){
  dat = e1[e1$Pattern == p1_name[pat],]
  res = lm( ft ~ data_y, data = dat)
  print(paste("y = ", round(summary(res)$coefficients[2,"Estimate"], digits = 3),
              "x + ", round(summary(res)$coefficients[1,"Estimate"], digits = 3),
              ", t = ", round(summary(res)$coefficients[2,"t value"], digits = 3),
              ", p = ", round(summary(res)$coefficients[2,"Pr(>|t|)"], digits = 6),
              ", R = ", round(cor.test(dat$ft,dat$data_y, method="pearson")$estimate, digits = 4), sep=""))
}

f = data.frame(
  sub = subName[1:numOfSub],
  y  = matrix(e1$ft, nrow = numOfSub*5, ncol=2),
  y2 = matrix(e1$data_y, nrow = numOfSub*5, ncol=2)
)

# names(f) <- c("sub",
#               "ft_g","ft-c","p_g","p_c")
# write.csv(f, paste(saveLoc,"/", "among_luminance.csv", sep = ""), row.names=F, col.names=F)

# across pattern ---------------------------------------------------------
tVal = NULL
pVal = NULL

for (j in 1:5){
  for (i in 1:timeLen){
    dat = ribbondata[
      ribbondata$Luminance == g1_name[j] &
        ribbondata$data_x == ribbondata$data_x[i],]
    res = lm( ft  ~ data_y, data = dat)
    tVal = rbind(tVal,round(summary(res)$coefficients[2,"t value"], digits = 4))
    pVal = rbind(pVal,round(summary(res)$coefficients[2,"Pr(>|t|)"], digits = 4))
  }
}

dat_t = data.frame(
  data_y = tVal,
  data_x = ribbondata$data_x[1:timeLen],
  Luminance = rep(g1_name, times = rep(timeLen,5))
)
time_p = data.frame(
  data_y = rep(0,length(tVal)),
  data_x = ribbondata$data_x[1:timeLen],
  Luminance = rep(g1_name, times = rep(timeLen,5))
)

dat_p = data.frame(
  data_y = pVal,
  data_x = ribbondata$data_x[1:timeLen],
  Luminance = rep(g1_name, times = rep(timeLen,5))
)

config$sigBarLoc = c(-4.1,-4.2)

for (j in 1:5){
  dat = dat_p[dat_p$Luminance == g1_name[j],]
  
  ind_uncorrected = which(dat$data_y < 0.05)             # looking for the significant period
  sorted_pdata = sort(dat$data_y[ind_uncorrected])
  order_uncorrected = order(dat$data_y[ind_uncorrected]) #sorting p-value by descending order
  
  m = length(sorted_pdata)
  if(m > 0){
    h<-NULL
    for(i in 1 : m){
      if(sorted_pdata[i] <= (i/m) * 0.05){
        h[i] = 1
      }else{
        h[i] = 0
      }
    }
    t = which(h == 0)
    if(length(t) > 0){
      order_uncorrected = sort(order_uncorrected[-t])
      corrected_pVal = ind_uncorrected[order_uncorrected]
    }else{
      order_uncorrected = sort(order_uncorrected)
      corrected_pVal = ind_uncorrected[order_uncorrected]
    }
    time_p[time_p$Luminance == g1_name[j],]$data_y[corrected_pVal] = config$sigBarLoc[pat]
  }
}

time_p = time_p[time_p$data_y != 0, ]
p <- ggplot(dat_t,aes(x = data_x, y = data_y, group = Luminance)) +
  geom_line(aes(color = Luminance), size = 0.5) +
  geom_hline(yintercept=0.05, aes(colour=Pattern), linetype="solid", size = 0.5)+
  # geom_line(data=time_p,color = "black", size = 0.5) +
  geom_point(data=time_p, aes(x=data_x, y = data_y,color = Luminance), size = 0.5,shape=15) +
  xlab('Time [sec]') +
  ylab('T-values on correlation') +
  # ggtitle("Correlation in each Contrast across Pattern") +
  ggtitle("C") +
  scale_color_manual(values = config$grCol) +
  scale_x_continuous(breaks = seq(0,config$lim_x[2],1),expand = c(0, 0))
p <- p + facet_grid(. ~ Luminance)
p = setFigureStyle(p)

eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1


# make csv ----------------------------------------------------------------
print("-------  in each Contrast across Pattern  --------")

for (j in 1:5){
  dat = e1[e1$Luminance == g1_name[j],]
  res = lm( ft  ~ data_y, data = dat)
  print(paste(
    ", R = ", round(cor.test(dat$ft,dat$data_y, method="pearson")$estimate, digits = 4), 
    ", p = ", round(summary(res)$coefficients[2,"Pr(>|t|)"], digits = 4),sep=""))
}

f = data.frame(
  sub = subName[1:numOfSub],
  y =  rbind(matrix(e1$ft, nrow = numOfSub, ncol=10)[,1:5],
             matrix(e1$ft, nrow = numOfSub, ncol=10)[,6:10]),
  y2 = rbind(matrix(e1$data_y, nrow = numOfSub, ncol=10)[,1:5],
             matrix(e1$data_y, nrow = numOfSub, ncol=10)[,6:10])
)

# names(f) <- c("sub",
#               "ft0.58","ft0.63","ft0.67","ft0.70","ft0.72","p0.58","p0.63","p0.67","p0.70","p0.72")
# write.csv(f, paste(saveLoc, "across_pattern.csv", sep = ""), row.names=F, col.names=F)

# across all ---------------------------------------------------------------------
print("-------  across all  --------")
dat = e1
res = lmer( data_y ~ ft + (1+ft|Luminance),data = dat)
print(paste(
  "y = ", round(summary(res)$coefficients[2,"Estimate"], digits = 3),
  "x + ", round(summary(res)$coefficients[1,"Estimate"], digits = 3),
  ", t = ", round(summary(res)$coefficients[2,"t value"], digits = 3),
  ", p = ", round(summary(res)$coefficients[2,"Pr(>|t|)"], digits = 6),
  ", R = ", round(cor.test(dat$ft,dat$data_y, method="pearson")$estimate, digits = 4), sep=""))

newavg <- data.frame(ft = seq(0,0.85,0.05))
newavg$reaction <- predict(res, re.form = NA, newavg)

interc_val = ranef(res)$Luminance$`(Intercept)` + summary(res)$coefficients[1,"Estimate"]
slope_val = ranef(res)$Luminance$ft + summary(res)$coefficients[2,"Estimate"]

p <- ggplot(dat,aes(x = ft, y = data_y,color=Luminance)) +
  geom_abline(intercept = interc_val, slope = slope_val, size = 0.1) +
  geom_abline(intercept = summary(res)$coefficients[1,"Estimate"], slope = summary(res)$coefficients[2,"Estimate"], size = 1,color='black') +
  scale_color_manual(values = config$grCol) +
  xlab('Normalized SSVEP amplitude at 7.5Hz') +
  ylab('Pupil change') +
  ggtitle("D") +
  geom_point(aes(shape=Pattern),size=2)
p = setFigureStyle(p)

# show graph  --------------------------------------------------------------
layout <- rbind(c(1, 1),
                c(2, 2),
                c(3, 3))
p = combineGraphs(seq(1,countFigNum-1),'p', layout)
print(p)
# width_fig=10
# height_fig=10
# 
# ggsave(file = paste(saveLoc,"/fig4B5.pdf", sep = ""),
#        plot = p, dpi = 300,
#        width = width_fig, height = height_fig)
