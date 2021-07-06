
# -------------setting path and initializing--------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------

fileLoc = "/Users/yuta/Creative Cloud Files/MATLAB/Insight/hiddenPic4.1.4/dataShow/finalVer/"
saveLoc = "/Users/yuta/Desktop/glare/"

# time course -------------------------------------------------------------

PLRdata <- readMat(paste(fileLoc,"data1.mat", sep = "")) # reading data
PLRdata$anovaData = PLRdata$anovaData * 100 - 100 # ratio to percent

sTime = -0.5
eTime = 1.5

g <- c("PN","PR")

d <- makeDataSetAll(PLRdata, g, seq(sTime,eTime,length=dim(PLRdata$anovaData)[2])) # making dataset using shaded_error.R

config <- list(lim_x = c(-0.2, 1.5),
               lim_y = c(-1, 4),
               alpha = 0.3,
               label_x = "Condition",
               label_y = "Pupil Changes [%]",
               grCol = c("#00BFC4","#F8766D"),
               gr_point = c("#F8766D","#ECB01F","#619CFF"),
               grName <- c(expression(paste("R"["1.5s"])),expression(paste("NR"["1.5s"])))
)

# adjustment of p-value by False Discovery Rate
d1 = PLRdata$anovaData[,,1]
d2 = PLRdata$anovaData[,,2]
pData<-NULL

for (i in 1:dim(d1)[2]){
  a <- t.test( d1[,i], d2[,i], paired=T) # paired t-test
  # t.test(dat_w,dat_m,paired=T
  pData[i]=a$p.value
}

ind_uncorrected = which(pData < 0.05) # looking for the significant period
sorted_pdata = sort(pData[ind_uncorrected]) # looking for the significant period
order_uncorrected = order(pData[ind_uncorrected]) #sorting p-value by descending order

m = length(sorted_pdata)
h<-NULL
for(i in 1 : m){ 
  if(sorted_pdata[i] <= (i/m) * 0.05){
    h[i] = 1
  }else{
    h[i] = 0
  }
}
t = which(h == 0)
order_uncorrected = sort(order_uncorrected[-t])
corrected_pVal = ind_uncorrected[order_uncorrected]

corrected_pVal = (corrected_pVal / (dim(d1)[2])) * (eTime-sTime) + sTime

# show time course of pupil 
p <- disp(d,config,1)
p <- p +
  geom_point(aes(y=rep(-1,length(corrected_pVal)), x = corrected_pVal), color="gray",shape=15)+
  theme(legend.position = 'none')

print(p)
ggsave(file = paste(saveLoc,"insight_exptimeCourse.pdf",".pdf", sep = ""),
       plot = p, 
       dpi = 300, 
       width = 3.27, height = 2.37
       )

