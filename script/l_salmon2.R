##sets working directory
setwd("~/Esmelda")
##loads text files
plate1=read.table("plate1.txt",header = F,sep = "\t")
## view loaded files
View(plate1)
##loads the drc package
install.packages("drc")
library(drc)

#makes df of average standards and concentrations
stddf=data.frame(matrix(ncol=2,nrow=5))
stddf$X1=(plate1$V1[1:5]+plate1$V2[1:5])/2
stddf$X2=c(0.001,3.6,11,33,100)
#X1 is OD, X2 is concentration, stddf is the dataframe containing these
#generates 4-parameter model
m4=drm(X1~X2,data=stddf,fct=LL.4())
e= m4$parmMat[4]
c= m4$parmMat[2]
d= m4$parmMat[3]
f= abs(m4$parmMat[1])
#test the parameters to see if output makes sense
conc=e*((((c-d)/(0.148-d))-1)^(1/f))
#now do for complete dataset
#make i the columns numbers and j the row numbers
conc_plate1=data.frame(matrix(nrow=8,ncol=12))
for (i in 1:12)
{
  for (j in 1:8)
  {
    conc_plate1[j,i]=e*((((c-d)/(plate1[j,i]-d))-1)^(1/f))
  }
}
## exporting tables or files into csv or text file
#exports the file to the desired working directory as csv file
write.table(conc_plate1,"Plate 1 conc.csv",sep=",",col.names=T,row.names=F)
