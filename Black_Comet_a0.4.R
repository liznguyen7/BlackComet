### calculate PAR, R/FR ratio, and graph out spectrum from Black commet data file (".IRR"). 
###### Kazu Nozue (Nov 08, 2011)
###### alpha version 0.4 (062112)
###### for Liza (040716)
### R/FR function
R_FR_ratio<-function(spec,resolution){ #1st column is wavelength(nm), 2nd column is fluence rate (micro E) measured by Black Comet	
	#print(as.character(spec))
	R<-sum(as.numeric(as.vector(spec[as.vector(spec[,1])>=655&as.vector(spec[,1])<=665,2])))* resolution;#print(paste("R=",R)) 
	FR<-sum(as.numeric(as.vector(spec[as.vector(spec[,1])>=725&as.vector(spec[,1])<=735,2])))* resolution;#print(paste("FR=",FR))
	#print(paste("R/FR=",R/FR))
	return(R/FR)
	}
#PAR function
PAR<-function(spec, resolution){
	PAR.microE<-sum(as.numeric(as.vector(spec[as.vector(spec[,1])>=400&as.vector(spec[,1])<=700,2])))* resolution
	return(PAR.microE)
	}
#### plot
spec.graph2<-function(specdata,resolution, color,title) #1s column of specdata is wavelength(nm), 2nd column are fluence rate measured by Black Comet. color is "black", "red", "blue", "gree", "pink", etc
{
	file.name<-gsub(" ","_",gsub(":","",Sys.time()))
	pdf(file=paste("spec",file.name,".pdf",sep=""),width=11,height=8)
	matplot(x=specdata[-dim(specdata)[1],1],y=specdata[-dim(specdata)[1],2],ylim=c(0,max(specdata[-dim(specdata)[1],2]*1.2)),xlim=c(min(as.numeric(as.character(specdata[-dim(specdata)[1],1]))),max(as.numeric(as.character(specdata[-dim(specdata)[1],1])))),ylab="fluence rate (µE)",xlab="wavelength (nm)", col=color, type="l",main=title)
	dev.off()
}

### example
#### set working directory
setwd("/Volumes/Data6/data_JM4/BlackComet")
# first test
# Angle 1: straight up (12 in from plant)
GH1040116<-read.table("040116GH1.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH1040116,0.5) # 
R_FR_ratio(GH1040116,0.5) # 1.12
spec.graph2(GH1040116,0.5,"blue","GH1040116") # find a pdf file in directory in your 
# Angle 2: ~45 degree (12 in from plant)
GH2040116<-read.table("040116GH2.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH2040116,0.5) # this must be 587.1529
R_FR_ratio(GH2040116,0.5) # 1.12
spec.graph2(GH2040116,0.5,"blue","GH2040116") # find a pdf file in directory in your 

# Angle 1: straight up (directly next to plant)
GH3040116<-read.table("040116GH3.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH3040116,0.5) # 652.6963
R_FR_ratio(GH3040116,0.5) # 1.07
spec.graph2(GH3040116,0.5,"blue","GH3040116") # find a pdf file in directory in your 
# Angle 2: ~45 degree (directly next to plant)
GH4040116<-read.table("040116GH4.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH4040116,0.5) # 433.8649
R_FR_ratio(GH4040116,0.5) # 0.9603109
spec.graph2(GH4040116,0.5,"blue","GH4040116") # find a pdf file in directory in your 

### ggplot2 version practice #####
spec.all<-cbind(GH1040116[-1775,],GH2040116[-1775,2],GH3040116[-1775,2],GH4040116[-1775,2])
head(spec.all)
tail(spec.all)
colnames(spec.all)<-c("wavelength","GH1","GH2","GH3","GH4")
qplot(data=spec.all,x=wavelength,y=GH1)
str(spec.all)
class(spec.all[,1])
#spec.all[,1]<-as.numeric(spec.all[,1]) # this will give you strange data
spec.all[,1]<-as.numeric(as.character(spec.all[,1]))
head(spec.all);tail(spec.all)
qplot(data=spec.all[,1:2],x=wavelength,y=GH1)
# use rbind
GH1040116$type<-"GH1"
GH2040116$type<-"GH2"
GH3040116$type<-"GH3"
GH4040116$type<-"GH4"


spec.all.r<-rbind(GH1040116[-1775,],GH2040116[-1775,],GH3040116[-1775,],GH4040116[-1775,])
head(spec.all.r)
summary(spec.all.r)
table(spec.all.r$type)
spec.all.r[,1]<-as.numeric(as.character(spec.all.r[,1]))
qplot(data=spec.all.r,x=V1,y=V2,color=type)
# ver2
GH1040116$angle<-0
GH2040116$angle<-45
GH3040116$angle<-0
GH4040116$angle<-45
# distance
GH1040116$distance<-12
GH2040116$distance<-12
GH3040116$distance<-0
GH4040116$distance<-0
#
head(GH1040116)
spec.all.r<-rbind(GH1040116[-1775,],GH2040116[-1775,],GH3040116[-1775,],GH4040116[-1775,])
head(spec.all.r)
summary(spec.all.r)
table(spec.all.r$angle)
spec.all.r[,1]<-as.numeric(as.character(spec.all.r[,1]))
qplot(data=spec.all.r,x=V1,y=V2,facets = angle ~ distance,color=distance,shape=factor(angle))






