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
#type
# use rbind
GH1040116$type<-"GH1"
GH2040116$type<-"GH2"
GH3040116$type<-"GH3"
GH4040116$type<-"GH4"


head(GH1040116)
spec.all.r<-rbind(GH1040116[-1775,],GH2040116[-1775,],GH3040116[-1775,],GH4040116[-1775,])
head(spec.all.r)
summary(spec.all.r)
table(spec.all.r$angle)
spec.all.r[,1]<-as.numeric(as.character(spec.all.r[,1]))
qplot(data=spec.all.r,x=V1,y=V2,facets = angle ~ distance,color=distance,shape=factor(angle))

# assignment (041216)
## use ggplot() to draw the same graph
# hint (in Chapter 4). Find our wich geom you need to use.
p <- ggplot(mtcars, aes(mpg, wt))
p <- p + geom_point(colour = "darkblue")
p
# bonus: calculate R/FR ratio and add them to each plot.
# your answer ###


###make same graph as qplot using ggplot
#use same data code from qplot for ggplot
plot<-ggplot(spec.all.r, aes(V1,V2))
plot<-plot+geom_point()
plot
##gives you basic all 4 spectrum lines on one graph
plot<-ggplot(spec.all.r, aes(V1,V2,color=type)) + geom_point() + facet_grid(distance~angle)
plot
##should give you 2x2 plot 
plot<-ggplot(spec.all.r, aes(V1,V2,color=type,shape=factor(angle))) + geom_point() + facet_grid(angle~distance)
##add angle in as legend
plot<-ggplot(spec.all.r, aes(V1,V2,color=type,shape=factor(angle),size=distance)) +scale_size_continuous(range=c(1,2)) + geom_point() + facet_grid(angle~distance)
plot
##add distance in as legend --how to get constant size??
##how to change x and y axis? 

str(spec.all.r)
summary(spec.all.r)
table(spec.all.r$distance)
# having the same size (KN)
plot<-ggplot(spec.all.r, aes(V1,V2,color=type,shape=factor(angle))) + geom_point() + facet_grid(angle~distance)
plot
# 
plot<-ggplot(spec.all.r, aes(V1,V2,color=type,shape=factor(angle),size=2)) + geom_point() + facet_grid(angle~distance)
plot
#
plot<-ggplot(spec.all.r, aes(V1,V2,color=type,shape=factor(angle))) + geom_point() + facet_grid(angle~distance)
plot

<<<<<<< HEAD



###make same graph as qplot using ggplot
#use same data code from qplot for ggplot
plot<-ggplot(spec.all.r, aes(V1,V2))
plot<-plot+geom_point()
plot
##gives you basic all 4 spectrum lines on one graph
plot<-ggplot(spec.all.r, aes(V1,V2,color=type)) + geom_point() + facet_grid(distance~angle)
plot
##should give you 2x2 plot 
plot<-ggplot(spec.all.r, aes(V1,V2,color=type,shape=factor(angle))) + geom_point() + facet_grid(angle~distance)
##add angle in as legend
plot<-ggplot(spec.all.r, aes(V1,V2,color=type,shape=factor(angle),size=distance)) +scale_size_continuous(range=c(1,2)) + geom_point() + facet_grid(angle~distance)
##add distance in as legend --how to get constant size??
##how to change x and y axis? 

=======
# chagne x, y axis label (ver1) # did not work
# head(spec.all.r)
# names(spec.all.r)[1:2]
# names(spec.all.r)[1:2]<-c("wavelength (nm)","fluence rate (µE)")
# names(spec.all.r)
# plot<-ggplot(spec.all.r, aes(x=wavelength (nm),y=names(spec.all.r)[2],color=type,shape=factor(angle))) + geom_point() + facet_grid(angle~distance)
# plot
# chagne x,y axis label (ver2)
head(spec.all.r)
plot<-ggplot(spec.all.r, aes(x=V1,y=V2,color=type,shape=factor(angle))) + geom_point() + facet_grid(angle~distance)
plot<-plot + labs(x="wavelength (nm)", y="fluence rate (µE)")
plot

## 050316
# GH1 # Angle 1: straight up (no surrounding plant)
GH1050316<-read.table("050316GH1.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH1050316,0.5) # 492
R_FR_ratio(GH1050316,0.5) # 0.61
# GH2 Angle 2: ~90 degree (no surrounding plant)
GH2050316<-read.table("050316GH2.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH2050316,0.5) # 388
R_FR_ratio(GH2050316,0.5) # 0.97
# GH3 straight up with surrounding plants
GH3050316<-read.table("050316GH3.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH3050316,0.5) # 256
R_FR_ratio(GH3050316,0.5) # 0.66

# GH4 -90 degree with surrounding plants
GH4050316<-read.table("050316GH4.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH4050316,0.5) # 315
R_FR_ratio(GH4050316,0.5) # 0.89

#
# ver2
# angle of sensor
GH1050316$angle<-0
GH2050316$angle<-90
GH3050316$angle<-0
GH4050316$angle<-90
# treatment
GH1050316$trt<-"alone"
GH2050316$trt<-"alone"
GH3050316$trt<-"surrounded"
GH4050316$trt<-"surrounded"

spec.all.r<-rbind(GH1050316[-1775,],GH2050316[-1775,],GH3050316[-1775,],GH4050316[-1775,])
head(spec.all.r)
summary(spec.all.r)
table(spec.all.r$angle)
spec.all.r[,1]<-as.numeric(as.character(spec.all.r[,1]))
library(ggplot2)
plot<-ggplot(spec.all.r, aes(x=V1,y=V2,shape=factor(angle))) + geom_point() + facet_grid(angle~trt)
plot<-plot + labs(x="wavelength (nm)")
plot

=======
## add text R/FR ratio & title
plot<-plot + ggtitle('042216 Spectrum') + annotate("text", x=700,y=.5,label=c("1.106",".9964","1.124","1.008"))
##label=ratio values you obtain - must match trial to plot yourself

##add ratio 
xpos<- c(300,300,600,600)
ypos<- c(0.8,0.8,0.8,0.8)
lab<- c(1,2,3,4)
ldata <- data.frame(xpos,ypos,lab,angle=c(0,0,90,90),distance=c(0,4,0,4),type=c("GH1","GH2","GH3","GH4"))

plot<-ggplot(spec.all.r, aes(x=V1,y=V2,shape=factor(angle))) + geom_point(aes(color=type)) + facet_grid(angle~distance)
plot
plot + geom_text(data=ldata,aes(x=xpos,y=ypos,label=lab))

# 050516 
# inside green hosue
# GH1 # Angle 1: straight up (no surrounding plant)
GH1IN050616<-read.table("050616GH1IN.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH1IN050516,0.5) # 178.9069
R_FR_ratio(GH1IN050516,0.5) # 1.17833
# GH2 Angle 2: ~90 degree (no surrounding plant)
GH2IN050616<-read.table("050616GH2IN.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH2IN050516,0.5) # 79.82014
R_FR_ratio(GH2IN050516,0.5) # 1.207607
# GH3 straight up with surrounding plants
GH3IN050616<-read.table("050616GH3IN.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH3IN050516,0.5) # 106.1501
R_FR_ratio(GH3IN050516,0.5) # 0.846201
# outside green house
GH4IN050616<-read.table("050616GH4IN.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH4IN050516,0.5) # 59.84898
R_FR_ratio(GH4IN050516,0.5) # 0.8984954
# GH1 # Angle 1: straight up (no surrounding plant) oustiside greenhosue
GH1OUT050616<-read.table("050616GH1OUT.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH1OUT050516,0.5) # 498.1909
R_FR_ratio(GH1OUT050516,0.5) # 1.261936
# GH2 # Angle 1: straight up (no surrounding plant) oustiside greenhosue
GH2OUT050616<-read.table("050616GH2OUT.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH2OUT050516,0.5) # 162.5635
R_FR_ratio(GH2OUT050516,0.5) # 1.292816


GH1IN050616$angle<-0
GH2IN050616$angle<-90
GH3IN050616$angle<-0
GH4IN050616$angle<-90
GH1OUT050616$angle<-0
GH2OUT050616$angle<-90
# type
GH1IN050616$type<-"SUN"
GH2IN050616$type<-"SUN"
GH3IN050616$type<-"SHADE"
GH4IN050616$type<-"SHADE"
GH1OUT050616$type<-"SUN"
GH2OUT050616$type<-"SUN"
# location
GH1IN050616$location<-"GH"
GH2IN050616$location<-"GH"
GH3IN050616$location<-"GH"
GH4IN050616$location<-"GH"
GH1OUT050616$location<-"OUT"
GH2OUT050616$location<-"OUT"


spec.all.r<-rbind(GH1IN050616[-1775,],GH2IN050616[-1775,],GH3IN050616[-1775,],GH4IN050616[-1775,],GH1OUT050616[-1775,],GH2OUT050616[-1775,])
spec.all.r[,1]<-as.numeric(as.character(spec.all.r[,1]))
spec.all.r$type<-factor(spec.all.r$type,levels=c("OUT","SUN","SHADE"))
plot<-ggplot(spec.all.r, aes(x=V1,y=V2,shape=factor(angle),color=factor(location))) + geom_point(aes(color=type)) + facet_grid(angle~type)
plot + ggtitle('050616 Spectrum') + labs(x="wavelength (nm)", y="fluence rate (µE)")
ggsave(file="spec050516.pdf") # Please add R/FR ratio inside plot

 R_FR_ratio(GH1IN050616,0.5)
# 1.115923
R_FR_ratio(GH2IN050616,0.5)
#0.9796839
R_FR_ratio(GH3IN050616,0.5)
# 1.159585
R_FR_ratio(GH4IN050616,0.5)
# 0.9138912
R_FR_ratio(GH1OUT050616,0.5)
#1.183582
R_FR_ratio(GH2OUT050616,0.5)
#1.17013

#add ratio to plot
xpos<- c(300,300,300,300,600,600)
ypos<- c(7.5,2.5,2.5,2.5,1.5,0)
lab<- c(1.18,1.17,1.16,0.91,1.12,0.98) #must match ratio numbers to location
ldata <- data.frame(xpos,ypos,lab,angle=c(0,90,0,90,0,90),type=c("SUN","SUN","SHADE","SHADE","SUN","SUN"),location=c("OUT","OUT","SHADE","SHADE","SUN","SUN"))

plot<- plot + geom_text(data=ldata,aes(x=xpos,y=ypos,label=lab)) 
plot
plot + ggtitle('050616 Spectrum') + labs(x="wavelength (nm)", y="fluence rate (µE)")
plot

# filtering effect in greenhouse
relative.spec<-data.frame(wavelength=GH1IN050516[-1775,1],filtered=GH1OUT050516[-1775,2]/GH1IN050516[-1775,2])
relative.spec[is.na(relative.spec)]<-0
relative.spec$wavelength<-as.numeric(as.character(relative.spec$wavelength))
filtered.spec.plot<-qplot(data=relative.spec,x=wavelength,y=filtered)
ggsave(file="filtered.spec.plot.pdf",plot=filtered.spec.plot)

# 050616 (tomato dense vs non dense)
# inside green hosue
# GH1 # Angle 1: straight up (no surrounding plant)
GH1IN050616<-read.table("050616GH1IN.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH1IN050616,0.5) # 1041.308
R_FR_ratio(GH1IN050616,0.5) # 1.115923
# GH2 Angle 2: ~90 degree (no surrounding plant)
GH2IN050616<-read.table("050616GH2IN.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH2IN050616,0.5) # 196.4688
R_FR_ratio(GH2IN050616,0.5) # 0.9796839
# GH3 straight up with surrounding plants
GH3IN050616<-read.table("050616GH3IN.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH3IN050616,0.5) # 523.7732
R_FR_ratio(GH3IN050616,0.5) # 1.159585
# GH4 
GH4IN050616<-read.table("050616GH4IN.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH4IN050616,0.5) # 158.7858
R_FR_ratio(GH4IN050616,0.5) # 0.9138912
# GH1 # Angle 1: straight up (no surrounding plant) oustiside greenhosue
GH1OUT050616<-read.table("050616GH1OUT.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH1OUT050616,0.5) # 2167.11
R_FR_ratio(GH1OUT050616,0.5) # 1.183582
# GH2 # Angle 1: straight up (no surrounding plant) oustiside greenhosue
GH2OUT050616<-read.table("050616GH2OUT.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(GH2OUT050616,0.5) # 474.5879
R_FR_ratio(GH2OUT050616,0.5) # 1.17013


GH1IN050616$angle<-0
GH2IN050616$angle<-90
GH3IN050616$angle<-0
GH4IN050616$angle<-90
GH1OUT050616$angle<-0
GH2OUT050616$angle<-90
# type
GH1IN050616$type<-"SUN"
GH2IN050616$type<-"SUN"
GH3IN050616$type<-"SHADE"
GH4IN050616$type<-"SHADE"
GH1OUT050616$type<-"SUN"
GH2OUT050616$type<-"SUN"
# location
GH1IN050616$location<-"GH"
GH2IN050616$location<-"GH"
GH3IN050616$location<-"GH"
GH4IN050616$location<-"GH"
GH1OUT050616$location<-"OUT"
GH2OUT050616$location<-"OUT"


spec.all.r<-rbind(GH1IN050616[-1775,],GH2IN050616[-1775,],GH3IN050616[-1775,],GH4IN050616[-1775,],GH1OUT050616[-1775,],GH2OUT050616[-1775,])
spec.all.r[,1]<-as.numeric(as.character(spec.all.r[,1]))
spec.all.r$type<-factor(spec.all.r$type,levels=c("OUT","SUN","SHADE"))
plot<-ggplot(spec.all.r, aes(x=V1,y=V2,shape=factor(angle),color=factor(location))) + geom_point() + facet_grid(angle~type)
plot
ggsave(file="spec050616.tomato.denseVSnondense.pdf") # Please add R/FR ratio inside plot


