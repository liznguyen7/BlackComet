### calculate PAR, R/FR ratio, and graph out spectrum from Black commet data file (".IRR"). 
###### Kazu Nozue (Nov 08, 2011)
###### alpha version 0.4 (062112)
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
setwd("C:/Users/Susan_Bush/Desktop/Susan")# 
test<-read.table("Rtest.IRR",header=FALSE,skip=2) # this is Susan's data
PAR(test,0.5) # this must be 155.8013
R_FR_ratio(test,0.5) #This must be 2.258859 (middle of #13 shelf in a big chamber (501?), 050812)
spec.graph2(test,0.5,"blue","test") # find a pdf file in directory in your 


### Coral (left LED chamber R92) (043014)
Coral1<-read.table("/Volumes/Data6/data_JM4/spec_data/Coral_043024red_leftR92.IRR",header=FALSE,skip=2)
red<-function(spec, resolution){
	PAR.microE<-sum(as.numeric(as.vector(spec[as.vector(spec[,1])>=600&as.vector(spec[,1])<=800,2])))* resolution
	return(PAR.microE)
	}
spec.graph2(Coral1,0.5,color="black",title="Coral 04/30/2014 Maloof lab left chamber (R92)")
red(Coral1,0.5) #15.19749

#### coral
leftredcoral052314_1<-read.table("/Volumes/Data6/data_JM4/spec_data/Coral/052314red1.IRR",header=FALSE,skip=2)
red(leftredcoral052314_1,0.5) #6.293363
leftredcoral052314_2<-read.table("/Volumes/Data6/data_JM4/spec_data/Coral/052314red2.IRR",header=FALSE,skip=2)
red(leftredcoral052314_2,0.5) #[1] 15.1955

#### 060414
right<-read.table("060414right1.IRR",header=FALSE,skip=2) 
PAR(right,0.5) # this must be 155.8013
R_FR_ratio(test,0.5) #42.49373)
spec.graph2(right,0.5,"black","right chamber") # find a pdf file in directory in your 

right<-read.table("060414right1.IRR",header=FALSE,skip=2) 
PAR(right,0.5) # 42.49373
R_FR_ratio(right,0.5) #1.356436
spec.graph2(right,0.5,"black","right chamber") # find a pdf file in directory in your 

left<-read.table("060414left.IRR",header=FALSE,skip=2) 
PAR(left,0.5) # 42.53426 
R_FR_ratio(left,0.5) #0.5018428
spec.graph2(left,0.5,"black","left chamber") # find a pdf file in directory in your 

# 041615 CEF49 and 53 for short day exp (041715)
CEF49.2.2<-read.table("/Volumes/Data6/data_JM4/spec_data/041615CEF49and53/041615cef49_2_2.IRR",header=FALSE,skip=2) # old "sun" condition (six fluorescence lamps and four incandescent lamps)
CEF49.0.2<-read.table("/Volumes/Data6/data_JM4/spec_data/041615CEF49and53/041615cef49_0_2.IRR",header=FALSE,skip=2) # only four incandescent lamp
CEF49.2.0<-read.table("/Volumes/Data6/data_JM4/spec_data/041615CEF49and53/041615cef49_2_0.IRR",header=FALSE,skip=2) # new "sun" condition (only six fluorescence lamps
CEF53.2.2<-read.table("/Volumes/Data6/data_JM4/spec_data/041615CEF49and53/041615cef53_2_2.IRR",header=FALSE,skip=2) # old "shade" condition (six fluorescence lamps and two incandescent lamps were burned (only two on))
CEF53.2.0<-read.table("/Volumes/Data6/data_JM4/spec_data/041615CEF49and53/041615cef53_2_0.IRR",header=FALSE,skip=2) # new "shade" condition (six fluorescence lamps and two FR lamps)
# old
R_FR_ratio(CEF49.2.2,0.5) #1.2
R_FR_ratio(CEF53.2.2,0.5) #0.6
PAR(CEF49.2.2,0.5) #160.0361
PAR(CEF53.2.2,0.5) #188.0692

# new
R_FR_ratio(CEF49.2.0,0.5) #5.6
R_FR_ratio(CEF53.2.0,0.5) #0.56
PAR(CEF49.2.0,0.5) #169.3515
PAR(CEF53.2.0,0.5) #162.6394

# draw graphs
# lamp
CEF49.2.2$lamp<-"F_I"
CEF49.2.0$lamp<-"F"
CEF49.0.2$lamp<-"I"
CEF53.2.2$lamp<-"F_I"
CEF53.2.0$lamp<-"F"
# treatment
CEF49.2.2$CEF<-"49"
CEF49.2.0$CEF<-"49"
CEF49.0.2$CEF<-"49"
CEF53.2.2$CEF<-"53"
CEF53.2.0$CEF<-"53"
# merge
CEF<-rbind(CEF49.2.2,CEF49.2.0,CEF49.0.2,CEF53.2.2,CEF53.2.0)

names(CEF)[1:2]<-c("wave_length","Fluence_rate")
summary(CEF$wave_length==":")
CEF<-CEF[!CEF$wave_length==":",]
CEF$wave_length<-as.numeric(as.character(CEF$wave_length))
# CEF[,1]<-as.numeric(CEF[,1])
# CEF[,2]<-as.numeric(CEF[,2])
# graph
p<-ggplot(data=CEF,aes(x=wave_length,y=Fluence_rate,colour=CEF)) + geom_point()
p<-p + facet_grid(CEF~lamp)
p
ggsave("/Volumes/Data7/Arabi_SAS_phenotyping/SAS_SD/CEF49_53_spec_041615.pdf",width=11,height=8)








#










