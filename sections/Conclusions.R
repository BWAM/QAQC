## ----setupC, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
#file.copy("sections/images", ".", recursive = TRUE)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#creating one flag to rule them all and interpetation of this flag

#convert all NA values in pass fields to A
data$labpass<-ifelse(is.na(data$labpass),"A",data$labpass)
data$spikepass<-ifelse(is.na(data$spikepass),"A",data$spikepass)
data$duppass<-ifelse(is.na(data$duppass),"A",data$duppass)
data$EBpass<-ifelse(is.na(data$EBpass),"A",data$EBpass)
data$pairpass<-ifelse(is.na(data$pairpass),"A",data$pairpass)
data$HTpass<-ifelse(is.na(data$HTpass),"A",data$HTpass)
#create an overall QA field
data$validator_qualifiers<-"A"
data$validator_qualifiers<-ifelse((data$labpass=="R"|data$spikepass=="R"|data$duppass=="R"|data$EBpass=="R"|data$pairpass=="R"|data$HTpass=="R"),"R",
           ifelse(data$EBpass=="T","T",
           ifelse((data$labpass=="J"|data$EBpass=="J"),"J",
           ifelse(data$labpass=="U","U",data$validator_qualifiers))))

#add explanation for flagged data for each type
#first create the field
data$interpreted_qualifiers<-"Normal"

#For lab errors
#R flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$labpass=="R","lab error","Normal"),
                                ifelse(data$labpass=="R",paste(data$interpreted_qualifiers," ; lab error",sep=""),data$interpreted_qualifiers))
#J flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$labpass=="J","estimated value due to lab error","Normal"),
                                ifelse(data$labpass=="J",paste(data$interpreted_qualifiers," ; estimated value due to lab error",sep=""),data$interpreted_qualifiers))
#U flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$labpass=="U","Analyte was analyzed for but not detected","Normal"),
                                ifelse(data$labpass=="U",paste(data$interpreted_qualifiers," ; Analyte was analyzed for but not detected",sep=""),data$interpreted_qualifiers))

#For EB errors
#R flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$EBpass=="R","Equipment Blank error","Normal"),
                                ifelse(data$EBpass=="R",paste(data$interpreted_qualifiers," ; Equipment Blank error",sep=""),data$interpreted_qualifiers))
#J flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$EBpass=="J","estimated value due to Equipment Blank error","Normal"),
                                ifelse(data$EBpass=="J",paste(data$interpreted_qualifiers," ; estimated value due to Equipment Blank error",sep=""),data$interpreted_qualifiers))

#For spike errors
#R flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$spikepass=="R","Accuracy error","Normal"),
                                ifelse(data$spikepass=="R",paste(data$interpreted_qualifiers," ; Accuracy error",sep=""),data$interpreted_qualifiers))

#For dup errors
#R flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$duppass=="R","Precision error","Normal"),
                                ifelse(data$duppass=="R",paste(data$interpreted_qualifiers," ; Precision error",sep=""),data$interpreted_qualifiers))


#For pair errors
#R flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$pairpass=="R","Result should be less than the total","Normal"),
                                ifelse(data$pairpass=="R",paste(data$interpreted_qualifiers," ; Result should be less than the total",sep=""),data$interpreted_qualifiers))

#For HT errors
#R flags
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",
                                ifelse(data$HTpass=="R","Holding Time error","Normal"),
                                ifelse(data$HTpass=="R",paste(data$interpreted_qualifiers," ; Holding Time error",sep=""),data$interpreted_qualifiers))

#changing the 'Normal' interpreted flag to ""
data$interpreted_qualifiers<-ifelse(data$interpreted_qualifiers=="Normal",'',data$interpreted_qualifiers)


## ----message=FALSE, warning=FALSE, results='asis'------------------------
flags<-read.csv("data/validator_flags.csv")

knitr::kable(flags)
rm(flags)

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#plotting the sample failures
#count fails and passes
library(plyr)
labfails<-count(data[c('short','validator_qualifiers')])
names(labfails)[names(labfails)=='validator_qualifiers']<-'legend'

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

 print(labfails<-(ggplot() +
    geom_col(data=labfails,aes(short,freq,fill=legend)) +
      coord_flip() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank())))
rm(list=c('labfails'))

## ----message=FALSE, warning=FALSE, results='asis'------------------------
#plot the result value / day for each param 
#color the points based on flags
labfails<-unique(data$chemical_name)
labfails<-count(data[c('short','interpreted_qualifiers')])
names(labfails)[names(labfails)=='interpreted_qualifiers']<-'legend'


library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())


 #plot the graph without the legend
 print((ggplot() +
    geom_col(data=labfails,aes(short,freq,fill=legend)) +
      coord_flip() +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")))
#plot just the legend
 legend<-(ggplot() +
                  geom_col(data=labfails,aes(short,freq,fill=legend)) +
                  coord_flip() +
                  theme_void())
legend<-get_legend(legend)
as_ggplot(legend)
rm(legend)

rm(list=c('labfails'))


