#getting started
#Alene Onion
#December 2018

#The purpose of this script is to QAQC data files #This script requires:
#data.csv: raw data files with the added DEC_sample_type column which identifies the same as a normal sample, blank, matrix spike, duplicate, or duplicate parent
#reporting.limits.csv: reporting limits file that includes accuracy limits, paired parameters that components should be samler than, and an abbreviated name column
#Ohio.Qualifiers.csv: A table of Ohio data qualifiers
#Holding.Times.csv: a lookup table for holding times

#This script associates samples with the nearest qc samples by date
#It is possible to run this script for a large or small data set 
#but I recommend dividing your data into regions (or samplers if you want to be very fine) to refine your application of QC data
#as long as the subset has a complete QC data set (spikes, duplicates, and equipment blanks) it can be run separately

#The output is a flagged data set and a quality report summarizing the flagging process


#first load the data file
data<-read.csv("sections/data/Wallkill_2018_chem.csv")

#first a QC check to make sure this script accoutns for all the possible lab error codes:
lab_qualifiers<-c("",'B','E','J','N','U','UE','UN')
possible_flags<-c('X','X','X','X','X','X','X','X')
possible_flags<-data.frame(lab_qualifiers,possible_flags)
possible_flags$lab_qualifiers<-as.character(possible_flags$lab_qualifiers)
flags<-data
flags$flags_in_this_data<-'X'
flags<-unique(flags[c('lab_qualifiers','flags_in_this_data')])
flags$lab_qualifiers<-as.character(flags$lab_qualifiers)
flags<-merge(possible_flags,flags,by=c('lab_qualifiers'),all=TRUE)
#now set up an error if there are flags not identified in the possible list
flagcheck<-flags[is.na(flags$possible_flags),]
rm(list=c('lab_qualifiers','possible_flags'))
if(length(flagcheck$possible_flags)==0){
   
#run the rmarkdown script for this list
rmarkdown::render("QAQC.Rmd", params = inputs)
}

