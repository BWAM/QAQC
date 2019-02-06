#This script will reformat LCI Data from the EDDs to a format that will go into the Filemaker database more efficiently

#read the data
data<-read.csv("sections/data/projectData/LCI.2018/LCI2018.output.csv")

#change column names according to file L:\DOW\BWAM\LMAS\Lakes Stuff\LCI\LCI_EDD\2017\LCI\2017DataToFM
colnames(data)[colnames(data)=="sys_sample_code"] <- "Sample Name"
colnames(data)[colnames(data)=="sample_date"] <- "Analysis Start Date"
colnames(data)[colnames(data)=="fraction"] <- "Result Sample Fraction"
colnames(data)[colnames(data)=="chemical_name"] <- "Characteristic Name"
colnames(data)[colnames(data)=="result_value"] <- "result value"

#add wanted columns "data$fakecolumnname<-NA"
data$"Lab Name Code"<-NA
data$"Lab Sample Name"<-NA
data$"Result Value Type"<-NA
data$"reporting_detection_limit"<-NA
data$"Analysis Date Timezone"<-"EDT"
data$"Results Detection Condition"<-data$lab_qualifiers

#Create default value system for Results Detection Condition Column
data$"Results Detection Condition"<-as.character(data$"Results Detection Condition")
data$"Results Detection Condition"<-ifelse(grepl("U",data$"Results Detection Condition"),"Not Detected",data$"Results Detection Condition")
data$"Results Detection Condition"<-ifelse(grepl("J",data$"Results Detection Condition"),"Reporting Limit, less than",data$"Results Detection Condition")

#save data as csv in folder
write.csv(data, file="sections/data/projectData/LCI.2018/2018reformdata.csv")