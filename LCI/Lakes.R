#2018/12/5
#Onion
#this script reformats and merges lakes data

#renaming columns so they match
names(location)[names(location)=="LocationID"]<-"LOCATION_ID"
names(location)[names(location)=="LakeID"]<-"LAKE_ID"
names(location)[names(location)=="Type"]<-"Sample_Location_Type"
names(location)[names(location)=="LocationType"]<-"Waterbody_Type"
names(lake)[names(lake)=="LakeID"]<-"LAKE_ID"
names(profiles)[names(profiles)=="Characteristic"]<-"Characteristic.Name"
names(profiles)[names(profiles)=="LocationID"]<-"LOCATION_ID"
names(profiles)[names(profiles)=="Result"]<-"Result.Value"
names(profiles)[names(profiles)=="Result_Value_Type"]<-"Result.Value.Type"
names(profiles)[names(profiles)=="Sample_ID"]<-"SAMPLE_ID"
names(profiles)[names(profiles)=="Unit"]<-"Result.Unit"

#removing duplicative columns
sample<-sample[,!(names(sample) %in% c('LakeDatabase..WATER'))]
sample<-sample[,!(names(sample) %in% c('Sample.5..START_DEPTH'))]
sample<-sample[,!(names(sample) %in% c('Sample.5..INFO_TYPE.1'))]
sample<-sample[,!(names(sample) %in% c('Sample.5..FP'))]
sample<-sample[,!(names(sample) %in% c('Sample.5..DATA_PROVIDER.1'))]
sample<-sample[,!(names(sample) %in% c('Sample.5..DATA_PROVIDER'))]
sample<-sample[,!(names(sample) %in% c('ESF..1'))]
sample<-sample[,!(names(sample) %in% c('TestResults.2..LAB_NAME_CODE'))]
sample<-sample[,!(names(sample) %in% c('LakeDatabase..WATER.1'))]
sample<-sample[,!(names(sample) %in% c('LakeDatabase..County'))]
sample<-sample[,!(names(sample) %in% c('LakeDatabase..Waterbody_Classification'))]
sample<-sample[,!(names(sample) %in% c('LakeDatabase..Waterbody_Type'))]
sample<-sample[,!(names(sample) %in% c('LakeDatabase..M_BAS_NAME'))]
sample<-sample[,!(names(sample) %in% c('HABS..STATUS'))]
results<-results[,!(names(results) %in% c('Sample.5..INFO_TYPE.1'))]
results<-results[,!(names(results) %in% c('Sample.5..SAMPLE_DATE'))]
results<-results[,!(names(results) %in% c('Sample.5..LAKE_ID'))]
results<-results[,!(names(results) %in% c('Sample.5..LOCATION_ID'))]
results<-results[,!(names(results) %in% c('Sample.5..INFO_TYPE'))]
results<-results[,!(names(results) %in% c('LakeDatabase.2..WATER'))]
results<-results[,!(names(results) %in% c('Sample.5..FP'))]
results<-results[,!(names(results) %in% c('Sample.5..START_DEPTH'))]
results<-results[,!(names(results) %in% c('Sample.5..DATA_PROVIDER'))]
results<-results[,!(names(results) %in% c('LakeDatabase.2..County'))]
results<-results[,!(names(results) %in% c('Sample.5..END_DEPTH'))]
results<-results[,!(names(results) %in% c('HABS.4..STATUS'))]
profiles<-profiles[,!(names(profiles) %in% c('LakeDatabase.2..County'))]
profiles<-profiles[,!(names(profiles) %in% c('LakeDatabase.2..WATER'))]
profiles<-profiles[,!(names(profiles) %in% c('LakeInfo.3..Depth_ max'))]
profiles<-profiles[,!(names(profiles) %in% c('Sample.4..Anoxic'))]
profiles<-profiles[,!(names(profiles) %in% c('Sample.4..Hypoxic'))]
profiles<-profiles[,!(names(profiles) %in% c('Sample.4..LAKE_ID'))]
profiles<-profiles[,!(names(profiles) %in% c('Sample.4..SAMPLE_DATE'))]
location<-location[,!(names(location) %in% c('na.strings'))]
location<-location[,!(names(location) %in% c('colNames'))]
location<-location[,!(names(location) %in% c('rowNames'))]
location<-location[,!(names(location) %in% c('skipEmptyRows'))]
location<-location[,!(names(location) %in% c('skipEmptyCols'))]


#now capitalize all the values in these columns which overlap between the tables
#If we didn't do this, two records would be considered different if they were written as "Lake" and "lake"
results$SAMPLE_ID<-toupper(results$SAMPLE_ID)
sample$LAKE_ID<-toupper(sample$LAKE_ID)
sample$LOCATION_ID<-toupper(sample$LOCATION_ID)
sample$SAMPLE_DATE<-toupper(sample$SAMPLE_DATE)
sample$SAMPLE_ID<-toupper(sample$SAMPLE_ID)
sample$INFO_TYPE<-toupper(sample$INFO_TYPE)
location$LAKE_ID<-toupper(location$LAKE_ID)
location$LOCATION_ID<-toupper(location$LOCATION_ID)
location$County<-toupper(location$County)
lake$LAKE_ID<-toupper(lake$LAKE_ID)
lake$County<-toupper(lake$County)

#Add info type to the profile data table
profiles$INFO_TYPE="DP"

#2. Merge the data tables
#merge results table with sample table 
data<-merge(results,sample,by=c("SAMPLE_NAME","SAMPLE_ID"),all=TRUE)

#merge with the location table
data<-merge(data,location,by=c("LAKE_ID","LOCATION_ID"),all=TRUE)

#merge with Lake data table
data<-merge(data,lake,by=c("LAKE_ID","County","Waterbody_Type"),all=TRUE)

#merge with profiles data table
data<-merge(data,profiles,by=c("LOCATION_ID","SAMPLE_ID","Characteristic.Name","Result.Value","Result.Unit","Result.Value.Type","Serial","INFO_TYPE"),all=TRUE)

#remove rows with blank values in crucial fields
#first convert blank values to NA
data$LAKE_ID[data$LAKE_ID==""] <- NA
data$LOCATION_ID[data$LOCATION_ID==""] <- NA
data$SAMPLE_DATE[data$SAMPLE_DATE==""] <- NA
data$SAMPLE_DATE[data$SAMPLE_NAME==""] <- NA
data$SAMPLE_ID[data$SAMPLE_ID==""] <- NA
#then delete na values
#data<-data[!is.na(data$LAKE_ID),]
#data<-data[!is.na(data$LOCATION_ID),]
#data<-data[!is.na(data$SAMPLE_DATE),]
#data<-data[!is.na(data$SAMPLE_NAME),]
#data<-data[!is.na(data$SAMPLE_ID),]

#3. Remove QA samples and HABs samples  
#In other words, we want to keep open water, depth profile, bottom sample, and secchi depth samples  
#data<-data[data$INFO_TYPE=="OW"|data$INFO_TYPE=="DP"|data$INFO_TYPE=="BS"|data$INFO_TYPE=="SD",]

#4.capitalize fields so they can be compared accurately
data$Characteristic.Name<-toupper(data$Characteristic.Name)
#5.make date field a date field
data$SAMPLE_DATE<-as.Date(data$SAMPLE_DATE,format="%m/%d/%Y")

#remove sub tables
rm(list = c('lake','profiles','location','results','sample'))


