

sample.df <- samples %>% 
  mutate(sys_sample_code = ifelse(sys_sample_code == "18LIS053", paste0(sys_sample_code, sample_date), as.character(sys_sample_code)))
match.df = EBs
match.col = "blank"
param.i <- "ALKALINITY, TOTAL (AS CaCO3)"
site.i <- "18LIS047"
site.i <- "18LIS053"
bind_nearest_date <- function(sample.df, match.df, match.col) {
  
  final.df <- lapply(unique(match.df$chemical_name), function(param.i) {
    print(param.i)
    sample.param.i <- sample.df[sample.df$chemical_name == param.i, ]
    match.param.i <- match.df[match.df$chemical_name == param.i,]
    
    site.df <- lapply(unique(sample.param.i$sys_sample_code), function(site.i) {
      print(site.i)
      sample.sub <- sample.param.i[sample.param.i$sys_sample_code == site.i, ]
      match.param.i$abs <- abs(match.param.i$sample_date - sample.sub$sample_date) 
      match.param.i$min <- min(abs(match.param.i$sample_date - sample.sub$sample_date))
      match.param.i <- match.param.i[which(match.param.i$abs == match.param.i$min), ]
      sample.sub[, match.col] <- match.param.i[, match.col][1]
      return(sample.sub)
    }) %>% 
      dplyr::bind_rows()
    
  }) %>% 
    dplyr::bind_rows() 
  
  return(final.df)
}

dataset <- bind_nearest_date(sample.df = samples,
                             match.df = EBs,
                             match.col = "blank")

params<-unique(EBs$chemical_name)
nparams<-length(params)
i <- 1
j <- 135
for(i in 1:nparams){
  temp<-samples[samples$chemical_name==params[i],]
  tempb<-EBs[EBs$chemical_name==params[i],]
  tempb$sample_date<-as.Date(tempb$sample_date,"%m/%d/%Y")
  sites<-unique(temp$sys_sample_code)
  nsites<-length(sites)
  for(j in 1:nsites){
    temp1<-temp[temp$sys_sample_code==sites[j],]
    temp1$sample_date<-as.Date(temp1$sample_date,"%m/%d/%Y")
    tempb1<-tempb[which(abs(tempb$sample_date-temp1$sample_date)==min(abs(tempb$sample_date-temp1$sample_date))),]
    temp1$blank<-tempb1$blank[1]
    if(exists("dataset")){
      dataset<-merge(dataset,temp1,all=TRUE)
    }
    if(!exists("dataset")){
      dataset<-temp1
    }
    rm(list=c('temp1','tempb1'))
  }
  rm(list=c('temp','tempb','sites','nsites','j'))
}