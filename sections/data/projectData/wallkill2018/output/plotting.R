data<-read.csv("dataqqaqcd.csv")
data$sample_date<-as.Date(data$sample_date,"%m/%d/%Y")
data<-data[data$validator_qualifiers!="R",]

data<-data[data$site=="13-DWAR-2.0"|data$site=="13-LGUN-6.0"|data$site=="13-MASO-0.2"|data$site=="13-MONH-0.4"|data$site=="13-MONH-4.1"|data$site=="13-PKIL-0.4"|data$site=="13-POCH-1.8"|data$site=="13-QKER-0.9"|data$site=="13-RIOG-0.7"|data$site=="13-RUTG-1.5"|data$site=="13-SWAK-1.7"|data$site=="13-TINW-0.5"|data$site=="13-WCHEE-0.6"|data$site=="13-WKLEI-0.6",]
params<-unique(data$chemical_name)
nparams<-length(params)


library(ggplot2)

for(i in 1:nparams){
  temp<-data[data$chemical_name==params[i],]
  plots<-(ggplot(data=temp,
         aes(x=sample_date, y=result_value, group=site, colour=site)) +
    geom_line()+
    ggtitle(params[i]))
  tit<-paste(params[i],'.pdf',sep="")
  pdf(tit)
      print(plots)
      dev.off()
  rm(plots)
  rm(tit)
}