  library(streamR)
  library(Rcpp)
  library(ROAuth)
  library(quanteda)
  library(smappR)
  library(dplyr)
  library(lubridate)
  library(brms)
  library(ggplot2)
  library(lubridate)
  
 library(tidyverse)
  library(reshape2)
setwd("C:/Users/kevin/Documents/GitHub/YT_mapping_replication/")
  
#social justice

  yt_sj<-read.csv("data/youtube_sj_100.csv", header = T, stringsAsFactors = F)
  yt_sj$month<-substr(yt_sj$video_publish_date, 1, 7)

  
  ###feminism
  yt_feminism<-read.csv("data/youtube_feminism_100.csv", header = T, stringsAsFactors = F)
  
  yt_feminism$month<-substr(yt_feminism$video_publish_date, 1, 7)
  
  
  
  
  ###white genocide
  yt_white<-read.csv("data/youtube_white_100.csv", header = T, stringsAsFactors = F)
  
  yt_white$month<-substr(yt_white$video_publish_date, 1, 7)



#####now  AIN // MSM channels 

ain<-read.csv(file = 'data/AIN_channel_stats.csv', header = TRUE, stringsAsFactors = F)

msm<-read.csv(file = 'data/media_yt.csv',  header = TRUE, stringsAsFactors = F)

####


##restrict to matches

yt_sj_ain<-filter(yt_sj, channel_id %in% ain$id)
yt_sj_msm<-filter(yt_sj, channel_id %in% msm$channel_id)

yt_feminism_ain<-filter(yt_feminism, channel_id %in% ain$id)
yt_feminism_msm<-filter(yt_feminism, channel_id %in% msm$channel_id)


yt_white_ain<-filter(yt_white, channel_id %in% ain$id)
yt_white_msm<-filter(yt_white, channel_id %in% msm$channel_id)



###initialize

sj_ain<-vector()
sj_msm<-vector()
feminism_msm<-vector()
feminism_ain<-vector()
white_msm<-vector()
white_ain<-vector()

##for each month

months<-unique(yt_feminism$month)

for(i in 1:length(months)){
  
  
  apropos<-filter(yt_sj_ain, month == months[i])
  sj_ain[i]<-length(apropos$channel_title)
  
  apropos<-filter(yt_sj_msm, month == months[i])
  sj_msm[i]<-length(apropos$channel_title)
  
  apropos<-filter(yt_feminism_ain, month == months[i])
  feminism_ain[i]<-length(apropos$channel_title)
  
  apropos<-filter(yt_feminism_msm, month == months[i])
  feminism_msm[i]<-length(apropos$channel_title)
  
  
  apropos<-filter(yt_white_ain, month == months[i])
  white_ain[i]<-length(apropos$channel_title)
  
  apropos<-filter(yt_white_msm, month == months[i])
  white_msm[i]<-length(apropos$channel_title)
  
  
}




alldens<-data.frame(months, feminism_msm, feminism_ain, white_msm, white_ain,
                    sj_msm, sj_ain)


##meld data
alldens.m = melt(alldens, id.vars ="months", measure.vars = c("feminism_msm","white_msm","sj_msm","feminism_ain", "white_ain",
                                                              "sj_ain"))

pdf("figures/frequency_niche_topics.pdf", 7, 5)

ggplot(alldens.m, aes(y=value, x=months, group=variable, color = variable)) +
  geom_smooth(se = F, span = .2, size = 2)+ scale_x_discrete(breaks=c("2008-01","2009-01","2010-01", 
                                                                      "2011-01", "2012-01", "2013-01",
                                                                      "2014-01", "2015-01", "2016-01",
                                                                      "2017-01", "2018-01"))+
  geom_point() + 
  xlab(NULL) + ylab("Frequency")  + theme_bw() + ggtitle("Videos in the Top 100 per Month: Niche Topics") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, 
        size = 14), legend.title = element_blank())

dev.off()


