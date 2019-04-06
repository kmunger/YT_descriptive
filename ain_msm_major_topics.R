
  
 library(tidyverse)
  library(reshape2)
setwd("C:/Users/kevin/Documents/GitHub/YT_mapping_replication/")
  
#economy

  yt_economy<-read.csv("data/youtube_economy_100.csv", header = T, stringsAsFactors = F)
  yt_economy$month<-substr(yt_economy$video_publish_date, 1, 7)

  
  ###news
  yt_news<-read.csv("data/youtube_news_100.csv", header = T, stringsAsFactors = F)
  
  yt_news$month<-substr(yt_news$video_publish_date, 1, 7)
  
  
  
  
  ###politics 
  yt_politics<-read.csv("data/youtube_politics_100.csv", header = T, stringsAsFactors = F)
  
  yt_politics$month<-substr(yt_politics$video_publish_date, 1, 7)



#####now  AIN // MSM channels 

ain<-read.csv(file = 'data/AIN_channel_stats.csv', header = TRUE, stringsAsFactors = F)

msm<-read.csv(file = 'data/media_yt.csv',  header = TRUE, stringsAsFactors = F)

####


##restrict to matches

yt_economy_ain<-filter(yt_economy, channel_id %in% ain$id)
yt_economy_msm<-filter(yt_economy, channel_id %in% msm$channel_id)

yt_news_ain<-filter(yt_news, channel_id %in% ain$id)
yt_news_msm<-filter(yt_news, channel_id %in% msm$channel_id)


yt_politics_ain<-filter(yt_politics, channel_id %in% ain$id)
yt_politics_msm<-filter(yt_politics, channel_id %in% msm$channel_id)



###initialize

economy_ain<-vector()
economy_msm<-vector()
news_msm<-vector()
news_ain<-vector()
politics_msm<-vector()
politics_ain<-vector()

##for each month

months<-unique(yt_news$month)

for(i in 1:length(months)){
  
  
  apropos<-filter(yt_economy_ain, month == months[i])
  economy_ain[i]<-length(apropos$channel_title)
  
  apropos<-filter(yt_economy_msm, month == months[i])
  economy_msm[i]<-length(apropos$channel_title)
  
  apropos<-filter(yt_news_ain, month == months[i])
  news_ain[i]<-length(apropos$channel_title)
  
  apropos<-filter(yt_news_msm, month == months[i])
  news_msm[i]<-length(apropos$channel_title)
  
  
  apropos<-filter(yt_politics_ain, month == months[i])
  politics_ain[i]<-length(apropos$channel_title)
  
  apropos<-filter(yt_politics_msm, month == months[i])
  politics_msm[i]<-length(apropos$channel_title)
  
  
}




alldens<-data.frame(months, news_msm, news_ain, politics_msm, politics_ain,
                    economy_msm, economy_ain)


##meld data
alldens.m = melt(alldens, id.vars ="months", measure.vars = c("news_msm","politics_msm","economy_msm","news_ain", "politics_ain",
                                                              "economy_ain"))

pdf("figures/frequency_major_topics.pdf", 7, 5)

ggplot(alldens.m, aes(y=value, x=months, group=variable, color = variable)) +
  geom_smooth(se = F, span = .2, size = 2)+ scale_x_discrete(breaks=c("2008-01","2009-01","2010-01", 
                                                                      "2011-01", "2012-01", "2013-01",
                                                                      "2014-01", "2015-01", "2016-01",
                                                                      "2017-01", "2018-01"))+
  geom_point() + 
  xlab(NULL) + ylab("Frequency")  + theme_bw() + ggtitle("Videos in the Top 100 per Month: Major Topics") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, 
        size = 14), legend.title = element_blank())

dev.off()


