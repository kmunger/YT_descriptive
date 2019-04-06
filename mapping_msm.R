library(tidyverse)
library(reshape2)
setwd("C:/Users/kevin/Documents/GitHub/YT_mapping_replication/")



#meta<-read.csv("data/vid_meta_msm.csv", header = T, stringsAsFactors = F)

###NOTE: I already ran this to make the file small enough for github, the youtube API returns more fields that we don't need
##and some videos without the metadata we need (170k of the original 990k are dropped)



##not sure why this field was returned as a character
#meta$video_view_count <- as.numeric(meta$video_view_count)


#meta<-filter(meta, is.na(meta$video_view_count) == F & is.na(meta$video_like_count) == F & is.na(meta$video_dislike_count) == F &
 #        is.na(meta$video_comment_count) == F )


####drop duplicated video_id's -- lose 2k vids here

#meta<-meta %>% distinct(video_id, .keep_all = TRUE)

##I ran this scraper midway through November 2018, so it doesn't count as a full month (lose 30k vids here)
#meta$month<-as.Date(paste(substr(meta$video_publish_date, 1, 7),"-01",sep=""))


#meta<-filter(meta, month >= "2006-08-01" & month < "2018-11-01")



#meta<-select(meta, video_view_count, video_like_count , video_comment_count , month)


#write.csv(file ="data/vid_meta_msm_preprocessed.csv", x = meta)

meta<-read.csv("data/vid_meta_msm_preprocessed.csv", header = T, stringsAsFactors = F)






sum_likes<-aggregate(video_like_count ~ month, data = meta,
                     sum)




pdf(file = "sum_likes_msm.pdf", width = 8, height = 5)

qplot( sum_likes$month, sum_likes$video_like_count, geom = 'smooth' )+ theme(
  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  axis.line.x = element_line(color = "grey")
  ) +  ylab("Sum Likes per Month") + ylim(0,NA) +ggtitle("MSM Likes Per Month ")


dev.off()





sum_views<-aggregate(video_view_count ~ dates, data = meta,
                      sum)


pdf(file = "sum_views_msm.pdf", width = 8, height = 5)


qplot( sum_views$dates, sum_views$video_view_count, geom = 'smooth' ) + theme(
  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  axis.line.x = element_line(color = "grey")
) + xlab("Date") + ylab("Sum Views per Month") + ylim(0,NA) +ggtitle("MSM Views Per Month ")


dev.off()


###plot videos by month

vids_msm<-table(meta$month)


##topcode for clarity --- these peaks are *really* high though (40k), not sure what's going on

vids_msm[vids_msm>15000]<-15000

pdf(file = "vids_msm.pdf", width = 8, height = 5)


plot(vids_msm_short,  ylab = "Videos Uploaded per Month", main = "Videos by MSM")
dev.off()

