
options(scipen =  999)
library(dplyr)
library(readxl)
library(purrr)
library(lubridate)

# 
# comments <- read.csv("comments.csv", header = FALSE, sep = ",")
# colnames(comments) <- c("comment_uid", "comment_id", "comment_username", "comment_time", "comment_textonly", "comment_atname_list")
# 
# comments <- comments[!duplicated(comments),]
# freq_table <- as.data.frame(table(comments$comment_uid))
# #9119 unique users contributed 42053 comments to these Weibos of Roywebar
# summary(freq_table$Freq)
# #Min. 1st Qu.  Median    Mean   3rd Qu.    Max. 
# #1.000   1.000   1.000   4.612   2.000  305.000 
# 
# freq_table_twice <- freq_table[freq_table$Freq>=2,] 
# #3619 out of 9119 contributed 2 or more times 
# 
# active_uid <- as.data.frame(freq_table_twice$Var1)
# write.table(active_uid, file="active_uid.csv",col.names=F, sep=",",quote = FALSE, row.names = F) 
# 
# #######################################################
# ######### below is for importing weibo content ########
# #######################################################
# 
# weibo_content <- read_excel("result.xlsx", sheet = 1, col_names = c("uid", "mid", "timestamp","weibo_text","mentions"), col_types = c("text", "text", "guess","text","text"))
# 
# colnames(weibo_content)
# dim(weibo_content)
# head(weibo_content,5)
# 
# #some errors: There should be only 5 columns, but there are~1000 columns with non-empty values
# #find the indexes of matching values for the function : the cell is not empty
# #which(map_lgl(weibo_content[[10]], function(x) x != ""))
# #1155 2310 2311 2312 2313 2314 2315
# 
# weibo_content2 <- read.csv("result2.csv", header = FALSE, sep = ",")
# colnames(weibo_content2)
# head(weibo_content2,5)
# dim(weibo_content2)
# 
# weibo_content3 <- read.csv("result3.csv", header = FALSE, sep = ",")
# dim(weibo_content3)
# 
# #### define a function df_list to get unique rows
# df_list <- list(weibo_content, weibo_content2, weibo_content3)
# 
# df_list <- lapply(df_list, function(x) {unique(x)})
# unique_weibo1 <- df_list[[1]]
# unique_weibo2 <- df_list[[2]]
# unique_weibo3 <- df_list[[3]]
# #worked. Do the same for other csv files
# 
# weibo_content4 <- read.csv("result4.csv", header = FALSE, sep = ",")
# dim(weibo_content4)
# 
# weibo_content5 <- read.csv("result5.csv", header = FALSE, sep = ",")
# dim(weibo_content5)
# 
# weibo_content6 <- read.csv("result6.csv", header = FALSE, sep = ",")
# dim(weibo_content6)
# 
# df_list <- list(weibo_content4, weibo_content5, weibo_content6)
# unique_weibo4 <- df_list[[1]]
# unique_weibo5 <- df_list[[2]]
# unique_weibo6 <- df_list[[3]]
# 
# ######
# weibo_content7 <- read.csv("result7.csv", header = FALSE, sep = ",")
# dim(weibo_content7)
# 
# weibo_content8 <- read.csv("result8.csv", header = FALSE, sep = ",")
# dim(weibo_content8)
# 
# weibo_content9 <- read.csv("result9.csv", header = FALSE, sep = ",")
# dim(weibo_content9)
# 
# ###
# df_list <- list(weibo_content7, weibo_content8, weibo_content9)
# unique_weibo7 <- df_list[[1]]
# unique_weibo8 <- df_list[[2]]
# unique_weibo9 <- df_list[[3]]
# 
# ####
# weibo_content10 <- read.csv("result10.csv", header = FALSE, sep = ",")
# dim(weibo_content10)
# 
# weibo_content11 <- read.csv("result11.csv", header = FALSE, sep = ",")
# dim(weibo_content11)
# 
# df_list <- list(weibo_content10, weibo_content11)
# unique_weibo10 <- df_list[[1]]
# unique_weibo11 <- df_list[[2]]
# ###
# all_weibo <- rbind(unique_weibo1, unique_weibo2, unique_weibo3, unique_weibo4, unique_weibo5, unique_weibo6,
#                    unique_weibo6, unique_weibo7, unique_weibo8, unique_weibo9, unique_weibo10, unique_weibo11)
# 
# # the output
# dim(all_weibo)
# #[1] 816305      5
# colnames(all_weibo) <- c("uid","mid","timestamp","weibo_text","mention")
# write.csv(all_weibo, file = "all_weibo.csv")

###remove zhuan fa wei bo转发微博＃＃＃

library(dplyr)
all_weibo <- read.csv("all_weibo.csv", header = TRUE, sep = ",", quote = "")
colnames(all_weibo) <- c("ID","uid","mid","timestamp","weibo_text","mention")
weibo <- all_weibo %>%  filter( weibo_text !='转发微博')
weibo <- weibo %>% mutate_if(is.factor, as.character)
Encoding(weibo[[5]]) <- "UTF-8"
Encoding(weibo[[6]]) <- "UTF-8"

#####remove special characters like "//@中华牙膏官微:"

removeSpecialChar <- function(weibo_str) {
  weibo_str <- gsub(pattern = "TFBOYS超话", "", weibo_str)
  weibo_str <- gsub(pattern = "王源超话", "", weibo_str)
  weibo_str <- gsub(pattern ="//", "" , weibo_str)
  weibo_str <- gsub(pattern="#","", weibo_str )
  weibo_str <- sub(pattern = "@.*?:", "", weibo_str)
}

weiboclean <- weibo %>% mutate(removeSpecialChar(weibo_text))
names(weiboclean)[7] <- "clean_weibotext" 

### handle date time in the format of ymd_hm from lubricate package
### the event exploded in May 21th morning 10:29am, with pictures showing the dinner at 5.20 night

weiboclean<- weiboclean %>% 
  filter(ymd_hm(timestamp) <= '2019-05-27 20:30:00 UTC') %>%
  mutate(timestamp = case_when(
    ymd_hm(timestamp) <= "2019-05-21 10:28:00 UTC"  ~ "before",
    ymd_hm(timestamp) >   "2019-05-21 10:28:00 UTC" ~ "after"
  ))

table(weiboclean$timestamp)
###  after before 
###  290728 221084 

write.csv(weiboclean, file = "weiboclean.csv",row.names=FALSE)
# textonly <- weiboclean[,c(2,6)]
# write.csv(textonly, file = "textonly.csv")
# head(textonly)

weiboclean <- read.csv("/home/rstudio/roywang_sinaweibo/weiboclean.csv")
head(weiboclean$weibo_text,40)

#### load demo csv files with "mention" and "mention_num" April 09
demo <- read.csv("demo.csv", header = T)
demo2 <- read.csv("demo2.csv", header = T)
demo3 <- read.csv("demo3.csv", header = T)
data0409 <- rbind(demo, demo2, demo3)
t.test(data0409$mention_num ~ data0409$timestamp)
