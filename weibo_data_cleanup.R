
options(scipen =  200)
library(dplyr)
library(readxl)
library(purrr)
library(lubridate)


comments <- read.csv("comments.csv", header = FALSE, sep = ",")
colnames(comments) <- c("comment_uid", "comment_id", "comment_username", "comment_time", "comment_textonly", "comment_atname_list")

comments <- comments[!duplicated(comments),]
freq_table <- as.data.frame(table(comments$comment_uid))
#9119 unique users contributed 42053 comments to these Weibos of Roywebar
summary(freq_table$Freq)
#Min. 1st Qu.  Median    Mean   3rd Qu.    Max. 
#1.000   1.000   1.000   4.612   2.000  305.000 

freq_table_twice <- freq_table[freq_table$Freq>=2,] 
#3619 out of 9119 contributed 2 or more times 

active_uid <- as.data.frame(freq_table_twice$Var1)
write.table(active_uid, file="active_uid.csv",col.names=F, sep=",",quote = FALSE, row.names = F) 

#######################################################
######### below is for importing weibo content ########
#######################################################

weibo_content <- read_excel("result.xlsx", sheet = 1, col_names = c("uid", "mid", "timestamp","weibo_text","mentions"), col_types = c("text", "text", "guess","text","text"))
colnames(weibo_content)
dim(weibo_content)
head(weibo_content,5)
weibo_content2 <- read_excel("result2.xlsx", sheet = 1, col_names = c("uid", "mid", "timestamp","weibo_text","mentions"), col_types = c("text", "text", "guess","text","text"))
weibo_content3 <- read_excel("result3.xlsx", sheet = 1, col_names = c("uid", "mid", "timestamp","weibo_text","mentions"), col_types = c("text", "text", "guess","text","text"))
weibo_content4 <- read_excel("result4.xlsx", sheet = 1, col_names = c("uid", "mid", "timestamp","weibo_text","mentions"), col_types = c("text", "text", "guess","text","text"))
weibo_content5 <- read_excel("result5.xlsx", sheet = 1, col_names = c("uid", "mid", "timestamp","weibo_text","mentions"), col_types = c("text", "text", "guess","text","text"))
weibo_content6 <- read_excel("result6.xlsx", sheet = 1, col_names = c("uid", "mid", "timestamp","weibo_text","mentions"), col_types = c("text", "text", "guess","text","text"))
weibo_content7 <- read_excel("result7.xlsx", sheet = 1, col_names = c("uid", "mid", "timestamp","weibo_text","mentions"), col_types = c("text", "text", "guess","text","text"))
weibo_content8 <- read_excel("result8.xlsx", sheet = 1, col_names = c("uid", "mid", "timestamp","weibo_text","mentions"), col_types = c("text", "text", "guess","text","text"))
weibo_content9 <- read_excel("result9.xlsx", sheet = 1, col_names = c("uid", "mid", "timestamp","weibo_text","mentions"), col_types = c("text", "text", "guess","text","text"))
weibo_content10 <- read_excel("result10.xlsx", sheet = 1, col_names = c("uid", "mid", "timestamp","weibo_text","mentions"), col_types = c("text", "text", "guess","text","text"))
weibo_content11 <- read_excel("result11.xlsx", sheet = 1, col_names = c("uid", "mid", "timestamp","weibo_text","mentions"), col_types = c("text", "text", "guess","text","text"))


#### define a function df_list to get unique rows
df_list <- list(weibo_content, weibo_content2, weibo_content3, weibo_content4, weibo_content5, weibo_content6, weibo_content7, weibo_content8)

df_list <- lapply(df_list, function(x) {unique(x)})

unique_weibo1 <- df_list[[1]]
unique_weibo2 <- df_list[[2]]
unique_weibo3 <- df_list[[3]]
unique_weibo4 <- df_list[[4]]
unique_weibo5 <- df_list[[5]]
unique_weibo6 <- df_list[[6]]
unique_weibo7 <- df_list[[7]]
unique_weibo8 <- df_list[[8]]

###
df_list <- list(weibo_content9, weibo_content10, weibo_content11)

unique_weibo9 <- df_list[[1]]
unique_weibo10 <- df_list[[2]]
unique_weibo11 <- df_list[[3]]

###
all_weibo <- rbind(unique_weibo1, unique_weibo2, unique_weibo3, unique_weibo4, unique_weibo5, unique_weibo6,
                   unique_weibo6, unique_weibo7, unique_weibo8, unique_weibo9, unique_weibo10, unique_weibo11)

# the output
dim(all_weibo)
#[1] 740557      5
colnames(all_weibo) <- c("uid","mid","timestamp","weibo_text","mention")
write.csv(all_weibo, file = "all_weibo.csv")
all_weibo <- read.csv("all_weibo.csv", header = TRUE, sep = ",", quote = "")

#################
## textual data clean-up
#################

library(dplyr)
#organizing
weibo <- all_weibo %>% mutate_if(is.factor, as.character)
Encoding(weibo[[4]]) <- "UTF-8"
Encoding(weibo[[5]]) <- "UTF-8"

###remove zhuan fa wei bo转发微博＃＃＃
weibo <- all_weibo %>%  filter( weibo_text !='转发微博' & weibo_text !='Repost') # down to 632645 rows

#####remove special characters like "//@中华牙膏官微:"

removeSpecialChar <- function(weibo_str) {
  weibo_str <- gsub(pattern = "TFBOYS超话", "", weibo_str)
  weibo_str <- gsub(pattern = "王源超话", "", weibo_str)
  weibo_str <- gsub(pattern ="//","" , weibo_str)
  weibo_str <- gsub(pattern="#","", weibo_str )
  weibo_str <- sub(pattern = "@.*?:", "", weibo_str)
}

weiboclean <- weibo %>% mutate(removeSpecialChar(weibo_text))
names(weiboclean)[6] <- "clean_weibotext" 
weiboclean <- unique(weiboclean) #down to 453867

### handle date time in the format of ymd_hm from lubricate package
### the scandale exploded in 5.21 morning 10:29, with pictures showing the dinner at 5.20 night

weiboclean<- weiboclean %>% 
  filter(ymd_hm(timestamp) <= '2019-05-27 20:30:00 UTC') %>%
  mutate(timestamp = case_when(
    ymd_hm(timestamp) <= "2019-05-21 10:28:00 UTC"  ~ "before",
    ymd_hm(timestamp) >   "2019-05-21 10:28:00 UTC" ~ "after"
  ))

### another version with raw timestamp ##
#########################################
# a <- weiboclean[,c('timestamp','mid')]
#write.csv(a, file = 'mid_rawtimestamp.csv')

table(weiboclean$timestamp) #some rows are filtered out, due to observation time period is ending before the calendar day
#after before 
#238305 186843 

write.csv(weiboclean, file = "weiboclean_with_date.csv")
textonly <- weiboclean[,c(2,6)] #only mid and clean text, 2 columns
write.csv(textonly, file = "weiboclean_only_mid_text.csv")
head(textonly)

###############
## compare with current sentiment score results
## find the mids + text that needs evaluation
######################
wb_sentiment <- read.csv("wb_sentiment.csv")  #448679
wb_sentiment <- unique(wb_sentiment) #448379 rows have been evaluated
wb_sentiment$mid <- as.character(wb_sentiment$mid)

mid_need_sentimentscore <- as.data.frame(setdiff(textonly$mid, wb_sentiment$mid)) #5135 rows need evaluation
colnames(mid_need_sentimentscore)[1] <- 'mid'
a <- semi_join(textonly, mid_need_sentimentscore, by ='mid')
write.csv(a, file = "mid_need_sentimentscore.csv")
