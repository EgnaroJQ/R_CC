library(tidyverse)

#读取txt数据的日期与标题存入df
txt2df <- function(x){
  my_dt <- data.frame(text = readLines(x))
  my_dt <- tibble(my_dt)
  # 去除空行。
  my_dt <- filter(my_dt, text != "")
  # 确认行数。
  my_dt$id <- rep(1:3,nrow(my_dt) / 3)
  
  # 提取题目。
  my_dt_title <- filter(my_dt, id == 3)
  
  # 提取日期。
  my_dt_date <- filter(my_dt, id == 2)
  my_dt_date$date <- substr(my_dt_date$text, 1, 11)
  
  # 组合日期和标题。
  my_res <- data.frame(
    title = my_dt_title$text, 
    date = my_dt_date$date
  )
  my_res <- tibble(my_res)
  return(my_res)
}

my_res <- txt2df("./RawData/ash_ti/2012CC.txt")
class(my_res)

#生成文件名列表
pro_name <- function(yhead, yend,x){#参数为起始年，停止年，关键词简写
  txtlst <- c()
  for (i in (yhead:yend)){
    txtname = paste0("./RawData/ash_ti/", as.character(i),x,".txt")
    txtname
    txtlst <- append(txtlst,txtname)
  }
  return(txtlst)
}
res_CC <- pro_name(2012,2022,"CC")
res_CC

#把文件导出并整合到数据框里
#先建一个df列表
df_lst <- vector("list", 11)
names(df_lst) <- res_CC #给list中的元素（车厢）命名

for (i in res_CC) {
  my_df <- txt2df(i)
  df_lst[[i]] <- my_df #注意 列表单方括号会连“车厢”取出
}
df_lst[[1]]
#把列表拼合起来
ash_cc <- Reduce(rbind, df_lst)
ash_cc

#对GW做相同操作
res_GW <- pro_name(2012,2022,"GW")
res_GW
#整合数据框
GW_lst <- vector("list", 11)
names(GW_lst) <- res_GW #给list中的元素（车厢）命名

for (i in res_GW) {
  my_df <- txt2df(i)
  GW_lst[[i]] <- my_df #注意 列表单方括号会连“车厢”取出
}
GW_lst[[1]]
#把列表拼合起来
ash_gw <- Reduce(rbind, GW_lst)
ash_gw

#数据框ash_gw温暖化129个月标题数据，ash_cc气候变动

#将date中的XX年XX月XX日转化为Date变量
ash_cc$dateday <- gsub("年","-",ash_cc$date)#替换字符串成为标准格式
ash_cc$dateday <- gsub("月","-",ash_cc$dateday)
ash_cc$dateday <- gsub("日","",ash_cc$dateday)

library(lubridate)#用于管理日期时间变量的包
ash_cc$dateday <- lubridate::ymd(ash_cc$dateday)#根据字符串生成变量
class(ash_cc$dateday[1])

ash_gw$dateday <- gsub("年","-",ash_gw$date)#替换字符串成为标准格式
ash_gw$dateday <- gsub("月","-",ash_gw$dateday)
ash_gw$dateday <- gsub("日","",ash_gw$dateday)
ash_gw$dateday <- lubridate::ymd(ash_gw$dateday)#根据字符串生成变量
class(ash_cc$dateday[1])


#加载需要的包
library(PerformanceAnalytics)
library(ggplot2)
library(patchwork)
library(readr)
library(RMeCab)
library(igraph)
library(ggraph)
library(reshape2)
library(showtext)
library(dplyr)

#按日计数
cc_count <- ash_cc %>%
  group_by(dateday) %>%
  summarise(count=n()) %>%
  as.data.frame()
cc_count

gw_count <- ash_cc %>%
  group_by(dateday) %>%
  summarise(count=n()) %>%
  as.data.frame()
gw_count

#按月计数
cc_mcount <- ash_cc %>% 
  group_by(month = lubridate::floor_date(dateday,"month")) %>% 
  summarize(count=n()) %>% 
  as.data.frame()
cc_mcount
gw_mcount <- ash_gw %>% 
  group_by(month = lubridate::floor_date(dateday,"month")) %>% 
  summarize(count=n()) %>% 
  as.data.frame()
gw_mcount

library(tidyr)
#把两个数据合成长表
mjoin <- full_join(cc_mcount,gw_mcount,by = "month") #先按列合并
colnames(mjoin)
mjoin <- mjoin %>%  rename( cc = count.x,gw = count.y)
mjoin
mjoin_long <- pivot_longer(mjoin, cols=c(cc,gw),#转置为长表
                           names_to ="grp",values_to="count")
mjoin_long
#画按月的对比图
library(showtext)#用来解决文字形式的包
showtext::showtext_auto()　#用来显示文字
ggplot(mjoin_long) + 
  geom_col(aes(month, count, fill = grp), position = "dodge") + 
  theme_bw() + #用来设置背景
  theme(legend.position = c(0.02, 0.98),
        legend.justification = c(0,1),
        legend.title = element_blank())+
  scale_fill_discrete(labels = c("気候変動","温暖化")) #修改图例标签

mjoin$cc2gw <- mjoin$cc / mjoin$gw　#计算cc/gw的比例

#合并同时提及的
df_cc_gw <- inner_join(ash_cc,ash_gw)
cc_gw_mcount <- df_cc_gw %>% 
  group_by(month = lubridate::floor_date(dateday,"month")) %>% 
  summarize(count=n()) %>% 
  as.data.frame()

ggplot(cc_gw_mcount) + #画同时提及的数量图
  geom_col(aes(month,count)) +
  theme_bw()

cc_gw_mcount <- left_join(mjoin,cc_gw_mcount,by="month") 
cc_gw_mcount <- cc_gw_mcount %>% rename(both=count) %>% 
  mutate(both2cc = both/cc, both2gw = both/gw) #添加变量
cc_gw_mcount

#画同时提及比cc|gw总数的对比图

#做文本分析
library(readtext)
library(quanteda)
corp_gw <- corpus(ash_gw, text_field = "title") #生成语料库
corp_gw

summary(corp_gw)
#保留汉字、片假名和数字
toks <- tokens(corp_gw)
toks <- tokens_select(toks, "^[０-９ぁ-んァ-ヶー一-龠]+$", valuetype = "regex", padding = TRUE)
min_count <- 10

#汉字假名和数字　
library("quanteda.textstats")
any_col <-  tokens_select(toks, "^[０-９ァ-ヶー一-龠]+$", valuetype = "regex", padding = TRUE) |> 
  textstat_collocations(min_count = min_count)
toks <- tokens_compound(toks, any_col[any_col$z > 3,], concatenator = "")

#作成文本矩阵
gw_dfm <- dfm(toks) %>% 
  dfm_remove("^[ぁ-ん]+$", valuetype = "regex", min_nchar = 2) %>%  
  dfm_trim(min_termfreq = 0.50, termfreq_type = "quantile", max_termfreq = 0.99)

#相对频度
key <- textstat_keyness(gw_dfm)
head(key, 20) |> knitr::kable()

topfeatures(gw_dfm,100) #看词频

#共现网络
library("quanteda.textplots")
library("quanteda.textplots")
feat <- head(key$feature, 50)
gw_fcm<- dfm_select(gw_dfm, feat) |> fcm()
gw_fcm
#可能是标题长度的问题，这个函数似乎是基于相对频度的，相对频度出来的词频很怪
#做不出想要的结果 ——20231204
#size <- sqrt(rowSums(gw_fcm) 
#这里会报错，因为会出现很多0
textplot_network(gw_fcm, min_freq = 0.85, edge_alpha = 0.9, 
                 vertex_size = size / max(size) * 3,
                 vertex_labelfont = if (Sys.info()["sysname"] == "Darwin") "SimHei" else NULL)


#2021年的标题词频
#gw的词频
#取出2021年
ash_gw_21 <- ash_gw %>% filter(year(dateday) == 2021)
#生成语料库
corp_gw_21 <- corpus(ash_gw_21, text_field = "title") 
corp_gw_21

summary(corp_gw_21)
#保留汉字、片假名和数字
toks_21 <- tokens(corp_gw_21)
toks_21 <- tokens_select(toks_21, "^[０-９ぁ-んァ-ヶー一-龠]+$", valuetype = "regex", padding = TRUE)
min_count <- 10 
#min_count是什么意思？定义最小计数吗？td

#汉字假名和数字　
library("quanteda.textstats")
any_col_21 <-  tokens_select(toks_21, "^[０-９ァ-ヶー一-龠]+$", valuetype = "regex", padding = TRUE) |> 
  textstat_collocations(min_count = min_count)
toks_21 <- tokens_compound(toks_21, any_col_21[any_col_21$z > 3,], concatenator = "")

#作成文本矩阵
gw_dfm_21 <- dfm(toks_21) %>% 
  dfm_remove("^[ぁ-ん]+$", valuetype = "regex", min_nchar = 2) %>%  
  dfm_trim(min_termfreq = 0.50, termfreq_type = "quantile", max_termfreq = 0.99)

#看词频
freq_gw_21 <- topfeatures(gw_dfm_21,100) 

#绘制词云
set.seed(100)
gw_dfm_21_trim <- dfm_trim(gw_dfm_21, min_termfreq = 5)
#设置MacOS字体
library(extrafont)
extrafont::font_import(SimHei)
textplot_wordcloud(gw_dfm_21_trim,min_count = 2,
                   rotation = 0.25, max_words = 100,
                   min_size = 0.5, max_size = 2.8,
                   font =  NULL,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

#21年cc的标题词频
#取出2021年
ash_cc_21 <- ash_cc %>% filter(year(dateday) == 2021)
#生成语料库
corp_cc_21 <- corpus(ash_cc_21, text_field = "title") 
corp_cc_21

summary(corp_gw_21)
#保留汉字、片假名和数字
#td：今后要改一下toks21的变量名或者把这团函数化
toks_21 <- tokens(corp_cc_21)
toks_21 <- tokens_select(toks_21, "^[０-９ぁ-んァ-ヶー一-龠]+$", valuetype = "regex", padding = TRUE)
min_count <- 10 
#min_count是什么意思？定义最小计数吗？td

#汉字假名和数字　
library("quanteda.textstats")
any_col_21 <-  tokens_select(toks_21, "^[０-９ァ-ヶー一-龠]+$", valuetype = "regex", padding = TRUE) |> 
  textstat_collocations(min_count = min_count)
toks_21 <- tokens_compound(toks_21, any_col_21[any_col_21$z > 3,], concatenator = "")

#作成文本矩阵
cc_dfm_21 <- dfm(toks_21) %>% 
  dfm_remove("^[ぁ-ん]+$", valuetype = "regex", min_nchar = 2) %>%  
  dfm_trim(min_termfreq = 0.50, termfreq_type = "quantile", max_termfreq = 0.99)

#看词频
freq_cc_21 <- topfeatures(cc_dfm_21,100) 
freq_cc_21
#绘制词云
set.seed(100)
cc_dfm_21_trim <- dfm_trim(cc_dfm_21, min_termfreq = 5)
#设置MacOS字体
textplot_wordcloud(cc_dfm_21_trim,min_count = 2,
                   rotation = 0.25, max_words = 100,
                   min_size = 0.5, max_size = 2.8,
                   font =  NULL,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

tw_count <- read.table(file = "twCount_all.csv",header = TRUE,
                       sep = "," )
tw_count$datey <- tw_count$X %>% substr(1,4)
tw_count$datem <- tw_count$X %>% substr(5,6)
tw_count$dateym <- paste(tw_count$datey, tw_count$datem, sep = "-")

tw_count <- tw_count %>% #管道符写法，作用同上一行
  mutate(dateym = paste(datey, datem, sep = "-"))
tw_count



