library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(ggthemes)
library(gridExtra)
library(padr)
library(data.table)
library(fields)
library(forcats)
library(ggrepel)
library(datasets)
library(caTools)
library(class)
library(scales)
library(tidyverse)
library(plotly)
library(ggfortify)
library(tseries)
library(forecast)
library(DT)

sales_training<-read_csv("sales_train.csv.gz")
sales_test<-read.csv("test.csv.gz")
items_list<-read.csv("items.csv")
items_new<-items_list[,c(2,3)]
sample_submission<-read.csv("sample_submission.csv.gz")

str(items_list)
head(sales_training,20)
head(sales_test,20)
head(items_new,20)
head(sample_submission,20)

str(sales_test)

sales_training <- sales_training %>% left_join(items_new,by=c("item_id"))

date_block_num <- sales_training$date_block_num %>% unique()
unique_shops <- sales_training$shop_id %>% unique()
unique_item_ids <- sales_training$item_id %>% unique()


all_ids <- expand.grid(date_block_num,unique_shops,unique_item_ids)

colnames(all_ids) <- c("date_block_num","shop_id","item_id")

sales_item_cnt_month <- sales_training %>% group_by(date_block_num,shop_id,item_id) %>% summarise(item_cnt_month-sum(item_cnt_day)) %>% ungroup()
























