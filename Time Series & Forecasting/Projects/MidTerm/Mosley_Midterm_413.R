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

# read in each dataset
sales_training<-read_csv("sales_train.csv.gz")
sales_test<-read.csv("test.csv.gz")
shops<-read.csv("shops.csv")
items<-read.csv("items.csv")
item_category<-read.csv("item_categories.csv")
items2 <- items[,c(2,3)]
sample_submission<-read.csv("sample_submission.csv.gz")

head(sales_training,20)
head(shops,20)
head(item_category,20)
head(items2,20)
head(sample_submission,20)

str(sales_training)
str(shops)
str(item_category)
str(items2)
str(sample_submission)

sales_training <- sales_training %>% left_join(items, by='item_id') %>% left_join(shops, by='shop_id') %>% left_join(item_category, by='item_category_id')

# review summary of joined dataset
head(sales_training)
tail(sales_training)

# review each dataset
dim(sales_training)
dim(shops)
dim(item_category)
dim(items2)
dim(sample_submission)


# add year/day/weekday/month to sales_training
sales_training$date<-dmy(sales_training$date)
sales_training$year <- year(sales_training$date)
sales_training$day <- day(sales_training$date)
sales_training$weekday <- weekdays(sales_training$date)
sales_training$month <- month(sales_training$date)
head(sales_training)

# review yearly sales 2013-2015
year_sales <- sales_training %>% group_by(year) %>% summarise(total_sale = sum(item_price))
year_sales

# plot yearly sales 2013-2015
ggplot(data=year_sales,aes(x=year,y=total_sale, fill=as.factor(year))) + 
  geom_bar(stat="identity") +
  labs(title = "Yearly Total Sales", x="Year", y="Total Sales P/Y",fill="Year") +
  scale_fill_manual("legend", values = c("2013" = "red", "2014" = "white", "2015" = "blue"))

# review monthly sales
monthly_sales <- sales_training %>% group_by(year, month) %>% summarise(total_monthly_sales = sum(item_cnt_day*item_price)) %>% arrange(year, month)
monthly_sales$YearMonth <- paste(monthly_sales$year,sep="-",monthly_sales$month)
head(monthly_sales)
tail(monthly_sales)

#monthly option 2
#sales_item_cnt_mth<-sales_training %>% group_by(year, month) %>% summarise(item_cnt_month - sum(item_cnt_day)) %>% ungroup()

# plot monthly sales 2013-2015
ggplot(data = monthly_sales, aes(x = YearMonth, y = total_monthly_sales, fill = as.factor(YearMonth))) + 
  geom_bar(stat = "identity") + 
  theme(text = element_text(size=9),axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = "Monthly Total Sales", x = "YYYY-M", y = "Monthly Total Sales P/Y", fill = "YYYY-M")

# finding which day of each month had the highest sales per year
highest_day_sales_monthly <- sales_training %>% group_by(year,month,day,weekday) %>% summarise(sales_per_day = sum(item_price * item_cnt_day)) %>% arrange(desc(sales_per_day))
head(highest_day_sales_monthly)
tail(highest_day_sales_monthly)

# plot highest day sales per month 2013-2015
ggplot(highest_day_sales_monthly,aes(day,sales_per_day,fill=as.factor(day)))+
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(min(1),max(31),by=1))+
  facet_wrap( ~ year,ncol=3) +
  theme(text = element_text(size=9),
        axis.text.x=element_text(angle=90,hjust=1)) +
  coord_flip() +
  labs(title="Daily Sales", x="Day of Month", y="Total Sales",fill="Day" )

# plot weekday with highest sales
ggplot(highest_day_sales_monthly,aes(weekday, sales_per_day,fill=as.factor(weekday)))+
  geom_bar(stat="identity") +
  theme(text = element_text(size=9),
        axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title="Weekday Sales", x="Day of Week", y="Total Sales",fill="Weekday" )

# review shops with highest sales
shop_sales <- sales_training %>% group_by(shop_id,year) %>% summarise(total_sale = sum(item_cnt_day)) %>% arrange(desc(total_sale))
head(shop_sales)
tail(shop_sales)

# plot shops with highest sales
ggplot(data=shop_sales,aes(x=reorder(as.factor(shop_id),total_sale),y= total_sale, fill=as.factor(shop_id)))+
  geom_bar(stat="identity",width=1.5) +
  theme(text = element_text(size=9),
        axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = "Shops-Highest Sales", x = "Shop ID", y = "Total Sales", fill = "Shop ID")

# review shops with most items
max_items <- sales_training %>% group_by(shop_id) %>% summarise(items_per_shop = n_distinct(item_id)) %>% arrange(desc(items_per_shop))
head(max_items)
tail(max_items)

#plot shops with most items
ggplot(data = max_items, aes(x = reorder(as.factor(shop_id), items_per_shop), y = items_per_shop, fill = as.factor(shop_id))) + 
  geom_bar(stat="identity") +
  theme(text = element_text(size=9),
        axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = "Shops-Most Items", x = "Shops", y = "Total # Items Per Shop", fill = 'Shop ID')

# review most frequent item bought per shop
sales_training <- sales_training %>% left_join(items, by = c("item_id"))
frequently_bought <- sales_training %>% group_by(shop_id, item_id) %>% summarise(max_sold = sum(item_cnt_day)) %>% filter(max_sold == max(max_sold)) %>% arrange(desc(max_sold))
head(frequently_bought)
tail(frequently_bought)

# plot most frequently bought items per shop
ggplot(data = frequently_bought, aes(x = reorder(as.factor(shop_id), max_sold), y = max_sold, fill = as.factor(item_id))) + 
  geom_bar(stat="identity") +
  theme(text = element_text(size=9), axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = "Frequently Bought Items", x = "Shops", y = "Mold Items Sold",fill="Item ID")

# top 10 highest sold products - all shops
top_selling_items <- sales_training %>% group_by(item_category_id.x) %>% summarise(high_sales = sum(item_price * item_cnt_day)) %>% arrange(desc(high_sales))

# plot top 10 highest sold products - all shops
ggplot(data = top_selling_items[1:10,], aes(x = reorder(as.factor(item_category_id.x), high_sales), y = high_sales, fill = as.factor(item_category_id.x))) + 
  geom_bar(stat="identity") +
  theme(text = element_text(size=10), axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = "Top 10 Product Sold - All Shops", x = "Item Category ID's", y = "High Sales", fill = "Item Category ID")

# top 10 most sold products within each prod category
topsold_prodcat <- sales_training %>% group_by(item_category_id.x, item_id) %>% summarise(high_sell = sum(item_price * item_cnt_day)) %>% filter(high_sell == max(high_sell)) %>% arrange(desc(high_sell))
topsold_prodcat[1:10,]

# plot top 10 most sold products within each category
ggplot(data = topsold_prodcat[1:10,], aes(x = reorder(as.factor(item_category_id.x), high_sell), y = high_sell, fill = as.factor(item_id))) + 
  geom_bar(stat="identity") +
  theme(text = element_text(size=10), axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = "Top 10 Sold Product Category", x = "Item Category ID's", y = "Total Sales", fill = "Item ID")

# Models - frequently bought items
#pop_shop_ts <- subset(frequently_bought,shop_id==31 & item_id==20949,select=c(item_cnt_day,date))
frequently_bought_ts = ts(frequently_bought[,1], frequency = 12, start=c(2013,01,01), end = c(2015,10,31))
autoplot(frequently_bought_ts, geom = "line", ylab = "Item 20949 Sales", xlab = "Year", main = "Sales of Item 20949/Shop 31/2013-2015")

#plot.ts(frequently_bought_ts)
frequentlyBoughtSES <- HoltWinters(frequently_bought_ts, beta = FALSE, gamma = FALSE)
frequentlyBoughtSES
frequentlyBoughtSES$fitted
plot(frequentlyBoughtSES)

model1 <- ets(frequently_bought_ts)
accuracy(model1)

model2 <- ets(frequently_bought_ts, model = "ZZZ")
accuracy(model2)

model3 <- auto.arima(frequently_bought_ts)
accuracy(model3)

fit <- ets(frequently_bought_ts, model = "ZZZ")
forecast(fit, h=12)

year_sales_ts = ts(year_sales[,], frequency = 12, start(2013))
autoplot(year_sales_ts) + ggtitle("Yearly Sales 2013-2015") + ylab("Sales") + xlab("Year")
#ggplot(year_sales,aes(x="Year", y="Sales")) + geom_line()

monthly_sales_ts = ts(monthly_sales[,1], frequency = 1, start=c(2013,01,01))
autoplot(monthly_sales_ts) + ggtitle("Monthly Sales 2013-2015") + ylab("Sales") + xlab("Month")

# autoplot(sales_training) + 
#   autolayer(meanf(sales_training, h=11),
#             series="Mean", PI=FALSE) +
#   autolayer(naive(sales_training, h=11),
#             series="Naïve", PI=FALSE) +
#   autolayer(snaive(sales_training, h=11),
#             series="Seasonal naïve", PI=FALSE)

highest_day_sales_monthly_ts <- ts(highest_day_sales_monthly)
plot.ts(highest_day_sales_monthly_ts)

shop_sales_ts <- ts(shop_sales)
plot.ts(shop_sales_ts)

shopSalesSES <- HoltWinters(shop_sales_ts, beta = FALSE, gamma = FALSE)
shopSalesSES
shopSalesSES$fitted
plot(shopSalesSES)

top_selling_items_ts <- ts(top_selling_items)
plot.ts(top_selling_items_ts)
topSellingSES <- HoltWinters(top_selling_items_ts, beta = FALSE, gamma = FALSE)
topSellingSES
topSellingSES$fitted

