train <- read.csv("train.csv", header= T)

#missing pattern
library(mice)
md.pattern(train)

library(VIM)
windows()
aggr(train,prop=FALSE,numbers=TRUE) 

#類別的行數
ncate <- sort(c(a, ))
a <- c(2, 7, 8, 11, 12, 153, 119, 115, 107, 41, 40, 39, 38, 37, 36, 35, 34, 
       30, 13, 85, 100, 103, 114, 117, 121, 123, 187, 156, 158, 160, 179, 181, 
       202, 204, 206, 225, 227, 229, 248, 250, 252, 271, 273, 275)
b <- seq(from=188, to=199, by= 1)
c <- seq(from=69, to=84, by=1)
d <- seq(from=164, to=176, by=1)
e <- seq(from=210, to=222, by=1)
f <- seq(from=233, to=245, by=1)
g <- seq(from=256, to=268, by=1)
h <- seq(from=279, to=291, by=1)
no.cate <- sort(c(a, b, c, d, e, f, g, h))
#類別中不是計數資料的行數
cate.factor <- c(2, 7, 8, 11, 12, 153, 119, 115, 107, 41, 
                 40, 39, 38, 37, 36, 35, 34, 30, 13)

#類別
cate <- train[, no.cate]
aggr(cate, prop=TRUE, numbers=TRUE)
train$state <- as.factor(train$state)
train$material <- as.factor(train$material)
levels(train$material)

#連續
cont <- train[, -no.cate]
windows()
aggr(cont, prop=TRUE, numbers=TRUE) 
colnames(cont)



#distribution of price
qqnorm(train$price_doc)
qqline(train$price_doc)

#以area為分類,對log(price)作圖
ggplot(train)+   
  geom_boxplot(aes(x= sub_area, y=log(price_doc))) +
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 5, angle = 90)) +
  labs(title = "不同地區log(價格)", x = "log(價格)", y = "地區") 
boxplot(log(train$price_doc))


#年月成交量
library(magrittr)
train$timestamp <- as.Date(train$timestamp)
train$ym <- format(train$timestamp, "%Y-%m")
volume <- table(train$ym)
mon.day <- seq.Date(from = as.Date("2011/08/01", format="%Y/%m/%d"), by="month",  length.out=length(volume))
plot(x= mon.day, y= volume, type= "o", pch="", 
     col=2, lwd= 2, main="年月成交量", ylab="成交量", xlab="年月")


#scatter plot of price and train distace
##zd_vokzaly_avto_km:Distance to train station
##ID_railroad_terminal:最近的火車終點站
dis1 <- ggplot(data= train, aes(x= log(price_doc), y=zd_vokzaly_avto_km)) +
  geom_point(aes(colour=as.factor(ID_railroad_terminal)), position="jitter") +
  labs(title = "價格與火車站距離的關係", x = "log(價格)", y = "與火車站距離") +
  guides(colour = guide_legend("最近的火車終點站"))
       

#scatter plot of price and bus distace
##bus_terminal_avto_km:Distance to bus station
##ID_bus_terminal:最近的公車終點站
dis2 <- ggplot(data= train, aes(x= log(price_doc), y=bus_terminal_avto_km)) +
  geom_point(aes(colour=as.factor(ID_bus_terminal)), position="jitter") +
  labs(title = "價格與公車站距離的關係", x = "log(價格)", y = "與公車站距離") +
  guides(colour = guide_legend("最近的公車終點站"))
windows()
cowplot::plot_grid(dis1, dis2, labels = "AUTO", ncol=1)
#時間與用途 
##timestamp/ product_type
library(scales)
library(ggplot2)
dateway <- train[, c(2, 12)] %>%  table %>% as.data.frame 
dateway$timestamp  <- as.Date(dateway$timestamp)
datebreaks <- seq(as.Date("2011-08-01"), as.Date("2015-06-01"), by="month")
ggplot(data= dateway, aes(x=timestamp, y = Freq, fill = product_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "交易時間與用途", x = "交易時間", y = "計數", fill="用途") +
  scale_x_date(breaks=  datebreaks, labels=date_format("%Y-%m")) +
  theme(axis.ticks=element_blank(), axis.text.x=element_text(angle=90), legend.position="bottom") 


#############################
#羅
library(grid)
#價格與公寓空間(有離群和沒有離群)
p1 <- ggplot(aes(x=full_sq, y=price_doc), data=train) + 
  geom_point(color='#009FCC')+
  labs(x='總面積', y='價錢', title='價錢與總面積(含離群值)')

p2 <- train %>% 
  filter(full_sq < 2000) %>%
  ggplot(aes(x=full_sq, y=price_doc)) + 
  geom_point(color='#009FCC', alpha=0.5) +
  labs(x='總面積', y='價錢', title='價錢與總面積(不含離群值)')
#source("http://peterhaschke.com/Code/multiplot.R")
#multiplot(p1, p2, cols=2)
#library("gridExtra")
#grid.arrange(p1, p2, ncol=2)
#cowplot::plot_grid(p1, p2, labels = "AUTO")
library(patchwork)
p1 + p2

#平均價格前20地區
train %>% select(sub_area,price_doc)%>% group_by(sub_area)%>% 
  summarize(count=n(),price=mean(price_doc))%>%
  arrange(desc(price))%>%head(n=20)%>% 
  ggplot(aes(x=factor(sub_area,levels=sub_area),y=price))+
  geom_bar(fill="#00BBFF",stat="identity")+
  theme(legend.position="none", axis.text.x = element_text(angle=90))+
  labs(title="平均價格前20地區",x="地區", y="平均價格")

#公寓樓層數與log價格
ggplot(train,aes(x=factor(floor),y=log(price_doc),fill=factor(floor)))+
  geom_boxplot(alpha=0.4)+
  labs(x="公寓樓層數",y="log(價格)",title="樓層數與價格之關係")+
  theme(legend.position="none")
ggplot(train,aes(x=factor(floor),y=price_doc,fill=factor(floor)))+
  geom_boxplot(alpha=0.4)+
  labs(x="公寓樓層數",y="Price",title="樓層數與價格")+
  theme(legend.position="none")

#學校有關變數與價格之corrplot
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue","#00007F"))
school_chars <- c('children_preschool', 'preschool_quota', 'preschool_education_centers_raion',
                  'children_school', 'school_quota', 'school_education_centers_raion', 
                  'school_education_centers_top_20_raion', 'university_top_20_raion',
                  'additional_education_raion', 'additional_education_km', 'university_km',
                  'price_doc')

corrplot(cor(train[, school_chars], use='complete.obs'), col = col1(20))


#附近設施距離+價格之corrplot
distance_chars <- c('metro_km_avto', 'metro_km_walk', 'kindergarten_km',
                    'school_km', 'park_km', 'green_zone_km', 
                    'industrial_km', 'stadium_km',
                    'cemetery_km', 'market_shop_km', 'railroad_station_walk_km',
                    'price_doc')
corrplot(cor(train[, distance_chars], use='complete.obs'), col = col1(20))


#交易日期與平均價格
train %>%
  group_by(timestamp) %>%
  summarize(mean_price = mean(price_doc)) %>%
  ggplot(aes(x = timestamp, y = mean_price)) +
  geom_line(color = '#009FCC') + 
  ggtitle('交易日期與每日平均價格')

#log(Price) between Investment and OwnerOccupier
ggplot(aes(x=price_doc), data=train) + 
  geom_density(fill='#009FCC', color='#009FCC') + 
  facet_grid(~product_type) + 
  scale_x_continuous(trans='log')+
  labs(x='log(價格)', y='', title='不同交易目的與log(價格)之間的關係')


#購買型態的平均價格對比
train %>% group_by(product_type) %>% summarize(mean(price_doc))


#Dist. of price betweem different product_type 
ggplot(train,aes(x= price_doc, fill= product_type))+
  geom_density(alpha=0.5)+
  labs(title="不同交易用途之價格分布",x="價格" , fill="交易用途") 
