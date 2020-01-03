data <- readRDS("HousePriceList")
price <- read.csv("HousePrice.csv")
colnames(price)

write.csv(summary(cont), file = "summary_continuous.csv")

names(data$categorical)
names(data$continuous)


is.na(price)
library(VIM)
aggr(price,prop=FALSE,numbers=TRUE) 



str(train, list.len=ncol(train))
      
   
     
 
sum(is.na(train$material))/length(price[, 1])
table(train$product_type)/length(price[, 1])
table(train$ecology )/length(price[, 1])
table(train$railroad_1line)/length(price[, 1])
table(train$big_road1_1line   )/length(price[, 1])
table(train$water_1line  )/length(price[, 1])
table(train$detention_facility_raion)/length(price[, 1])
table(train$nuclear_reactor_raion  )/length(price[, 1])
table(train$big_market_raion )/length(price[, 1])
table(train$railroad_terminal_raion)/length(price[, 1])
table(train$radiation_raion)/length(price[, 1])
table(train$oil_chemistry_raion)/length(price[, 1])
table(train$incineration_raion)/length(price[, 1])
table(train$thermal_power_plant_raion  )/length(price[, 1])
table(train$culture_objects_top_25   )/length(price[, 1])
table(train$sub_area  )/length(price[, 1])
table(train$product_type  )/length(price[, 1])


#tables of build year
sum(is.na(train$build_year))/length(price[, 1])
levels(as.factor(train$build_year))

by <- subset(train, build_year >= 1600 & build_year < 2019 , select=8)
windows()
by %>%
  table %>%
  as.data.frame %>%
  ggplot(aes(reorder(., Freq), y = Freq)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "建造年分(1691-2018)", x = "年分", y = "計數") +
  theme(axis.ticks=element_blank(), axis.text.x=element_text(angle=90, size=8))
  
theme(axis.ticks=element_blank(), axis.text.x=element_blank())

windows()
by %>%
  table %>%
  as.data.frame %>%
  ggplot(aes(x=., y = Freq)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "建造年分(1691-2018)", x = "年分", y = "計數") +
  theme(axis.ticks=element_blank(), axis.text.x=element_text(angle=90, size=8))



#tables of ecology
library(plotrix) 
library(plotly)
library(dplyr)
slices <- summary(train$ecology)
p <- round(100*slices/sum(slices), 2) 
rate <- c(names(slices),p)
rate <- matrix(rate, nrow= 5, ncol=2, byrow=T)
colnames(rate) <- c("ecology", "percentage")
rate <- as.data.frame(rate)
plot_ly(data=rate, labels = ~ecology, values = ~percentage, type = 'pie') %>%
  layout(title = "房屋所在生態區比例",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#tables of sub_area
windows()
ggplot(data = train, aes(x = reorder(train$sub_area, train$sub_area, function(x) length(x)))) +
  geom_bar() +   
  coord_flip() +
  labs(title = "房屋所在地區", x = "區域", y = "數量")+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 5))
table(train$sub_area) > 500 



str(cont, list.len=ncol(cont))
str(cate, list.len=ncol(cate))

