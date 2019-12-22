### packages
library(magrittr)
library(dplyr)
library(mice)
library(missForest)

### data pre-processing
# -----------------------
DataDir = list.files()[file.info(list.files())$isdir]
# HousePriceTest = read.csv(list.files(DataDir, pattern = "test", full.names = TRUE))
HousePriceTrain = read.csv(list.files(DataDir, pattern = "train", full.names = TRUE))
head(HousePriceTrain)
dim(HousePriceTrain)

## pick some special variable which we want to keep
id = HousePriceTrain$id
time_trade = as.Date(HousePriceTrain$timestamp)
sub_area = HousePriceTrain$sub_area

## create column names vector
cnames = colnames(HousePriceTrain)

### remove columns with ("count" or "ID" or "top") in column names
Columns_Count_Top_ID = (grepl("count", cnames) | grepl("ID", cnames) | grepl("top", cnames))
HousePrice_NO_Count_Top_ID = HousePriceTrain[, !Columns_Count_Top_ID]
head(HousePrice_NO_Count_Top_ID)
tables = apply(HousePrice_NO_Count_Top_ID, 2, table)
str(HousePrice_NO_Count_Top_ID)

### check NA proportion in variables
## default setting
AbandonRatio = .2

## get columns with NAs
NAinVar = colSums(is.na(HousePrice_NO_Count_Top_ID))
NAinVar[NAinVar > 0]

## get columns with NAs
# NAratio = apply(HousePriceTrain, 2, function(x) mean(is.na(x)))
# round(NAratio[NAratio > 0], 2)

## remove columns with too many NAs
ColumnRM_NA = which(NAinVar >= nrow(HousePrice_NO_Count_Top_ID)*AbandonRatio)
HousePrice_NAreduce = HousePrice_NO_Count_Top_ID[, -ColumnRM_NA]
head(HousePrice_NAreduce)
dim(HousePrice_NAreduce)
str(HousePrice_NAreduce)

## remove data which datatype is "factor"
ColumnRM_factor = sapply(HousePrice_NAreduce, class)
HousePrice_FACTORreduce = HousePrice_NAreduce[, ColumnRM_factor != "factor"]
str(HousePrice_FACTORreduce)

      
### transfer dummy variables into dummy
## find the freqence of each column not greater then 10 while it's not a factor
TabLen = apply(HousePrice, 2, function(x) length(table(x)))
names(TabLen[TabLen <= 10])

HousePrice[, which(TabLen <= 10)] %>%
  apply(2, FUN = table, useNA = "ifany")

ncol(HousePrice) - sum(TabLen <= 10) # Number of the columns left without freqence length not greater than 10

### Decision: remove all the columns which are factor(dummy) or freqence length not greater than 10
## find the index of factor and TabLen <= 10
idx = unique(c(which(sapply(HousePrice, is.factor)), which(TabLen <= 10)))
HousePriceConti = HousePrice[, -idx]
dim(HousePriceConti)
head(HousePriceConti)

## check the complete case left
sum(!complete.cases(HousePriceConti)) / nrow(HousePriceConti)
# more than 37% rows contain missing values

## write continuous data into HousePrice_ContiVar.csv
# write.csv(data = HousePriceConti, "HousePrice_ContiVar.csv", row.names = FALSE)


### ------------ Imputation Start From Here ------------ ###
## try n error: imputation
# get complete case of house price which are continuous
HousePriceConti = read.csv("HousePriceContinueVar.csv")
CompleteHousePrice = HousePriceConti[complete.cases(HousePriceConti), ]
mean(is.na(HousePriceConti)) # missing rate

## use CompletetHousePrice as input for analysis for saving time
write.csv(CompleteHousePrice, "ImputedHousePrice.csv", row.names = FALSE)

# use some sample to find the best method to imputate
# idx = sample(1:nrow(CompletetHousePrice), 5000)
# CompletetHousePrice_sample = CompletetHousePrice[idx, ]
# TestImputeHousePrice = prodNA(CompletetHousePrice_sample, noNA = .05)
TestImputeHousePrice = prodNA(CompletetHousePrice, noNA = .05)
RFimputeHousePrice = mice(TestImputeHousePrice, method = "rf", maxit = 2, m = 20)
write.csv(data = RFimputeHousePrice, "RFimputationTest.csv", row.names = FALSE)
complete(RFimputeHousePrice)

CARTimputeHousePrice = mice(TestImputeHousePrice, method = "cart", maxit = 2, m = 20)
complete(CARTimputeHousePrice)

### 

