### packages
library(magrittr)
library(dplyr)
library(mice)
library(missForest)

### ------------ Delete columns of categorical ------------ ###
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
dim(HousePrice_FACTORreduce)


### ------------ Combine data with similar info ------------ ###
## Combine male and female data as sex ratio
cnames = colnames(HousePrice_FACTORreduce) # get the columns of data after deleting variables
start = which(grepl("full_all", cnames)) # find variable "full_all"
end = which(grepl("X0_13_female", cnames)) # find variable "X0_13_female"

## seperate data into two parts
data_part1 = HousePrice_FACTORreduce[, -(start:end)] # other data
data_part2 = HousePrice_FACTORreduce[, start:end] # population data

# head(data_part2)
# population data to sex ratio data
ratio = NULL
for(i in 1:ncol(data_part2)){
  if(i %% 3 == 1){
    ratio = cbind(ratio, data_part2[, i])
  }else if(i %% 3 == 2){
    ratio = cbind(ratio, (data_part2[, i]/data_part2[, i+1]))
  }
}
varnames = gsub("_male", "_ratio", colnames(data_part2))
varnames = varnames[!grepl("female", varnames)]
varnames = gsub("ekder", "elder", varnames)
varnames = gsub("male_f", "full_ratio", varnames)
colnames(ratio) = varnames

columns_to_rm = grepl("X0_1", varnames)
sexRatio = data.frame(ratio[, !columns_to_rm])

## Combine sex ratio and data_part1 and name it as HousePrice
HousePrice = cbind(data_part1, sexRatio)
str(HousePrice)
dim(HousePrice)
write.csv(HousePrice, "HousePrice.csv", row.names = FALSE)

## missing ratio
cat("Missing ratio:", mean(is.na(HousePrice)), "\n")
colMeans(is.na(HousePrice))


### 

