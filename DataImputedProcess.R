### packages
library(vegan) # for isomap

### ---- read data and remove NA rows and columns that could cause multi-collinearity ---- ###
### read data from .rds file
HousePriceRDS = readRDS(list.files(pattern = "cart"))
HousePrice_withNA = HousePriceRDS$data

## remove variables by hand
cnames = colnames(HousePrice_withNA)

# pick the index of variable with "_min" in their names but not including "_min_price"
column_min_idx = setdiff(grep("_min", cnames), grep("_min_price", cnames))
column_cafavg_idx = grep("cafe_avg", cnames)
column_to_remove = union(column_min_idx, column_cafavg_idx)

# pick columns of only complete ones
row_idx_to_keep = complete.cases(HousePrice_withNA)

# remove rows and columns that we decided to remove
HousePrice = HousePrice_withNA[row_idx_to_keep, -column_to_remove]

# split data into id, data, price
id = HousePrice[, "id"]
price = HousePrice[, "price_doc"]

# find index of vector in another vector using "match"
idx_for_id_price = match(c("id", "price_doc"), colnames(HousePrice))
data = HousePrice[, -idx_for_id_price]


### load raw data and pick the categorical data we want
## read origin data
DataDir = list.files()[file.info(list.files())$isdir]
HousePriceTrain = read.csv(list.files(DataDir, pattern = "train", full.names = TRUE))
cnames_train = colnames(HousePriceTrain)

## find the categorical variables
# find the index for the specific variables
factor_idx = which(sapply(HousePriceTrain, class) == "factor")
raion_idx = grep("raion", cnames_train)

raion = HousePriceTrain[, intersect(factor_idx, raion_idx)]
type = HousePriceTrain$product_type
sub_area = HousePriceTrain$sub_area

categorical = cbind(raion, type, sub_area)[row_idx_to_keep, ]

### Combine data into a list
HousePriceList = list(id = id, 
                      price = price, 
                      continuous = data, 
                      categorical = categorical)

## save data in "HousePriceList.rds"
saveRDS(HousePriceList, "HousePriceList.rds")

