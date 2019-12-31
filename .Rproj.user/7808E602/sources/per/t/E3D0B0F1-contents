### packages
library(mice)
library(missForest)

### ------------ Imputation ------------ ###
## try n error: imputation
HousePrice = read.csv("HousePrice.csv")

### ------------ Testing Imputation Accuracy ------------ ###
# # get complete case of house price which are continuous
# CompleteHousePrice = HousePrice[complete.cases(HousePrice), ]
# mean(is.na(HousePrice)) # missing rate
# 
# ## use CompletetHousePrice as input for analysis for saving time
# write.csv(CompleteHousePrice, "ImputedHousePrice.csv", row.names = FALSE)
# 
# ### imputation by multi-session
# # TestImputeHousePrice = prodNA(CompletetHousePrice, noNA = .05) # package missForest
# 
# RFimputeHousePrice = mice(TestImputeHousePrice, method = "rf", maxit = 2, m = 20)
# saveRDS(RFimputeHousePrice, "RFimpute.rds")
# complete(RFimputeHousePrice)
# 
# CARTimputeHousePrice = mice(TestImputeHousePrice, method = "cart", maxit = 2, m = 20)
# saveRDS(CARTimputeHousePrice, "CARTimpute.rds")
# complete(CARTimputeHousePrice)
### ------------ Ignore this part ------------ ###

## take a look at NA data
str(HousePrice)
NAPerCol = colSums(is.na(HousePrice)) # missing per columns 
NAPerCol[NAPerCol > 0] # only columns with missing value

## data of NA for "cafe.*1500"
cafe1500 = HousePrice[, grepl("cafe.*1500", colnames(HousePrice))]
cafe1500[!complete.cases(cafe1500), ]

## data of NA for "cafe.*2000"
cafe2000 = HousePrice[, grepl("cafe.*2000", colnames(HousePrice))]
cafe2000[!complete.cases(cafe2000), ]

# Conclusion: data with NA all in the same row 
# which will lead to data after imputation still be missing somehow

imputeFUN = function(data, METHOD, MAXIT = 2, M = 10){
  # imputation
  start = Sys.time()
  impute = mice(data, method = METHOD, maxit = MAXIT, m = M)
  end = Sys.time()
  timePass = end - start
  
  # save imputed data
  saveName = paste(METHOD, "_impute.rds", sep = "")
  saveRDS(impute, saveName)
  
  # print time spent for imputation
  cat("Time Pass:", timePass)
  
  # return all the output as list
  out = list(impute = impute, timePass = timePass)
  return(out)
}
cart = imputeFUN(HousePrice, "cart")
randomforest = imputeFUN(HousePrice, "rf")
# pmm = imputeFUN(HousePrice, "pmm")

