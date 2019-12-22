### ------------ Imputation ------------ ###
## try n error: imputation
# get complete case of house price which are continuous
HousePriceConti = read.csv("HousePrice.csv")
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
