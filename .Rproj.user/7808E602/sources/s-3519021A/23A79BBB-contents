### packages
library(vegan)
# for isomap


### Use methods like PCA, isomap to analyze the imputed data
HousePriceFile = read.csv(list.files(pattern = "Imputed"))

# find the specific columns
cnames = colnames(HousePriceFile) # column names
price_column = grep("price_doc", cnames) # get price(target) column
id_column = grep("id", cnames) # get id column

# split data into different parts
price = HousePriceFile[, price_column]
id = HousePriceFile[, id_column]
rawdata = HousePriceFile[, -c(id_column, price_column)]

data = scale(rawdata) # scale the raw data
head(data)
dim(data)


### Create distants and correlation(normalized covariance)
dataCor = cor(data) # equal to cov(data)
dataDist = dist(data) # computing distance matrices takes time
saveRDS(dataDist, "DataDist.rds") # save distance matrices as .rds file

## for reading distance data
# dataDist = readRDS("DataDist.rds")

### Dimension Reduction Methods
PCA = eigen(dataCor)
SVD = svd(data)

{
  mdsStart = Sys.time()
  MDS = cmdscale(dataDist)
  mdsEnd = Sys.time()
  cat("Time pass:", mdsEnd - mdsStart, "Hour")
  write.csv(MDS, "MDS.csv", row.names = FALSE)
}

{
  isoStart = Sys.time()
  ISO = isomap(dataDist, ndim = 20, k = 500)
  isoEnd = Sys.time()
  cat("Time pass:", isoEnd - isoStart, "Hour")
  write.csv(MDS, "iso.csv", row.names = FALSE)
}


for(i in c(100, 500, 1000, 2000, 3000)){
  testISO = isomap(dataDist, ndim = 20, k = 500)
  # plot()
}
