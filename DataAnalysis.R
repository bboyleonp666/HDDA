
### Use methods like PCA, isomap to analyze the imputed data
### Create distants and correlation(normalized covariance)
dataCor = cor(data) # equal to cov(data)
# dataDist = dist(data) # computing distance matrices takes time
# saveRDS(dataDist, "DataDist.rds") # save distance matrices as .rds file

## for reading distance data
dataDist = readRDS("DataDist.rds")

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
