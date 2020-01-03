### separate list into three parts, "n >> p", "n = p", "n << p"
## read data and get dimensions
HousePriceList = readRDS("HousePriceList.rds")
(dim = dim(HousePriceList$continuous))        # [1] 26073   114

## get index by random sample numbers
greater_idx = sample(1:dim[1], dim[1] - 1e4)  # keep 10,000 rows for testing
equal_idx = sample(1:dim[1], dim[2])          # pick the number of rows of the same as columns
smaller_idx = sample(1:dim[1], 20)            # pick 20 rows decided by ourselves

## store data in a list
DifferentDimensionHousrPriceList = list(
  `n>>p` = HousePriceList$continuous[greater_idx, ],
  `n==p` = HousePriceList$continuous[equal_idx, ], 
  `n<<p` = HousePriceList$continuous[smaller_idx, ],
  id = HousePriceList$id,
  price = HousePriceList$price,
  categorical = HousePriceList$categorical,
  all = HousePriceList$continuous
)

## save as rds file
saveRDS(DifferentDimensionHousrPriceList, "DimReduction/DifferentDimensionHousrPriceList.rds")