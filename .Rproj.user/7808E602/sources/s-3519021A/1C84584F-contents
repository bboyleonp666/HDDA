## package
library(magrittr)
library(ggplot2)

DataDir = list.files()[file.info(list.files())$isdir]
# HousePriceTest = read.csv(list.files(DataDir, pattern = "test", full.names = TRUE))
HousePriceTrain = read.csv(list.files(DataDir, pattern = "train", full.names = TRUE))


NA_volumn = colSums(is.na(HousePriceTrain))
NAdata = data.frame(variable = row.names(NAdata), 
                    notNA_volumn = nrow(HousePriceTrain) - NA_volumn,
                    NA_volumn, 
                    row.names = NULL)

NAdata %>% 
  subset(NA_volumn > 0) %>%
  ggplot(aes(reorder(variable, NA_volumn), y = NA_volumn)) + 
  coord_flip() + xlab("Variables") + ylab("NA Volumn") +
  geom_bar(position = "stack", stat = "identity") +
  theme_bw()
