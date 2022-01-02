## For 2015
# Merging the Barro-Lee dataset
library(dplyr)
BL_MF_M <- left_join(BL_MF, BL_M, by = c("Country", "Year"))
BL_all <- left_join(BL_MF_M, BL_F, by = c("Country", "Year"))
          
# Subsetting the BL dataset
BL_all_2015 <- subset(BL_all, subset = BL_all$Year == 2015)

#Subsetting the IPU dataset
IPUdata_2015 <- subset(IPUdata, subset = (IPUdata$Year == 2015) & (IPUdata$`Chamber Type` == "Single" | IPUdata$`Chamber Type` == "Lower"))
IPUdata_2015_sl <- subset(IPUdata_2015, subset = (IPUdata_2015$`Chamber Type` == "Single" | IPUdata_2015$`Chamber Type` == "Lower"))

#Merging BL dataset and IPU dataset
BL_IPU_2015 <- left_join(IPUdata_2015, BL_all_2015, by = "Country")

##For 2010
BL_all_2010 <- subset(BL_all, subset = BL_all$Year == 2010)
IPUdata_2010 <- subset(IPUdata, subset = IPUdata$Year == 2010)
IPUdata_2010_sl <- subset(IPUdata_2010, subset = (IPUdata_2010$`Chamber Type` == "Single" | IPUdata_2010$`Chamber Type` == "Lower"))
BL_IPU_2010 <- left_join(IPUdata_2010_sl, BL_all_2010, by = "Country")

BL_IPU_2015_clean <- subset(BL_IPU_2015, select = -c(3, 5, 10:15, 27:33, 45:51, 63:65))
BL_IPU_2010_clean <- subset(BL_IPU_2010, select = -c(3, 5, 10:15, 27:33, 45:51, 63:65))


final_dataset <- na.omit(BL_IPU_2015_clean)
names(final_dataset)[names(final_dataset) == 'Year.x'] <- "Year"

#QAROT
QAROT_2015 <- subset(QAROT, subset = QAROT$Year == 2015, select = c("Country", "Year", "adopted.quota", "implemented.quota"))
QAROT_2015_clean <- subset(QAROT_2015, select = c("Country", "Year", "adopted.quota", "implemented.quota"))

final_dataset_quota <- left_join(final_dataset, QAROT_2015_clean, by = c("Country", "Year"))

#LIED (democracy index)
LIED_2015 <- subset(democracy_ind, subset = democracy_ind$Year == 2015, select = c("Country", "Year", "lexical_index"))
LIED_2015_clean <- subset(LIED_2015, select = c("Country", "Year", "lexical_index")) 
final_dataset_quota_dem_2015 <- left_join(final_dataset_quota, LIED_2015_clean, by = c("Country", "Year"))

osamura_oshiro2015 <- na.omit(final_dataset_quota_dem_2015)
osamura_oshiro2015_dem <- subset(osamura_oshiro2015, subset = osamura_oshiro2015$lexical_index == 6)
