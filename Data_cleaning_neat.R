## Importing datasets

#Educational Attainment - Barro Lee Dataset
BL_MF <- read.csv("~/Desktop/ITAS/Final Project/BL_v3_MF1564.csv")
BL_M <- read.csv("~/Desktop/ITAS/Final Project/BL_v3_M1564.csv")
BL_F <- read.csv("~/Desktop/ITAS/Final Project/BL_v3_F1564.csv")

#Women in Chamber - IPU
library(readxl)
IPUdata <- read_excel("Desktop/ITAS/Final Project/women_in_parliament-historical_database-1945_to_2018 (3).xlsx",
                      na = "NA")

#Electoral Gender Quota - QAROT
QAROT <- read.csv("~/Desktop/ITAS/Final Project/QAROTdata_HughesPaxtonClaytonZetterberg_CountryYear_V1_August2017.csv")

#Democracy index - LIED
LIED <- read_excel("Desktop/ITAS/Final Project/lied_v5.1.xls", 
                   na = "NA")

## Merging the Barro-Lee dataset
library(dplyr)
BL_MF_M <- left_join(BL_MF, BL_M, by = c("Country", "Year"))
BL_all <- left_join(BL_MF_M, BL_F, by = c("Country", "Year"))

## For 2010

BL_all_2010 <- subset(BL_all, 
                      subset = BL_all$Year == 2010)
IPUdata_2010 <- subset(IPUdata, 
                       subset = 
                         (IPUdata$Year == 2010) & 
                         (IPUdata$`Chamber Type` == "Single" | IPUdata$`Chamber Type` == "Lower"))
BL_IPU_2010 <- left_join(IPUdata_2010, 
                         BL_all_2010, 
                         by = c("Country", "Year"))
BL_IPU_2010 <- subset(BL_IPU_2010, 
                      select = -c(3, 5, 10:14, 26:32, 44:50, 62, 63))

QAROT_2010 <- subset(QAROT, 
                     subset = QAROT$Year == 2010, 
                     select = c("Country", "Year", "adopted.quota", "implemented.quota"))
BL_IPU_QAROT_2010 <- left_join(BL_IPU_2010, QAROT_2010, by = c("Country", "Year"))
LIED_2010 <- subset(LIED, 
                    subset = LIED$Year == 2010, 
                    select = c("Country", "Year", "lexical_index"))
LIED_2010 <- subset(LIED_2010, 
                    subset = LIED_2010$lexical_index == 6)
final_dataset_2010 <- left_join(BL_IPU_QAROT_2010, LIED_2010, by = c("Country", "Year"))
final_dataset_2010 <- na.omit(final_dataset_2010)
names(final_dataset_2010)[names(final_dataset_2010) == '% Of Women in Chamber'] <- "Women in Chamber"


## For 2015

BL_all_2015 <- subset(BL_all, 
                      subset = BL_all$Year == 2015)
IPUdata_2015 <- subset(IPUdata, 
                       subset = 
                         (IPUdata$Year == 2015) & 
                         (IPUdata$`Chamber Type` == "Single" | IPUdata$`Chamber Type` == "Lower"))
BL_IPU_2015 <- left_join(IPUdata_2015, 
                         BL_all_2015, 
                         by = c("Country", "Year"))
BL_IPU_2015 <- subset(BL_IPU_2015, 
                            select = -c(3, 5, 10:14, 26:32, 44:50, 62, 63))

QAROT_2015 <- subset(QAROT, 
                     subset = QAROT$Year == 2015, 
                     select = c("Country", "Year", "adopted.quota", "implemented.quota"))
BL_IPU_QAROT_2015 <- left_join(BL_IPU_2015, QAROT_2015, by = c("Country", "Year"))
LIED_2015 <- subset(LIED, 
                    subset = LIED$Year == 2015, 
                    select = c("Country", "Year", "lexical_index"))
LIED_2015 <- subset(LIED_2015, 
                    subset = LIED_2015$lexical_index == 6)
final_dataset_2015 <- left_join(BL_IPU_QAROT_2015, LIED_2015, by = c("Country", "Year"))
final_dataset_2015 <- na.omit(final_dataset_2015)

names(final_dataset_2015)[names(final_dataset_2015) == '% Of Women in Chamber'] <- "Women in Chamber"
final_dataset_2015$`Women in Chamber` <- as.numeric(final_dataset_2015$`Women in Chamber`)
final_dataset_2015$`Women in Chamber` <- final_dataset_2015$`Women in Chamber`*100

Israel <- subset(final_dataset_2015, subset = final_dataset_2015$Country == "Israel")
Israel_wic_mean <- mean(Israel$`Women in Chamber`)
final_dataset_2015 <- final_dataset_2015[-c(15:18),]
final_dataset_2015$`Women in Chamber`[14] <- Israel_wic_mean

final_dataset_2015 <- subset(final_dataset_2015, select = c(1, 2, 4, 7, 8, 9 , 11, 13, 19, 20, 22, 24, 30, 31, 33, 35, 43))
write.csv(final_dataset_2015, "~/Desktop/ITAS/final_dataset_2015.csv")




