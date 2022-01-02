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
## Merging the Barro-Lee dataset
library(dplyr)
BL_MF_M <- left_join(BL_MF, BL_M, by = c("Country", "Year"))
BL_all <- left_join(BL_MF_M, BL_F, by = c("Country", "Year"))

#Electoral emocracy index - V-Dem
vdem <- read.csv("~/Desktop/ITAS/Final Project/V-Dem-CY-Core-v11.1.csv")

#subsetting IPU by newest elections and chamber type
names(IPUdata)[names(IPUdata) == 'Chamber Type'] <- "chamber_type"
names(IPUdata)[names(IPUdata) == '% Of Women in Chamber'] <- "women_in_chamber"
IPU_2005_2015 <- subset(IPUdata, subset = (IPUdata$Year >= 2005 & IPUdata$Year <= 2015))
IPU_max <- IPU_2005_2015 %>% group_by(Country) %>% slice(which.max(Year))
IPU_clean <- subset(IPU_max, subset = (IPU_max$chamber_type == "Single" | IPU_max$chamber_type == "Lower"))

#Merging IPU and QAROT
IPU_QAROT_2005_2015 <- left_join(IPU_clean, QAROT, by = c("Country", "Year"))
IPU_QAROT_2005_2015 <- subset(IPU_QAROT_2005_2015, select = c(1, 2, 4, 6, 9, 14))

#Subsetting and merging Vdem
vdem_2005_2015 <- subset(vdem, subset = (vdem$year >= 2005 & vdem$year <= 2015))
vdem_2005_2015 <- subset(vdem, select = c(1, 4, 23))
names(vdem_2005_2015)[names(vdem_2005_2015) == "country_name"] <- "Country"
names(vdem_2005_2015)[names(vdem_2005_2015) == "year"] <- "Year"
names(vdem_2005_2015)[names(vdem_2005_2015) == "v2x_polyarchy"] <- "electoral_democracy_index"
IPU_QAROT_vdem_2005_2015 <- left_join(IPU_QAROT_2005_2015, vdem_2005_2015, by = c("Country", "Year"))

#Subsetting and merging Barro-Lee
#2005 - 2007
BL_all_2005 <- subset(BL_all, subset = BL_all$Year == 2005)
IPU_QAROT_vdem_2005_2007 <- subset(IPU_QAROT_vdem_2005_2015, subset = (IPU_QAROT_vdem_2005_2015$Year >= 2005 & IPU_clean$Year <= 2007))

data_2005_2007 <- left_join(IPU_QAROT_vdem_2005_2007, BL_all_2005, by = "Country")

#2008 - 2012
BL_all_2010 <- subset(BL_all, subset = BL_all$Year == 2010)
IPU_QAROT_vdem_2008_2012 <- subset(IPU_QAROT_vdem_2005_2015, subset = (IPU_QAROT_vdem_2005_2015$Year >= 2008 & IPU_clean$Year <= 2012))

data_2008_2012 <- left_join(IPU_QAROT_vdem_2008_2012, BL_all_2005, by = "Country")

#2013 - 2015
BL_all_2015 <- subset(BL_all, subset = BL_all$Year == 2015)
IPU_QAROT_vdem_2013_2015 <- subset(IPU_QAROT_vdem_2005_2015, subset = (IPU_QAROT_vdem_2005_2015$Year >= 2013 & IPU_clean$Year <= 2015))

data_2013_2015 <- left_join(IPU_QAROT_vdem_2013_2015, BL_all_2005, by = "Country")

data_2005_2012 <- rbind(data_2005_2007, data_2008_2012)
data_2005_2015 <- rbind(data_2005_2012, data_2013_2015)

final_dataset <- subset(data_2005_2015, select =  c(1:7, 13, 14, 16, 18, 31, 32, 34, 36, 49, 50, 52, 54))
final_dataset <- na.omit(final_dataset)

final_dataset$women_in_chamber <- as.numeric(final_dataset$women_in_chamber)
final_dataset$women_in_chamber <- final_dataset$women_in_chamber*100
names(final_dataset)[names(final_dataset) == "implemented.quota"] <- "implemented_quota"
names(final_dataset)[names(final_dataset) == "Country"] <- "country"
names(final_dataset)[names(final_dataset) == "Year.x"] <- "year"
names(final_dataset)[names(final_dataset) == "Region"] <- "region"

final_dataset$mf_educational_attainment <- final_dataset$mf_primary + final_dataset$mf_secondary + final_dataset$mf_tertiary
final_dataset$m_educational_attainment <- final_dataset$m_primary + final_dataset$m_secondary + final_dataset$m_tertiary
final_dataset$f_educational_attainment <- final_dataset$f_primary + final_dataset$f_secondary + final_dataset$f_tertiary

final_dataset$mf_total_secondary <- final_dataset$mf_secondary + final_dataset$mf_tertiary
final_dataset$m_total_secondary <- final_dataset$m_secondary + final_dataset$m_tertiary
final_dataset$f_total_secondary <- final_dataset$f_secondary + final_dataset$f_tertiary

write.csv(final_dataset, "~/Desktop/ITAS/final_dataset.csv")










#Reading CSV file
wic <- read.csv("final_dataset_2015.csv")
wic <- subset(wic, select = -1)

#About the sample 
library(dplyr)
dim_wic <- dim(wic)
region_count <- wic %>% count(Region)
chambert_count <- wic %>% count(chamber_type)

#About "Women in Chamber"
women_mean <- mean(wic$women_in_chamber)
women_median <- median(wic$women_in_chamber)

women_region_mean <- tapply(wic$women_in_chamber, wic$Region, mean)
women_region_median <- tapply(wic$women_in_chamber, wic$Region, median)


boxplot(women_in_chamber ~ Region,
        data = wic,
        ylab = "Percentage of women in chamber",
        xlab = "Region",
        main = "Percentage of Women in Chamber per Region",
        cex = 0.5,
        las = 1,
        xaxt = "n"
)
axis(1, at = 1:4, labels = c("America", "Asia", "Europe", "Sub-Saharan Africa"),)

women_chambert_mean <- tapply(wic$women_in_chamber, wic$chamber_type, mean)
women_chambert_median <- tapply(wic$women_in_chamber, wic$chamber_type, median)

women_quota_mean <- tapply(wic$women_in_chamber, wic$implemented_quota, mean)

