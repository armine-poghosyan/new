##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Date Last Updated: # Mon Sep 12 2022
# Purpose: Merge precip and ndvi datasets (obtain baseline mean & st.dev)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rm(list = ls(all = TRUE))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Libraries 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(dplyr)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Working Directory
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pathdata <- if(Sys.info()["user"] == "armin"){
  "C:/Users/armin/Dropbox/shared_wb_climateshocks/data/"
} else{ #TODO: coauthors -- change the file here for your path
  "~/Dropbox/shared_wb_climateshocks/data/"
}
datapath <- function(x){paste0(pathdata, x)}

setwd("G:/My Drive/VT/_Projects/_WB Sahel/_data/output")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

precip_md <- read.csv(datapath("output/admin2precip_md.csv"))
ndvi_md <- read.csv(datapath("output/admin2ndvi_md.csv"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Remove NAs
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
precip_md <- data.frame(precip_md) %>% na.omit()
ndvi_md <- data.frame(ndvi_md) %>% na.omit()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Ensure the Time Period Matches: Removed 1981, 2020,and 2021 Years
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
precip_md <- precip_md[!(precip_md$year == '1981'|precip_md$year == '2020'|precip_md$year == '2021'),]
ndvi_md <- ndvi_md[!(ndvi_md$year == '1981'|ndvi_md$year == '2020'|ndvi_md$year == '2021'),]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Annual Z-Scores 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ndvi_admin2_md <- 
  ndvi_md %>% 
  select(admin1Name, admin1Pcod, admin2Name, admin2Pcod, year, peak_ndvi) %>%
  group_by(admin2Pcod) %>% 
  mutate(baseline_Mean_ndvi = mean(peak_ndvi[year >= "1982" & year <= "2010"]), 
         baseline_SD_ndvi = sd(peak_ndvi[year >= "1982" & year <= "2010"]),
         baseline_2SD_ndvi = 2*baseline_SD_ndvi)   %>% 
  group_by(admin2Pcod, year) %>% 
  mutate(zscore_ndvi_new = (peak_ndvi - baseline_Mean_ndvi)/(baseline_SD_ndvi)) %>% 
  ungroup()

precip_admin2_md <- 
  precip_md %>% 
  select(admin2Name, admin2Pcod, year, total_precip) %>%
  group_by(admin2Pcod) %>% 
  mutate(baseline_Mean_precip = mean(total_precip[year >= "1982" & year <= "2010"]), 
         baseline_SD_precip = sd(total_precip[year >= "1982" & year <= "2010"]),
         baseline_2SD_precip = 2*baseline_SD_precip) %>% 
  group_by(admin2Pcod, year) %>% 
  mutate(zscore_precip_new = (total_precip - baseline_Mean_precip)/(baseline_SD_precip)) %>% 
  ungroup()


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Merge and Save Z-Score Dataset
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
zscore_merged <- merge(ndvi_admin2_md,precip_admin2_md,by=c("admin2Pcod", "year"))

write.csv(zscore_merged,"zscore_merged.csv", row.names = TRUE)

# Keep only the important years
zscore_11_14_18 <- zscore_merged %>% 
  filter(year == "2011" | year== "2014" | year == "2018")

write.csv(zscore_11_14_18,"zscore_11_14_18.csv")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Shock Variables
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#generate the sum of mean + st dev for shock variable
zscore_merged <- 
  zscore_merged %>% 
  mutate(sh1_ndvi = baseline_Mean_ndvi + baseline_SD_ndvi,
         sh2_ndvi = baseline_Mean_ndvi + baseline_2SD_ndvi,
         sh1_precip = baseline_Mean_precip + baseline_SD_precip,
         sh2_precip = baseline_Mean_precip + baseline_2SD_precip)

#if Z>sh1/sh2, then 1 = shock, else 0
zscore_merged <- 
  zscore_merged %>% 
  mutate(shock_SD_ndvi = ifelse(zscore_ndvi_new > sh1_ndvi, 1, 0),
         shock_2SD_ndvi = ifelse(zscore_ndvi_new > sh2_ndvi, 1, 0),
         shock_SD_precip = ifelse(zscore_precip_new > sh1_precip, 1, 0),
         shock_2SD_precip = ifelse(zscore_precip_new > sh2_precip, 1, 0))

write.csv(zscore_merged, "zscore_merged.csv")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Defining Shock Variables similar to Ange's approach
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#if Z>sh1/sh2, then 1 = shock, else 0
zscore_merged <- 
  zscore_merged %>% 
  mutate(shock_SD_ndvi_pos = ifelse(zscore_ndvi_new > 1, 1, 0), #+1 stdev positive shock
         shock_SD_ndv_neg = ifelse(zscore_ndvi_new < -1, 1, 0), #-1 stdev negative shock 
         shock_2SD_ndvi_pos = ifelse(zscore_ndvi_new > 2, 1, 0),
         shock_2SD_ndvi_neg = ifelse(zscore_ndvi_new < -2, 1, 0),
         
         shock_SD_precip_pos = ifelse(zscore_precip_new > 1, 1, 0),
         shock_SD_precip_neg = ifelse(zscore_precip_new < -1, 1, 0),
         shock_2SD_precip_pos = ifelse(zscore_precip_new > 2, 1, 0),
         shock_2SD_precip_neg = ifelse(zscore_precip_new < -2, 1, 0))

write.csv(zscore_merged, "zscore_merged.csv")

####################################################################################
#WORK ON THE CODE BELOW LATER: ADJUST/LOOP/REPRODUCIBLE


############################
##### BOXPLOTS ########

my_color <- rep("white", length(zscore_merged$year))
my_color[zscore_merged$year==2011] <- "red"
my_color[zscore_merged$year==2014] <- "blue"
my_color[zscore_merged$year==2018] <- "green"

baseline_mean <- mean(zscore_merged$peak_ndvi[zscore_merged$year>=1982 & zscore_merged$year <= 2010])


boxplot(peak_ndvi ~ year, 
        data=zscore_merged, 
        col = my_color,
        main="Peak NDVI at admin2 level over 1982 - 2019 period",
        ylab = "peak NDVI")
abline(h=baseline_mean, col="magenta",lwd=1.5)

legend("topleft", 
       legend = c("Mean NDVI 2011 = 0.278", 
                  "Mean NDVI 2014 = 0. 281", 
                  "Mean NDVI 2018 = 0.312",
                  "Baseline Mean = 0.292"),
       pch =c( "__", "__", "__"), col = c("red", "blue", "green", "magenta"))


boxplot(total_precip ~ year, 
        data=zscore_merged, 
        col=my_color,
        main="Total Precipitation at admin2 level over 1982 - 2019 period",
        ylab = "total precip (mm)")
abline(h=mean(zscore_merged$total_precip[zscore_merged$year>=1982 & zscore_merged$year <= 2010]), 
       col="magenta",lwd=1.5)


legend("topleft", 
       legend = c("2011 = 313", 
                  "2014 = 346", 
                  "2018 = 425 ",
                  "Baseline Mean = 342"),
       pch =c( "__", "__", "__", "__"), col = c("red", "blue", "green", "magenta"))


#######################################################################
#LINE GRAPH

#mean aggregate data
ndvi <- 
  ndvi_admin2_md %>% 
  select(admin1Name, admin1Pcod, admin2Name, admin2Pcod, year, peak_ndvi) %>%
  group_by(year) %>% 
  summarise_at(vars(peak_ndvi), list(name=mean))%>% 
  ungroup()

precip <- 
  precip_admin2_md %>% 
  select(admin2Name, admin2Pcod, year, total_precip) %>%
  group_by(year) %>% 
  summarise_at(vars(total_precip), list(name=mean))%>% 
  ungroup()

#keep only unique observations
#ndvi <- data.frame(unique(ndvi[c("year", "mean_ndvi")]))
#precip<- data.frame(unique(precip[c("year", "mean_precip")]))

#plot the line graph
plot(ndvi$year, ndvi$mean_ndvi, type="b",
     ylab="Mean NDVI",
     xlab="year",
     xaxt="n",
     main="Mean NDVI over 1982 - 2019, mean agg. over department (admin2)") 
axis(1, at = seq(1982, 2019))

abline(v="2011" , col="red",lwd=2)     
abline(v= "2014", col="blue",lwd=2)
abline(v= "2018", col="darkgreen",lwd=2)  
abline(h=mean(ndvi$mean_ndvi[ndvi$year>=1982 & ndvi$year <= 2010]), 
       col="purple", lwd=1.5, lty="dotted")

  
legend("topleft", 
       legend = c("year = 2011", 
                  "year = 2014", 
                  "year = 2018", 
                  "Baseline Mean NDVI = 0.29"),
       pch =c( "__", "__", "__", "__"), 
       col = c("red", "blue", "darkgreen", "purple"))


##THIS IS THE CORRECT CODE (IGNORE THE ONE FOR NDVI)
#plot the line graph
plot(precip$year, precip$name, type="b",
     ylab="Mean precip (mm)",
     xlab="year",
     ylim = c(0, 490),
     xlim = c(),
     xaxt="n",
     main="Mean Precipitation over 1982 - 2019, mean agg. over department (admin2)") 
axis(1, at = seq(1982, 2019))

baseline_precip <- mean(precip$name[precip$year >=1982 
                                          & precip$year <= 2010])

baseline_precip_sd <- sd(precip$name[precip$year >=1982 
                                           & precip$year <= 2010])

UL_precip <- baseline_precip + baseline_precip_sd 
LL_precip <-  baseline_precip - baseline_precip_sd 


abline(v="2011" , col="red",lwd=2, lty="dotted")     
abline(v= "2014", col="blue",lwd=2, lty="dotted")
abline(v= "2018", col="darkgreen",lwd=2, lty="dotted")  
abline(h=baseline_precip, col="purple", lwd=1.5, lty="dotted")
abline(h=UL_precip, col="hotpink", lwd=1.5, lty="dotted")
abline(h=LL_precip, col="hotpink", lwd=1.5, lty="dotted")



legend("bottomleft", 
       legend = c("year = 2011", 
                  "year = 2014", 
                  "year = 2018", 
                  "Baseline Mean precip = 342 mm",
                  "1 sd dev (61) around the baseline mean"),
       pch =c( "__", "__", "__", "__"), 
       col = c("red", "blue", "darkgreen", "purple", "hotpink"))



####################################################################################
####################################################################################
#Histograms

#plot histogram
hist(f$mean_ndvi,                                         
    breaks = 30,
     col="lightpink", 
     xlab="Mean NDVI",
     main="Histogram of Mean NDVI over 1982 - 2019 (mean aggregated over region)") 

abline(v= mean(g_2011$v), col="red",lwd=3)
abline(v= mean(g_2014$v), col="blue",lwd=3)
abline(v= mean(g_2018$v), col="darkgreen",lwd=3)
abline(v=mean(f$mean_ndvi), col="purple", lwd=3)

#add legend for vertical lines

legend("topright", 
       legend = c("Mean NDVI in 2011 = 0.278", 
                  "Mean NDVI in 2014 = 0. 281", 
                  "Mean NDVI in 2018 = 0.312",
                  "Overall Mean NDVI = 0.292"),
       pch =c( "__", "__", "__", "__"), 
       col = c("red", "blue", "darkgreen", "purple"))



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Histogram: OPTIMIZE CODING LATER
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#get the average of ndvi for 2011, 2014, 2018 years

g_2011 <- zscore_11_14_18 %>% filter(year == "2011" ) %>% 
  mutate(v= mean(peak_ndvi))

g_2014 <- zscore_11_14_18 %>% filter(year == "2014" ) %>% 
  mutate(v= mean(peak_ndvi))

g_2018 <- zscore_11_14_18 %>% filter(year == "2018" ) %>% 
  mutate(v= mean(peak_ndvi))

#get the breaks for histogram

hst_breaks <- seq(min(ndvi_admin2_md$peak_ndvi), 
                  max(ndvi_admin2_md$peak_ndvi), length=25)

#plot histogram
  hist(ndvi_admin2_md$peak_ndvi,                                         
     breaks = hst_breaks, 
     col="lightpink", 
     xlab="Peak NDVI",
     main="Histogram of Peak NDVI over 1982 - 2019 (admin2)") 

#addd vertical lines for avg ndvi of 2011, 2014, and 2018

abline(v= mean(g_2011$v), col="red",lwd=3)
abline(v= mean(g_2014$v), col="blue",lwd=3)
abline(v= mean(g_2018$v), col="darkgreen",lwd=3)

#add legend for vertical lines

legend("topright", 
       legend = c("Mean NDVI in 2011 = 0.278", 
                  "Mean NDVI in 2014 = 0. 281", 
                  "Mean NDVI in 2018 = 0.312"),
       pch =c( "__", "__", "__"), col = c("red", "blue", "darkgreen"))


#NOW FOR PRECIPITATION

#get the average of precip for 2011, 2014, 2018 years

p_2011 <- zscore_11_14_18 %>% filter(year == "2011" ) %>% 
  mutate(w= mean(total_precip))

p_2014 <- zscore_11_14_18 %>% filter(year == "2014" ) %>% 
  mutate(w= mean(total_precip))

p_2018 <- zscore_11_14_18 %>% filter(year == "2018" ) %>% 
  mutate(w= mean(total_precip))

#get the breaks for histogram

hst_breaks <- seq(min(precip_admin2_md$total_precip), 
                  max(precip_admin2_md$total_precip), length=25)

#plot histogram
hist(precip_admin2_md$total_precip,                                         
     breaks = hst_breaks, 
     col="lightblue", 
     xlab="Total Precipitation",
     main="Histogram of Total Precipitation over 1982 - 2019 (admin2)") 

#addd vertical lines for avg precip of 2011, 2014, and 2018

abline(v= mean(p_2011$w), col="red",lwd=3)
abline(v= mean(p_2014$w), col="blue",lwd=3)
abline(v= mean(p_2018$w), col="darkgreen",lwd=3)

#add legend for vertical lines

legend("topright", 
       legend = c("Mean Precipitation in 2011 = 313 (mm)", 
                  "Mean Precipitation in 2014 = 346 (mm)", 
                  "Mean Precipitation in 2018 = 425 (mm)"),
       pch =c( "__", "__", "__"), col = c("red", "blue", "darkgreen"))



##############################################################################
# histogram for average ndvi and precip by year

ndvi <- 
  ndvi_admin2_md %>% 
  select(admin1Name, admin1Pcod, admin2Name, admin2Pcod, year, peak_ndvi) %>%
  group_by(year) %>% 
  mutate(mean_ndvi = mean(peak_ndvi)) %>% 
  ungroup()

precip <- 
  precip_admin2_md %>% 
  select(admin2Name, admin2Pcod, year, total_precip) %>%
  group_by(year) %>% 
  mutate(mean_precip = mean(total_precip)) %>% 
  ungroup()

hst_breaks <- seq(min(ndvi$peak_ndvi), 
                  max(ndvi$peak_ndvi), length=39)
#plot histogram
h <- hist(ndvi$year,                                         
          col="darkgray") 

bin_2011 <- cut(g_2011$v, h$breaks)
bin_2014 <- cut(g_2014$v, h$breaks)
bin_2018 <- cut(g_2018$v, h$breaks)

clr <- matrix(data = NA, nrow = 2, ncol=13)

clr[bin_2011] <- "red"
clr[bin_2014] <- "blue"
clr[bin_2018] <- "darkgreen"
plot(h, col=clr, 
     xlab="Peak NDVI",
     main="Histogram of the Average of Peak NDVI for Each year")

#add legend for vertical lines

legend("topright", 
       legend = c("Mean NDVI in 2011 = 0.278", 
                  "Mean NDVI in 2014 = 0. 281", 
                  "Mean NDVI in 2018 = 0.312"),
       pch =c( "__", "__", "__"), col = c("red", "blue", "darkgreen"))


#plot histogram
h <- hist(precip$mean_precip,                                         
     col="darkgray") 

bin_2011 <- cut(p_2011$w, h$breaks)
bin_2014 <- cut(p_2014$w, h$breaks)
bin_2018 <- cut(p_2018$w, h$breaks)

clr <- matrix(data = NA, nrow = 2, ncol=13)
  
clr[bin_2011] <- "red"
clr[bin_2014] <- "blue"
clr[bin_2018] <- "darkgreen"
plot(h, col=clr, 
     xlab="Total Precipitation",
     main="Histogram of the Average of Total Precipitation for Each year")

#add legend for vertical lines

legend("topright", 
       legend = c("Mean Precip in 2011 = 313 (mm)", 
                  "Mean Precip in 2014 = 346 (mm)", 
                  "Mean Precip in 2018 = 425 (mm)"),
       pch =c( "__", "__", "__"), col = c("red", "blue", "darkgreen"))