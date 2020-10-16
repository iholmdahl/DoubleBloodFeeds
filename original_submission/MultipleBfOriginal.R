library(raster)
library(rgdal)
library(sp)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(data.table)
library(hablar)
library(ggpubr)
library(farver)


##
## IMPORT AND CLEAN BORDERS
##

borders <- raster::shapefile("./data/Africa_SHP/Africa.shp")
borders.df <- fortify(borders)

## country border file doesn't include south sudan, so needs to be added separately
SouthSudan<-getData('GADM', country='SSD', level=0)  ##Get national border for south sudan
SouthSudan.df <- fortify(SouthSudan)


##
## IMPORT AND CLEAN ANOPHELES EXTENT -- THIS WILL BE OUR REFERENCE MAP
##

An.extent = raster('./data/2017_Anopheles_gambiae_complex.Mean_Decompressed.geotiff')
An.extent <- setMinMax(An.extent) %>%               
             mask(borders)       ## crops raster to extent of borders file (not mapping outside of Africa)

R0data <- An.extent %>%
  as.data.frame(xy=TRUE) %>%
  rename(Anopheles = X2017_Anopheles_gambiae_complex.Mean_Decompressed) %>%
  filter(!is.na(Anopheles))


##
## IMPORT AND CLEAN POPULATION DATA
##

pop <- raster('./data/AFR_PPP_2015_adj_v2.tif')

R0data.pop <- pop %>%
  setMinMax() %>%
  aggregate(fact=(res(An.extent)[1]/res(pop)[1]), fun=sum) %>%    ## lower the resolution to match temperature file (sum because these are population counts)
  resample(An.extent, method = 'bilinear') %>%                    ## resample so that pixels match Anopheles extent map -- bilinear averages 4 nearest cells
  mask(An.extent) %>%                                             ## everything outside of Anopheles extent replaced with NA
  as.data.frame(xy=TRUE) %>%
  rename(population=AFR_PPP_2015_adj_v2) %>%
  right_join(R0data)


##
## IMPORT AND CLEAN MONTHLY TEMPERATURE DATA
##

R0data<- raster("./data/wc2.0_2.5m_tavg_01.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp01=wc2.0_2.5m_tavg_01) %>%
  right_join(R0data)

R0data<- raster("./data/wc2.0_2.5m_tavg_02.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp02=wc2.0_2.5m_tavg_02) %>%
  right_join(R0data)

R0data<- raster("./data/wc2.0_2.5m_tavg_03.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp03=wc2.0_2.5m_tavg_03) %>%
  right_join(R0data)

R0data<- raster("./data/wc2.0_2.5m_tavg_04.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp04=wc2.0_2.5m_tavg_04) %>%
  right_join(R0data)

R0data<- raster("./data/wc2.0_2.5m_tavg_05.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp05=wc2.0_2.5m_tavg_05) %>%
  right_join(R0data)

R0data<- raster("./data/wc2.0_2.5m_tavg_06.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp06=wc2.0_2.5m_tavg_06) %>%
  right_join(R0data)

R0data<- raster("./data/wc2.0_2.5m_tavg_07.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp07=wc2.0_2.5m_tavg_07) %>%
  right_join(R0data)

R0data<- raster("./data/wc2.0_2.5m_tavg_08.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp08=wc2.0_2.5m_tavg_08) %>%
  right_join(R0data)

R0data<- raster("./data/wc2.0_2.5m_tavg_09.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp09=wc2.0_2.5m_tavg_09) %>%
  right_join(R0data)

R0data<- raster("./data/wc2.0_2.5m_tavg_10.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp10=wc2.0_2.5m_tavg_10) %>%
  right_join(R0data)

R0data<- raster("./data/wc2.0_2.5m_tavg_11.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp11=wc2.0_2.5m_tavg_11) %>%
  right_join(R0data)

R0data<- raster("./data/wc2.0_2.5m_tavg_12.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp12=wc2.0_2.5m_tavg_12) %>%
  right_join(R0data)


## calculate average temperature for each cell 
R0data <- R0data %>%
  mutate(meantemp = (temp01+temp02+temp03+temp04+temp05+temp06+temp07+temp08+temp09+temp10+temp11+temp12)/12)


##
## SET UP CONTINENT BACKGROUND FOR PLOTS
##

background <- setMinMax(raster("./data/wc2.0_5m_tavg_01.tif")) %>% 
  mask(borders) %>%
  as.data.frame(xy=TRUE) %>%
  filter(!is.na(wc2.0_5m_tavg_01))  %>%
  mutate(continent.fill = 0)



##
## APPLY R0temp FUNCTION TO EACH MONTH'S TEMP 
##

R0data <- cbind(R0data, t(apply(R0data["temp01"], MARGIN = 1, R0temp)))
colnames(R0data)[(ncol(R0data)-2):ncol(R0data)] <- c("MordecaiR0.01", "AdjustedR0.01", "R0Ratio.01")

R0data <- cbind(R0data, t(apply(R0data["temp02"], MARGIN = 1, R0temp)))
colnames(R0data)[(ncol(R0data)-2):ncol(R0data)] <- c("MordecaiR0.02", "AdjustedR0.02", "R0Ratio.02")

R0data <- cbind(R0data, t(apply(R0data["temp03"], MARGIN = 1, R0temp)))
colnames(R0data)[(ncol(R0data)-2):ncol(R0data)] <- c("MordecaiR0.03", "AdjustedR0.03", "R0Ratio.03")

R0data <- cbind(R0data, t(apply(R0data["temp04"], MARGIN = 1, R0temp)))
colnames(R0data)[(ncol(R0data)-2):ncol(R0data)] <- c("MordecaiR0.04", "AdjustedR0.04", "R0Ratio.04")

R0data <- cbind(R0data, t(apply(R0data["temp05"], MARGIN = 1, R0temp)))
colnames(R0data)[(ncol(R0data)-2):ncol(R0data)] <- c("MordecaiR0.05", "AdjustedR0.05", "R0Ratio.05")

R0data <- cbind(R0data, t(apply(R0data["temp06"], MARGIN = 1, R0temp)))
colnames(R0data)[(ncol(R0data)-2):ncol(R0data)] <- c("MordecaiR0.06", "AdjustedR0.06", "R0Ratio.06")

R0data <- cbind(R0data, t(apply(R0data["temp07"], MARGIN = 1, R0temp)))
colnames(R0data)[(ncol(R0data)-2):ncol(R0data)] <- c("MordecaiR0.07", "AdjustedR0.07", "R0Ratio.07")

R0data <- cbind(R0data, t(apply(R0data["temp08"], MARGIN = 1, R0temp)))
colnames(R0data)[(ncol(R0data)-2):ncol(R0data)] <- c("MordecaiR0.08", "AdjustedR0.08", "R0Ratio.08")

R0data <- cbind(R0data, t(apply(R0data["temp09"], MARGIN = 1, R0temp)))
colnames(R0data)[(ncol(R0data)-2):ncol(R0data)] <- c("MordecaiR0.09", "AdjustedR0.09", "R0Ratio.09")

R0data <- cbind(R0data, t(apply(R0data["temp10"], MARGIN = 1, R0temp)))
colnames(R0data)[(ncol(R0data)-2):ncol(R0data)] <- c("MordecaiR0.10", "AdjustedR0.10", "R0Ratio.10")

R0data <- cbind(R0data, t(apply(R0data["temp11"], MARGIN = 1, R0temp)))
colnames(R0data)[(ncol(R0data)-2):ncol(R0data)] <- c("MordecaiR0.11", "AdjustedR0.11", "R0Ratio.11")

R0data <- cbind(R0data, t(apply(R0data["temp12"], MARGIN = 1, R0temp)))
colnames(R0data)[(ncol(R0data)-2):ncol(R0data)] <- c("MordecaiR0.12", "AdjustedR0.12", "R0Ratio.12")


## calculate average R0 across the year for each cell 
R0data <- R0data %>%
  mutate(meanMordecaiR0 = (MordecaiR0.01+MordecaiR0.02+MordecaiR0.03+MordecaiR0.04+MordecaiR0.05+MordecaiR0.06
                        +MordecaiR0.07+MordecaiR0.08+MordecaiR0.09+MordecaiR0.10+MordecaiR0.11+MordecaiR0.12)/12)
R0data <- R0data %>%
  mutate(meanAdjustedR0 = (AdjustedR0.01+AdjustedR0.02+AdjustedR0.03+AdjustedR0.04+AdjustedR0.05+AdjustedR0.06
                           +AdjustedR0.07+AdjustedR0.08+AdjustedR0.09+AdjustedR0.10+AdjustedR0.11+AdjustedR0.12)/12)
R0data <- R0data %>%
  mutate(meanR0Ratio = meanAdjustedR0/meanMordecaiR0)



##
## RUN R0 FUNCTION OVER POPULATION *AND* TEMPERATURE DATA
##

## set minimum population to 200 (make pop.temp column where any cell with population<250 <- 250)
R0data$population.temp <- R0data$population
R0data$population.temp[R0data$population.temp < 500] <- 500


##
## apply R0temppop to monthly temp, mean population
##

## JAN
R0.estimates <- mapply(R0temppop, R0data$temp01, R0data$population.temp, SIMPLIFY=FALSE)
R0.est <- as.data.frame(matrix(unlist(R0.estimates), nrow=length(R0.estimates), byrow=T))

R0data <- cbind(R0data, R0.est[c(1:2)])
colnames(R0data)[(ncol(R0data)-1):ncol(R0data)] <- c("pop500MordecaiR0.01", "pop500AdjustedR0.01")


## FEB
R0.estimates <- mapply(R0temppop, R0data$temp02, R0data$population.temp, SIMPLIFY=FALSE)
R0.estimates <- as.data.frame(matrix(unlist(R0.estimates), nrow=length(R0.estimates), byrow=T))

R0data <- cbind(R0data, R0.estimates[c(1:2)])
colnames(R0data)[(ncol(R0data)-1):ncol(R0data)] <- c("pop500MordecaiR0.02", "pop500AdjustedR0.02")


## MAR
R0.estimates <- mapply(R0temppop, R0data$temp03, R0data$population.temp, SIMPLIFY=FALSE)
R0.estimates <- as.data.frame(matrix(unlist(R0.estimates), nrow=length(R0.estimates), byrow=T))

R0data <- cbind(R0data, R0.estimates[c(1:2)])
colnames(R0data)[(ncol(R0data)-1):ncol(R0data)] <- c("pop500MordecaiR0.03", "pop500AdjustedR0.03")


## APR
R0.estimates <- mapply(R0temppop, R0data$temp04, R0data$population.temp, SIMPLIFY=FALSE)
R0.estimates <- as.data.frame(matrix(unlist(R0.estimates), nrow=length(R0.estimates), byrow=T))

R0data <- cbind(R0data, R0.estimates[c(1:2)])
colnames(R0data)[(ncol(R0data)-1):ncol(R0data)] <- c("pop500MordecaiR0.04", "pop500AdjustedR0.04")


## MAY
R0.estimates <- mapply(R0temppop, R0data$temp05, R0data$population.temp, SIMPLIFY=FALSE)
R0.estimates <- as.data.frame(matrix(unlist(R0.estimates), nrow=length(R0.estimates), byrow=T))

R0data <- cbind(R0data, R0.estimates[c(1:2)])
colnames(R0data)[(ncol(R0data)-1):ncol(R0data)] <- c("pop500MordecaiR0.05", "pop500AdjustedR0.05")


## JUN
R0.estimates <- mapply(R0temppop, R0data$temp06, R0data$population.temp, SIMPLIFY=FALSE)
R0.estimates <- as.data.frame(matrix(unlist(R0.estimates), nrow=length(R0.estimates), byrow=T))

R0data <- cbind(R0data, R0.estimates[c(1:2)])
colnames(R0data)[(ncol(R0data)-1):ncol(R0data)] <- c("pop500MordecaiR0.06", "pop500AdjustedR0.06")


## JUL
R0.estimates <- mapply(R0temppop, R0data$temp07, R0data$population.temp, SIMPLIFY=FALSE)
R0.estimates <- as.data.frame(matrix(unlist(R0.estimates), nrow=length(R0.estimates), byrow=T))

R0data <- cbind(R0data, R0.estimates[c(1:2)])
colnames(R0data)[(ncol(R0data)-1):ncol(R0data)] <- c("pop500MordecaiR0.07", "pop500AdjustedR0.07")


## AUG
R0.estimates <- mapply(R0temppop, R0data$temp08, R0data$population.temp, SIMPLIFY=FALSE)
R0.estimates <- as.data.frame(matrix(unlist(R0.estimates), nrow=length(R0.estimates), byrow=T))

R0data <- cbind(R0data, R0.estimates[c(1:2)])
colnames(R0data)[(ncol(R0data)-1):ncol(R0data)] <- c("pop500MordecaiR0.08", "pop500AdjustedR0.08")


## SEP
R0.estimates <- mapply(R0temppop, R0data$temp09, R0data$population.temp, SIMPLIFY=FALSE)
R0.estimates <- as.data.frame(matrix(unlist(R0.estimates), nrow=length(R0.estimates), byrow=T))

R0data <- cbind(R0data, R0.estimates[c(1:2)])
colnames(R0data)[(ncol(R0data)-1):ncol(R0data)] <- c("pop500MordecaiR0.09", "pop500AdjustedR0.09")


## OCT
R0.estimates <- mapply(R0temppop, R0data$temp10, R0data$population.temp, SIMPLIFY=FALSE)
R0.estimates <- as.data.frame(matrix(unlist(R0.estimates), nrow=length(R0.estimates), byrow=T))

R0data <- cbind(R0data, R0.estimates[c(1:2)])
colnames(R0data)[(ncol(R0data)-1):ncol(R0data)] <- c("pop500MordecaiR0.10", "pop500AdjustedR0.10")


## NOV
R0.estimates <- mapply(R0temppop, R0data$temp11, R0data$population.temp, SIMPLIFY=FALSE)
R0.estimates <- as.data.frame(matrix(unlist(R0.estimates), nrow=length(R0.estimates), byrow=T))

R0data <- cbind(R0data, R0.estimates[c(1:2)])
colnames(R0data)[(ncol(R0data)-1):ncol(R0data)] <- c("pop500MordecaiR0.11", "pop500AdjustedR0.11")


## DEC
R0.estimates <- mapply(R0temppop, R0data$temp12, R0data$population.temp, SIMPLIFY=FALSE)
R0.estimates <- as.data.frame(matrix(unlist(R0.estimates), nrow=length(R0.estimates), byrow=T))

R0data <- cbind(R0data, R0.estimates[c(1:2)])
colnames(R0data)[(ncol(R0data)-1):ncol(R0data)] <- c("pop500MordecaiR0.12", "pop500AdjustedR0.12")



##
## CALCULATE ANNUAL MEANS
##

R0data <- R0data %>%
  mutate(meanMordecaiR0.pop500 = (pop500MordecaiR0.01 + pop500MordecaiR0.02 + pop500MordecaiR0.03 + pop500MordecaiR0.04 + pop500MordecaiR0.05 + pop500MordecaiR0.06
                           + pop500MordecaiR0.07 + pop500MordecaiR0.08 + pop500MordecaiR0.09 + pop500MordecaiR0.10 + pop500MordecaiR0.11 + pop500MordecaiR0.12)/12)
R0data <- R0data %>%
  mutate(meanAdjustedR0.pop500 = (pop500AdjustedR0.01 + pop500AdjustedR0.02 + pop500AdjustedR0.03 + pop500AdjustedR0.04 + pop500AdjustedR0.05 + pop500AdjustedR0.06
                           + pop500AdjustedR0.07 + pop500AdjustedR0.08 + pop500AdjustedR0.09 + pop500AdjustedR0.10 + pop500AdjustedR0.11 + pop500AdjustedR0.12)/12)
summary(R0data$meanMordecaiR0.pop500)

R0data <- R0data %>%
  mutate(meanMordecaiR0.scaled500 = meanMordecaiR0.pop500 * Anopheles, 
         meanAdjustedR0.scaled500 = meanAdjustedR0.pop500 * Anopheles)



## remove columns by name: R0data[,c("popdensityAdjusted", "popdensityMordecai")] <- list(NULL)
## REORDER COLUMNS for convenience: R0data <- R0data[c(1:2, 14:3, 15:85)]
## ==save: write.csv(R0data, file="R0data.csv")


##
## CALCULATE MEAN, MIN, MAX USING RESTRICTED VALUES (ONLY WHEN ADJUSTED R0>1)
##

R0cutoff <- 1
R0data.temp <- R0data %>% 
  
  rationalize() %>%
  
  select(popMordecaiR0.01, popAdjustedR0.01, R0Ratio.01, 
         popMordecaiR0.02, popAdjustedR0.02, R0Ratio.02, 
         popMordecaiR0.03, popAdjustedR0.03, R0Ratio.03, 
         popMordecaiR0.04, popAdjustedR0.04, R0Ratio.04, 
         popMordecaiR0.05, popAdjustedR0.05, R0Ratio.05, 
         popMordecaiR0.06, popAdjustedR0.06, R0Ratio.06, 
         popMordecaiR0.07, popAdjustedR0.07, R0Ratio.07, 
         popMordecaiR0.08, popAdjustedR0.08, R0Ratio.08, 
         popMordecaiR0.09, popAdjustedR0.09, R0Ratio.09, 
         popMordecaiR0.10, popAdjustedR0.10, R0Ratio.10, 
         popMordecaiR0.11, popAdjustedR0.11, R0Ratio.11, 
         popMordecaiR0.12, popAdjustedR0.12, R0Ratio.12) %>%
  
  mutate(R0Ratio.01 = ifelse(popAdjustedR0.01>R0cutoff, R0Ratio.01, NA), 
         R0Ratio.02 = ifelse(popAdjustedR0.02>R0cutoff, R0Ratio.02, NA), 
         R0Ratio.03 = ifelse(popAdjustedR0.03>R0cutoff, R0Ratio.03, NA), 
         R0Ratio.04 = ifelse(popAdjustedR0.04>R0cutoff, R0Ratio.04, NA), 
         R0Ratio.05 = ifelse(popAdjustedR0.05>R0cutoff, R0Ratio.05, NA), 
         R0Ratio.06 = ifelse(popAdjustedR0.06>R0cutoff, R0Ratio.06, NA), 
         R0Ratio.07 = ifelse(popAdjustedR0.07>R0cutoff, R0Ratio.07, NA), 
         R0Ratio.08 = ifelse(popAdjustedR0.08>R0cutoff, R0Ratio.08, NA), 
         R0Ratio.09 = ifelse(popAdjustedR0.09>R0cutoff, R0Ratio.09, NA), 
         R0Ratio.10 = ifelse(popAdjustedR0.10>R0cutoff, R0Ratio.10, NA), 
         R0Ratio.11 = ifelse(popAdjustedR0.11>R0cutoff, R0Ratio.11, NA), 
         R0Ratio.12 = ifelse(popAdjustedR0.12>R0cutoff, R0Ratio.12, NA), 
         popMordecaiR0.01 = ifelse(popAdjustedR0.01>R0cutoff, popMordecaiR0.01, NA),
         popMordecaiR0.02 = ifelse(popAdjustedR0.02>R0cutoff, popMordecaiR0.02, NA),
         popMordecaiR0.03 = ifelse(popAdjustedR0.03>R0cutoff, popMordecaiR0.03, NA),
         popMordecaiR0.04 = ifelse(popAdjustedR0.04>R0cutoff, popMordecaiR0.04, NA),
         popMordecaiR0.05 = ifelse(popAdjustedR0.05>R0cutoff, popMordecaiR0.05, NA),
         popMordecaiR0.06 = ifelse(popAdjustedR0.06>R0cutoff, popMordecaiR0.06, NA),
         popMordecaiR0.07 = ifelse(popAdjustedR0.07>R0cutoff, popMordecaiR0.07, NA),
         popMordecaiR0.08 = ifelse(popAdjustedR0.08>R0cutoff, popMordecaiR0.08, NA),
         popMordecaiR0.09 = ifelse(popAdjustedR0.09>R0cutoff, popMordecaiR0.09, NA),
         popMordecaiR0.10 = ifelse(popAdjustedR0.10>R0cutoff, popMordecaiR0.10, NA),
         popMordecaiR0.11 = ifelse(popAdjustedR0.11>R0cutoff, popMordecaiR0.11, NA),
         popMordecaiR0.12 = ifelse(popAdjustedR0.12>R0cutoff, popMordecaiR0.12, NA),
         popAdjustedR0.01 = ifelse(popAdjustedR0.01>R0cutoff, popAdjustedR0.01, NA),
         popAdjustedR0.02 = ifelse(popAdjustedR0.02>R0cutoff, popAdjustedR0.02, NA),
         popAdjustedR0.03 = ifelse(popAdjustedR0.03>R0cutoff, popAdjustedR0.03, NA),
         popAdjustedR0.04 = ifelse(popAdjustedR0.04>R0cutoff, popAdjustedR0.04, NA),
         popAdjustedR0.05 = ifelse(popAdjustedR0.05>R0cutoff, popAdjustedR0.05, NA),
         popAdjustedR0.06 = ifelse(popAdjustedR0.06>R0cutoff, popAdjustedR0.06, NA),
         popAdjustedR0.07 = ifelse(popAdjustedR0.07>R0cutoff, popAdjustedR0.07, NA),
         popAdjustedR0.08 = ifelse(popAdjustedR0.08>R0cutoff, popAdjustedR0.08, NA),
         popAdjustedR0.09 = ifelse(popAdjustedR0.09>R0cutoff, popAdjustedR0.09, NA),
         popAdjustedR0.10 = ifelse(popAdjustedR0.10>R0cutoff, popAdjustedR0.10, NA),
         popAdjustedR0.11 = ifelse(popAdjustedR0.11>R0cutoff, popAdjustedR0.11, NA),
         popAdjustedR0.12 = ifelse(popAdjustedR0.12>R0cutoff, popAdjustedR0.12, NA)) %>%
  
  rowwise() %>%
  mutate(Mordecai.mean = mean(c(popMordecaiR0.01, popMordecaiR0.02, popMordecaiR0.03,popMordecaiR0.04, popMordecaiR0.05, popMordecaiR0.06,
                                popMordecaiR0.07, popMordecaiR0.08, popMordecaiR0.09, popMordecaiR0.10, popMordecaiR0.11, popMordecaiR0.12), na.rm=TRUE), 
         Adjusted.mean = mean(c(popAdjustedR0.01, popAdjustedR0.02, popAdjustedR0.03,popAdjustedR0.04, popAdjustedR0.05, popAdjustedR0.06,
                                popAdjustedR0.07, popAdjustedR0.08, popAdjustedR0.09, popAdjustedR0.10, popAdjustedR0.11, popAdjustedR0.12), na.rm=TRUE)) %>%
  mutate(meanRatio = Adjusted.mean/Mordecai.mean)

  
## plot to test that the correct data was discarded
R0data %>%
  ggplot(aes(x=R0Ratio.06, y=popMordecaiR0.06)) + 
  geom_point()+
  scale_y_continuous(limits=c(0,1)) + 
  scale_x_continuous(limits=c(0,4)) +
  geom_point(data=R0data.temp, aes(x=R0Ratio.06, y=popMordecaiR0.06), color="red")
  
## add new data to working data frame
R0data.temp$maxRatio <- do.call(pmax, c(R0data.temp[, c("R0Ratio.01", "R0Ratio.02", "R0Ratio.03", "R0Ratio.04", "R0Ratio.05", "R0Ratio.06", 
                                                        "R0Ratio.07", "R0Ratio.08", "R0Ratio.09", "R0Ratio.10", "R0Ratio.11", "R0Ratio.12")], list(na.rm=TRUE)))
R0data.temp$minRatio <- do.call(pmin, c(R0data.temp[, c("R0Ratio.01", "R0Ratio.02", "R0Ratio.03", "R0Ratio.04", "R0Ratio.05", "R0Ratio.06", 
                                                        "R0Ratio.07", "R0Ratio.08", "R0Ratio.09", "R0Ratio.10", "R0Ratio.11", "R0Ratio.12")], list(na.rm=TRUE)))

R0data <- cbind(R0data, R0data.temp[,c("maxRatio.1", "minRatio.1", "meanRatio", "Adjusted.mean", "Mordecai.mean")])


#
##  QUANTIFY CHANGE IN TERMS OF POPULATION SIZE
## 15% change from mean map:

totalpopulation <- R0data %>%
  filter(Anopheles>0.05) %>%
  select(population) %>%
  sum(na.rm=TRUE)

meanpop.85 <- R0data %>%
  filter(Anopheles>0.05) %>%
  filter(meanRatio < 1.15) %>%
  select(population) %>%
  sum(na.rm=TRUE)

meanpop.15 = totalpopulation - meanpop.85


## 25% change from max map: 

maxpop.75 <- R0data %>%
  filter(Anopheles>0.05) %>%
  filter(maxRatio < 1.25) %>%
  select(population) %>%
  sum(na.rm=TRUE)

maxpop.25 = totalpopulation - maxpop.75


R0data2 <- R0data %>%
  select(x, y, minRatio.1, meanRatio, maxRatio.1, population, Anopheles)

saveRDS(R0data2, "R0data.rds")

R0data <- readRDS("R0data.rds")


