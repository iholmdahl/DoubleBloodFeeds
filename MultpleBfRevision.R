library(raster)
library(sp)
library(dplyr)
library(ggplot2)
library(hablar)
library(ggpubr)
library(expss)

source("./MordecaiR0.R")


##################################################################################################################
## IMPORT AND CLEAN DATA
##################################################################################################################

borders <- raster::shapefile("./spatial.data/Africa_SHP/Africa.shp")
borders.df <- fortify(borders)

## country border file doesn't include south sudan, so needs to be added separately
SouthSudan<-getData('GADM', country='SSD', level=0)  ##Get national border for south sudan
SouthSudan.df <- fortify(SouthSudan)


An.extent = raster('./spatial.data/2017_Anopheles_gambiae_complex.Mean_Decompressed.geotiff')
An.extent <- setMinMax(An.extent) %>%               
  mask(borders)       ## crops raster to extent of borders file (not mapping outside of Africa)

R0data <- An.extent %>%
  as.data.frame(xy=TRUE) %>%
  rename(Anopheles = X2017_Anopheles_gambiae_complex.Mean_Decompressed) %>%
  filter(!is.na(Anopheles))

pop <- raster('/Users/Inga/Documents/Harvard/Research/multiple.bf/spatial.data/AFR_PPP_2020_adj_v2.tif')

R0data <- pop %>%
  setMinMax() %>%
  aggregate(fact=(res(An.extent)[1]/res(pop)[1]), fun=sum) %>%    ## lower the resolution to match temperature file (sum because these are population counts)
  resample(An.extent, method = 'bilinear') %>%                    ## resample so that pixels match Anopheles extent map -- bilinear averages 4 nearest cells
  mask(An.extent) %>%                                             ## everything outside of Anopheles extent replaced with NA
  as.data.frame(xy=TRUE) %>%
  rename(population=AFR_PPP_2020_adj_v2) %>%
  right_join(R0data)


#######################################################################################################
## IMPORT AND CLEAN MONTHLY TEMPERATURE DATA
#######################################################################################################

R0data<- raster("spatial.data/wc2.0_2.5m_tavg_01.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp01=wc2.0_2.5m_tavg_01) %>%
  right_join(R0data)

R0data<- raster("spatial.data/wc2.0_2.5m_tavg_02.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp02=wc2.0_2.5m_tavg_02) %>%
  right_join(R0data)

R0data<- raster("spatial.data/wc2.0_2.5m_tavg_03.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp03=wc2.0_2.5m_tavg_03) %>%
  right_join(R0data)

R0data<- raster("spatial.data/wc2.0_2.5m_tavg_04.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp04=wc2.0_2.5m_tavg_04) %>%
  right_join(R0data)

R0data<- raster("spatial.data/wc2.0_2.5m_tavg_05.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp05=wc2.0_2.5m_tavg_05) %>%
  right_join(R0data)

R0data<- raster("spatial.data/wc2.0_2.5m_tavg_06.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp06=wc2.0_2.5m_tavg_06) %>%
  right_join(R0data)

R0data<- raster("spatial.data/wc2.0_2.5m_tavg_07.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp07=wc2.0_2.5m_tavg_07) %>%
  right_join(R0data)

R0data<- raster("spatial.data/wc2.0_2.5m_tavg_08.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp08=wc2.0_2.5m_tavg_08) %>%
  right_join(R0data)

R0data<- raster("spatial.data/wc2.0_2.5m_tavg_09.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp09=wc2.0_2.5m_tavg_09) %>%
  right_join(R0data)

R0data<- raster("spatial.data/wc2.0_2.5m_tavg_10.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp10=wc2.0_2.5m_tavg_10) %>%
  right_join(R0data)

R0data<- raster("spatial.data/wc2.0_2.5m_tavg_11.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp11=wc2.0_2.5m_tavg_11) %>%
  right_join(R0data)

R0data<- raster("spatial.data/wc2.0_2.5m_tavg_12.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp12=wc2.0_2.5m_tavg_12) %>%
  right_join(R0data)


##################################################################################################################
## SET UP CONTINENT BACKGROUND FOR PLOTS
##################################################################################################################

background <- setMinMax(raster("spatial.data/wc2.0_5m_tavg_01.tif")) %>% 
  mask(borders) %>%
  as.data.frame(xy=TRUE) %>%
  filter(!is.na(wc2.0_5m_tavg_01))  %>%
  mutate(continent.fill = 0)
 

################################################################################################################
## RESTRICT TEMPERATURE VALUES (throw out temps outside of bounds)
################################################################################################################

R0data$count_months <- count_row_if(25 %thru% 29, R0data$temp01,R0data$temp02,R0data$temp03,R0data$temp04,
                                           R0data$temp05,R0data$temp06,R0data$temp07,R0data$temp08,R0data$temp09,
                                           R0data$temp10,R0data$temp11,R0data$temp12)
Tempcutoff.low <- 25
Tempcutoff.high <- 29

R0data <- R0data %>% 
  
  rationalize() %>%
  
  mutate(temp01 = ifelse(temp01>Tempcutoff.low & temp01<Tempcutoff.high, temp01, NA), 
         temp02 = ifelse(temp02>Tempcutoff.low & temp02<Tempcutoff.high, temp02, NA), 
         temp03 = ifelse(temp03>Tempcutoff.low & temp03<Tempcutoff.high, temp03, NA), 
         temp04 = ifelse(temp04>Tempcutoff.low & temp04<Tempcutoff.high, temp04, NA), 
         temp05 = ifelse(temp05>Tempcutoff.low & temp05<Tempcutoff.high, temp05, NA), 
         temp06 = ifelse(temp06>Tempcutoff.low & temp06<Tempcutoff.high, temp06, NA), 
         temp07 = ifelse(temp07>Tempcutoff.low & temp07<Tempcutoff.high, temp07, NA), 
         temp08 = ifelse(temp08>Tempcutoff.low & temp08<Tempcutoff.high, temp08, NA), 
         temp09 = ifelse(temp09>Tempcutoff.low & temp09<Tempcutoff.high, temp09, NA), 
         temp10 = ifelse(temp10>Tempcutoff.low & temp10<Tempcutoff.high, temp10, NA), 
         temp11 = ifelse(temp11>Tempcutoff.low & temp11<Tempcutoff.high, temp11, NA), 
         temp12 = ifelse(temp12>Tempcutoff.low & temp12<Tempcutoff.high, temp12, NA))
  

################################################################################################################
## CALCULATE R0 RATIO FOR EACH MONTH
################################################################################################################

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


################################################################################################################
## CALCULATE MEAN R0 RATIO FOR EACH LOCATION
################################################################################################################

R0data <- R0data %>%
  rowwise() %>%
  mutate(Mordecai.mean = mean(c(MordecaiR0.01, MordecaiR0.02, MordecaiR0.03,MordecaiR0.04, MordecaiR0.05, MordecaiR0.06,
                                MordecaiR0.07, MordecaiR0.08, MordecaiR0.09, MordecaiR0.10, MordecaiR0.11, MordecaiR0.12), na.rm=TRUE), 
         Adjusted.mean = mean(c(AdjustedR0.01, AdjustedR0.02, AdjustedR0.03,AdjustedR0.04, AdjustedR0.05, AdjustedR0.06,
                                AdjustedR0.07, AdjustedR0.08, AdjustedR0.09, AdjustedR0.10, AdjustedR0.11, AdjustedR0.12), na.rm=TRUE)) %>%
  mutate(meanRatio = Adjusted.mean/Mordecai.mean)


################################################################################################################
## FIND MIN AND MAX R0 RATIO FOR EACH LOCATION
################################################################################################################

R0data$maxRatio <- do.call(pmax, c(R0data[, c("R0Ratio.01", "R0Ratio.02", "R0Ratio.03", "R0Ratio.04", "R0Ratio.05", 
                                              "R0Ratio.06", "R0Ratio.07", "R0Ratio.08", "R0Ratio.09", "R0Ratio.10", 
                                              "R0Ratio.11", "R0Ratio.12")], list(na.rm=TRUE)))
R0data$minRatio <- do.call(pmin, c(R0data[, c("R0Ratio.01", "R0Ratio.02", "R0Ratio.03", "R0Ratio.04", "R0Ratio.05", 
                                              "R0Ratio.06", "R0Ratio.07", "R0Ratio.08", "R0Ratio.09", "R0Ratio.10", 
                                              "R0Ratio.11", "R0Ratio.12")], list(na.rm=TRUE)))


##############################################################################################################c
## Calculate summary measures 
##############################################################################################################
# first restrict to relevant area: 
total.pop <- pop %>%
  setMinMax() %>%
  as.data.frame(xy=TRUE) %>%
  select(AFR_PPP_2020_adj_v2) %>%
  sum(na.rm=TRUE)

R0data %>%
  filter(Anopheles>0.05) %>%
  filter(count_months>0)%>%
  select(population) %>%
  sum(na.rm=TRUE) -> population.sum

population.sum

# mean R0 ratio (and min and max for the average)
R0data %>%
  filter(Anopheles>0.05) %>%
  filter(count_months>0)%>%
  select(meanRatio) %>%
  summary() -> meanSummary
  
# average max R0 ratio
R0data %>%
  filter(Anopheles>0.05) %>%
  filter(count_months>0)%>%
  select(maxRatio) %>%
  summary() -> maxSummary

# average min R0 ratio
R0data %>%
  filter(Anopheles>0.05) %>%
  filter(count_months>0)%>%
  select(minRatio) %>%
  summary() -> minSummary


##############################################################################################################c
## Figure 4 Revision
##############################################################################################################

Months <- R0data %>%
  filter(Anopheles>0.05) %>%
  filter(count_months>0)%>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = as.factor(count_months))) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="Months between 25-29C") +
  theme(text=element_text(size=14,  family="sans"))

MEAN  <- R0data %>%
  filter(count_months>0)%>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = meanRatio)) + 
  scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                       limits=c(1,1.13), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.13), 
                       labels=c("0%", "10%", "13%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="Mean") +
  theme(text=element_text(size=14,  family="sans"))

ggarrange(Months, MEAN, ncol=2, nrow=1)
ggsave("Figure4.pdf", width=8, height=4, units="in")


##############################################################################################################c
## Supplementary figure 1 (ratio vs temperature)
##############################################################################################################

Ratio.plot <- as.data.frame(matrix(data=NA, nrow=400, ncol=4))
Ratio.plot$V1 <- (1:400)/10
Ratio.plot[,2:4] <- t(apply(Ratio.plot[,1, drop=F], MARGIN = 1, R0temp))
colnames(Ratio.plot) <- c("temperature", "BaselineR0", "AdjustedR0", "R0ratio")

FigureS1.A <- Ratio.plot %>%
  filter(R0ratio<3) %>%
  ggplot() +
  geom_vline(xintercept = 25, linetype="dotted",  color = "#777777", size=0.5) + 
  geom_vline(xintercept = 29, linetype="dotted",  color = "#777777", size=0.5) + 
  geom_line(aes(x=temperature, y=BaselineR0, color="Baseline")) + 
  geom_line(aes(x=temperature, y=AdjustedR0, color="Adjusted")) +
  theme_classic() +
  scale_x_continuous(name = "Temperature (C)", breaks=c(25, 29))+
  scale_y_continuous(name="Scaled R0")+
  #coord_cartesian(ylim=c(0.5, 1.2), xlim =c(16, 36))+
  scale_color_manual(name = "",
                     breaks = c("Baseline", "Adjusted", "Ratio"),
                     values = c(Baseline = "#FCA636FF", Adjusted = "#B12A90FF", Ratio= "black"))

FigureS1.B <- Ratio.plot %>%
  filter(R0ratio<3) %>%
  ggplot() +
  geom_vline(xintercept = 25, linetype="dotted",  color = "#777777", size=0.5) + 
  geom_vline(xintercept = 29, linetype="dotted",  color = "#777777", size=0.5) + 
  #geom_hline(yintercept = c(0.5, 0.6, 0.7),  color = "black", size=0.2)+
  geom_line(aes(x=temperature, y=R0ratio, color="Ratio")) + 
  theme_classic() +
  scale_x_continuous(name = "Temperature (C)", breaks=c(25, 29))+
  scale_y_continuous(breaks=c(1,1.1,1.2, 1.3), labels=c("0", "10%", "20%",), name="R0 Increase")+
  coord_cartesian(ylim=c(1, 1.4), xlim =c(16, 36))+
  scale_color_manual(name = "",
                     breaks = c("Baseline", "Adjusted", "Ratio"),
                     values = c(Baseline = "#FCA636FF", Adjusted = "#B12A90FF", Ratio= "black"))

ggarrange(FigureS1.A, FigureS1.B, nrow=2, ncol=1)
ggsave("FigureS1.pdf", units=c("in"), height=6, width =6)



##############################################################################################################c
## Supplementary figure 2 monthly plots
##############################################################################################################


JAN <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.01)) + 
  scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                       limits=c(1,1.13), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.13), 
                       labels=c("0%", "10%", "13%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="January") + 
  theme(legend.key.height = unit(2, "cm"))

FEB <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.02)) + 
  scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                       limits=c(1,1.13), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.13), 
                       labels=c("0%", "10%", "13%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="February") + 
  theme(legend.key.height = unit(2, "cm"))

MAR <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.03)) + 
  scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                       limits=c(1,1.13), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.13), 
                       labels=c("0%", "10%", "13%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="March") + 
  theme(legend.key.height = unit(2, "cm"))

APR <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.04)) + 
  scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                       limits=c(1,1.13), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.13), 
                       labels=c("0%", "10%", "13%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="April") + 
  theme(legend.key.height = unit(2, "cm"))

MAY <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.05)) + 
  scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                       limits=c(1,1.13), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.13), 
                       labels=c("0%", "10%", "13%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="May") + 
  theme(legend.key.height = unit(2, "cm"))

JUN <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.06)) + 
  scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                       limits=c(1,1.13), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.13), 
                       labels=c("0%", "10%", "13%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="June") + 
  theme(legend.key.height = unit(2, "cm"))

JUL <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.07)) + 
  scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                       limits=c(1,1.13), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.13), 
                       labels=c("0%", "10%", "13%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="July") + 
  theme(legend.key.height = unit(2, "cm"))

AUG <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.08)) + 
  scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                       limits=c(1,1.13), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.13), 
                       labels=c("0%", "10%", "13%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="August") + 
  theme(legend.key.height = unit(2, "cm"))

SEP <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.09)) + 
  scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                       limits=c(1,1.13), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.13), 
                       labels=c("0%", "10%", "13%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="September") + 
  theme(legend.key.height = unit(2, "cm"))

OCT <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.10)) + 
  scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                       limits=c(1,1.13), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.13), 
                       labels=c("0%", "10%", "13%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="October") + 
  theme(legend.key.height = unit(2, "cm"))

NOV <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.11)) + 
  scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                       limits=c(1,1.13), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.13), 
                       labels=c("0%", "10%", "13%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="November") + 
  theme(legend.key.height = unit(2, "cm"))

DEC <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.12)) + 
  scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                       limits=c(1,1.13), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.13), 
                       labels=c("0%", "10%", "13%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="December") + 
  theme(legend.key.height = unit(2, "cm"))

ggarrange(JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC, ncol=4, nrow=3, common.legend = TRUE, legend="right")
ggsave("FigureS2.pdf", width=7.5, height=6.25, units="in")



