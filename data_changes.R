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

borders <- raster::shapefile("./data/Africa.shp")
borders.df <- fortify(borders)

## country border file doesn't include south sudan, so needs to be added separately
SouthSudan<-getData('GADM', country='SSD', level=0)  ##Get national border for south sudan
SouthSudan.df <- fortify(SouthSudan)


An.extent = raster('./data/2017_Anopheles_gambiae_complex.Mean_Decompressed.geotiff')
An.extent <- setMinMax(An.extent) %>%               
  mask(borders)       ## crops raster to extent of borders file (not mapping outside of Africa)

R0data_temp <- An.extent %>%
  as.data.frame(xy=TRUE) %>%
  rename(Anopheles = X2017_Anopheles_gambiae_complex.Mean_Decompressed) %>%
  filter(!is.na(Anopheles))

pop <- raster('./data/AFR_PPP_2020_adj_v2.tif')

R0data_temp <- pop %>%
  setMinMax() %>%
  aggregate(fact=(res(An.extent)[1]/res(pop)[1]), fun=sum) %>%    ## lower the resolution to match temperature file (sum because these are population counts)
  resample(An.extent, method = 'bilinear') %>%                    ## resample so that pixels match Anopheles extent map -- bilinear averages 4 nearest cells
  mask(An.extent) %>%                                             ## everything outside of Anopheles extent replaced with NA
  as.data.frame(xy=TRUE) %>%
  rename(population=AFR_PPP_2020_adj_v2) %>%
  right_join(R0data_temp)


#######################################################################################################
## IMPORT AND CLEAN MONTHLY TEMPERATURE DATA
#######################################################################################################

R0data_temp<- raster("./data/wc2.0_2.5m_tavg_01.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp01=wc2.0_2.5m_tavg_01) %>%
  right_join(R0data_temp)

R0data_temp<- raster("./data/wc2.0_2.5m_tavg_02.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp02=wc2.0_2.5m_tavg_02) %>%
  right_join(R0data_temp)

R0data_temp<- raster("./data/wc2.0_2.5m_tavg_03.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp03=wc2.0_2.5m_tavg_03) %>%
  right_join(R0data_temp)

R0data_temp<- raster("./data/wc2.0_2.5m_tavg_04.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp04=wc2.0_2.5m_tavg_04) %>%
  right_join(R0data_temp)

R0data_temp<- raster("./data/wc2.0_2.5m_tavg_05.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp05=wc2.0_2.5m_tavg_05) %>%
  right_join(R0data_temp)

R0data_temp<- raster("./data/wc2.0_2.5m_tavg_06.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp06=wc2.0_2.5m_tavg_06) %>%
  right_join(R0data_temp)

R0data_temp<- raster("./data/wc2.0_2.5m_tavg_07.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp07=wc2.0_2.5m_tavg_07) %>%
  right_join(R0data_temp)

R0data_temp<- raster("./data/wc2.0_2.5m_tavg_08.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp08=wc2.0_2.5m_tavg_08) %>%
  right_join(R0data_temp)

R0data_temp<- raster("./data/wc2.0_2.5m_tavg_09.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp09=wc2.0_2.5m_tavg_09) %>%
  right_join(R0data_temp)

R0data_temp<- raster("./data/wc2.0_2.5m_tavg_10.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp10=wc2.0_2.5m_tavg_10) %>%
  right_join(R0data_temp)

R0data_temp<- raster("./data/wc2.0_2.5m_tavg_11.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp11=wc2.0_2.5m_tavg_11) %>%
  right_join(R0data_temp)

R0data_temp<- raster("./data/wc2.0_2.5m_tavg_12.tif") %>% 
  setMinMax() %>%
  resample(An.extent, method = 'bilinear') %>%
  mask(An.extent) %>%                                        
  as.data.frame(xy=TRUE) %>%
  rename(temp12=wc2.0_2.5m_tavg_12) %>%
  right_join(R0data_temp)


##################################################################################################################
## SET UP CONTINENT BACKGROUND FOR PLOTS
##################################################################################################################

background <- setMinMax(raster("./data/wc2.0_5m_tavg_01.tif")) %>% 
  mask(borders) %>%
  as.data.frame(xy=TRUE) %>%
  filter(!is.na(wc2.0_5m_tavg_01))  %>%
  mutate(continent.fill = 0)



#############################################################################################################c
## Map difference between temperatures for each month
##############################################################################################################

R0data_temp$difference01 <- R0data_revision$temp01 - R0data_temp$temp01
summary(R0data_temp$difference01)

JAN <- R0data_temp %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = difference01)) + 
  scale_fill_gradientn(limits=c(-1 , 3), oob=scales::squish,  na.value = "#EBEBEB", 
                       colours=c("#00b7ff", "#ffffff","#fc0810"), 
                       values = scales::rescale(c(-1, 0, 3))) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="January") + 
  theme(legend.key.height = unit(2, "cm"))


scale_fill_gradientn(colours = c("#F0F921", "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887"),
                     limits=c(1,1.13), oob=scales::squish, 
                     values = scales::rescale(c(1.0, 1.1, 1.11, 1.12, 1.13)),
                     guide = "colorbar",
                     breaks=c(1, 1.1, 1.13), 
                     labels=c("0%", "10%", "13%"), 
                     na.value = "#EBEBEB") 


R0data_temp$difference02 <- R0data_revision$temp02 - R0data_temp$temp02
summary(R0data_temp$difference02)

FEB <- R0data_temp %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = difference02)) + 
  scale_fill_gradientn(limits=c(-1 , 3), oob=scales::squish,  na.value = "#EBEBEB", 
                       colours=c("#00b7ff", "#ffffff","#fc0810"), 
                       values = scales::rescale(c(-1, 0, 3))) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="February") + 
  theme(legend.key.height = unit(2, "cm"))

R0data_temp$difference03 <- R0data_revision$temp03 - R0data_temp$temp03
summary(R0data_temp$difference03)

MAR <- R0data_temp %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = difference03)) + 
  scale_fill_gradientn(limits=c(-1 , 3), oob=scales::squish,  na.value = "#EBEBEB", 
                         colours=c("#00b7ff", "#ffffff","#fc0810"), 
                         values = scales::rescale(c(-1, 0, 3))) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="March") + 
  theme(legend.key.height = unit(2, "cm"))


R0data_temp$difference04 <- R0data_revision$temp04 - R0data_temp$temp04
summary(R0data_temp$difference04)

APR <- R0data_temp %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = difference04)) + 
  scale_fill_gradientn(limits=c(-1 , 3), oob=scales::squish,  na.value = "#EBEBEB", 
                       colours=c("#00b7ff", "#ffffff","#fc0810"), 
                       values = scales::rescale(c(-1, 0, 3))) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="April") + 
  theme(legend.key.height = unit(2, "cm"))

R0data_temp$difference05 <- R0data_revision$temp05 - R0data_temp$temp05
summary(R0data_temp$difference05)

MAY <- R0data_temp %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = difference05)) + 
  scale_fill_gradientn(limits=c(-1 , 3), oob=scales::squish,  na.value = "#EBEBEB", 
                       colours=c("#00b7ff", "#ffffff","#fc0810"), 
                       values = scales::rescale(c(-1, 0, 3))) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="May") + 
  theme(legend.key.height = unit(2, "cm"))

R0data_temp$difference06 <- R0data_revision$temp06 - R0data_temp$temp06
summary(R0data_temp$difference06)

JUN <- R0data_temp %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = difference06)) + 
  scale_fill_gradientn(limits=c(-1 , 3), oob=scales::squish,  na.value = "#EBEBEB", 
                       colours=c("#00b7ff", "#ffffff","#fc0810"), 
                       values = scales::rescale(c(-1, 0, 3))) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="June") + 
  theme(legend.key.height = unit(2, "cm"))

R0data_temp$difference07 <- R0data_revision$temp07 - R0data_temp$temp07
summary(R0data_temp$difference07)

JUL <- R0data_temp %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = difference07)) + 
  scale_fill_gradientn(limits=c(-1 , 3), oob=scales::squish,  na.value = "#EBEBEB", 
                       colours=c("#00b7ff", "#ffffff","#fc0810"), 
                       values = scales::rescale(c(-1, 0, 3))) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="July") + 
  theme(legend.key.height = unit(2, "cm"))

R0data_temp$difference08 <- R0data_revision$temp08 - R0data_temp$temp08
summary(R0data_temp$difference08)

AUG <- R0data_temp %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = difference08)) + 
  scale_fill_gradientn(limits=c(-1 , 3), oob=scales::squish,  na.value = "#EBEBEB", 
                       colours=c("#00b7ff", "#ffffff","#fc0810"), 
                       values = scales::rescale(c(-1, 0, 3))) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="August") + 
  theme(legend.key.height = unit(2, "cm"))

R0data_temp$difference09 <- R0data_revision$temp09 - R0data_temp$temp09
summary(R0data_temp$difference09)

SEP <- R0data_temp %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = difference09)) + 
  scale_fill_gradientn(limits=c(-1 , 3), oob=scales::squish,  na.value = "#EBEBEB", 
                       colours=c("#00b7ff", "#ffffff","#fc0810"), 
                       values = scales::rescale(c(-1, 0, 3))) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="September") + 
  theme(legend.key.height = unit(2, "cm"))

R0data_temp$difference10 <- R0data_revision$temp10 - R0data_temp$temp10
summary(R0data_temp$difference10)

OCT <- R0data_temp %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = difference10)) + 
  scale_fill_gradientn(limits=c(-1 , 3), oob=scales::squish,  na.value = "#EBEBEB", 
                       colours=c("#00b7ff", "#ffffff","#fc0810"), 
                       values = scales::rescale(c(-1, 0, 3))) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="October") + 
  theme(legend.key.height = unit(2, "cm"))

R0data_temp$difference11 <- R0data_revision$temp11 - R0data_temp$temp11
summary(R0data_temp$difference11)

NOV <- R0data_temp %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = difference11)) + 
  scale_fill_gradientn(limits=c(-1 , 3), oob=scales::squish,  na.value = "#EBEBEB", 
                       colours=c("#00b7ff", "#ffffff","#fc0810"), 
                       values = scales::rescale(c(-1, 0, 3))) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="November") + 
  theme(legend.key.height = unit(2, "cm"))

R0data_temp$difference12 <- R0data_revision$temp12 - R0data_temp$temp12
summary(R0data_temp$difference12)

DEC <- R0data_temp %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = difference12)) + 
  scale_fill_gradientn(limits=c(-1 , 3), oob=scales::squish,  na.value = "#EBEBEB", 
                       colours=c("#00b7ff", "#ffffff","#fc0810"), 
                       values = scales::rescale(c(-1, 0, 3))) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="December") + 
  theme(legend.key.height = unit(2, "cm"))

ggarrange(JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC, ncol=4, nrow=3, common.legend = TRUE, legend="right")
ggsave(paste0(Sys.Date(),"_Temp_change.pdf"), width=7.5, height=6.25, units="in")

################################################################################################################
## RESTRICT TEMPERATURE VALUES (throw out temps outside of bounds)
################################################################################################################

R0data_temp$count_months <- count_row_if(25 %thru% 29, R0data_temp$temp01,R0data_temp$temp02,R0data_temp$temp03,R0data_temp$temp04,
                                    R0data_temp$temp05,R0data_temp$temp06,R0data_temp$temp07,R0data_temp$temp08,R0data_temp$temp09,
                                    R0data_temp$temp10,R0data_temp$temp11,R0data_temp$temp12)
Tempcutoff.low <- 25
Tempcutoff.high <- 29

R0data_temp <- R0data_temp %>% 
  
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

R0data_temp <- cbind(R0data_temp, t(apply(R0data_temp["temp01"], MARGIN = 1, R0temp)))
colnames(R0data_temp)[(ncol(R0data_temp)-2):ncol(R0data_temp)] <- c("MordecaiR0.01", "AdjustedR0.01", "R0Ratio.01")

R0data_temp <- cbind(R0data_temp, t(apply(R0data_temp["temp02"], MARGIN = 1, R0temp)))
colnames(R0data_temp)[(ncol(R0data_temp)-2):ncol(R0data_temp)] <- c("MordecaiR0.02", "AdjustedR0.02", "R0Ratio.02")

R0data_temp <- cbind(R0data_temp, t(apply(R0data_temp["temp03"], MARGIN = 1, R0temp)))
colnames(R0data_temp)[(ncol(R0data_temp)-2):ncol(R0data_temp)] <- c("MordecaiR0.03", "AdjustedR0.03", "R0Ratio.03")

R0data_temp <- cbind(R0data_temp, t(apply(R0data_temp["temp04"], MARGIN = 1, R0temp)))
colnames(R0data_temp)[(ncol(R0data_temp)-2):ncol(R0data_temp)] <- c("MordecaiR0.04", "AdjustedR0.04", "R0Ratio.04")

R0data_temp <- cbind(R0data_temp, t(apply(R0data_temp["temp05"], MARGIN = 1, R0temp)))
colnames(R0data_temp)[(ncol(R0data_temp)-2):ncol(R0data_temp)] <- c("MordecaiR0.05", "AdjustedR0.05", "R0Ratio.05")

R0data_temp <- cbind(R0data_temp, t(apply(R0data_temp["temp06"], MARGIN = 1, R0temp)))
colnames(R0data_temp)[(ncol(R0data_temp)-2):ncol(R0data_temp)] <- c("MordecaiR0.06", "AdjustedR0.06", "R0Ratio.06")

R0data_temp <- cbind(R0data_temp, t(apply(R0data_temp["temp07"], MARGIN = 1, R0temp)))
colnames(R0data_temp)[(ncol(R0data_temp)-2):ncol(R0data_temp)] <- c("MordecaiR0.07", "AdjustedR0.07", "R0Ratio.07")

R0data_temp <- cbind(R0data_temp, t(apply(R0data_temp["temp08"], MARGIN = 1, R0temp)))
colnames(R0data_temp)[(ncol(R0data_temp)-2):ncol(R0data_temp)] <- c("MordecaiR0.08", "AdjustedR0.08", "R0Ratio.08")

R0data_temp <- cbind(R0data_temp, t(apply(R0data_temp["temp09"], MARGIN = 1, R0temp)))
colnames(R0data_temp)[(ncol(R0data_temp)-2):ncol(R0data_temp)] <- c("MordecaiR0.09", "AdjustedR0.09", "R0Ratio.09")

R0data_temp <- cbind(R0data_temp, t(apply(R0data_temp["temp10"], MARGIN = 1, R0temp)))
colnames(R0data_temp)[(ncol(R0data_temp)-2):ncol(R0data_temp)] <- c("MordecaiR0.10", "AdjustedR0.10", "R0Ratio.10")

R0data_temp <- cbind(R0data_temp, t(apply(R0data_temp["temp11"], MARGIN = 1, R0temp)))
colnames(R0data_temp)[(ncol(R0data_temp)-2):ncol(R0data_temp)] <- c("MordecaiR0.11", "AdjustedR0.11", "R0Ratio.11")

R0data_temp <- cbind(R0data_temp, t(apply(R0data_temp["temp12"], MARGIN = 1, R0temp)))
colnames(R0data_temp)[(ncol(R0data_temp)-2):ncol(R0data_temp)] <- c("MordecaiR0.12", "AdjustedR0.12", "R0Ratio.12")


################################################################################################################
## CALCULATE MEAN R0 RATIO FOR EACH LOCATION
################################################################################################################

R0data_temp <- R0data_temp %>%
  rowwise() %>%
  mutate(Mordecai.mean = mean(c(MordecaiR0.01, MordecaiR0.02, MordecaiR0.03,MordecaiR0.04, MordecaiR0.05, MordecaiR0.06,
                                MordecaiR0.07, MordecaiR0.08, MordecaiR0.09, MordecaiR0.10, MordecaiR0.11, MordecaiR0.12), na.rm=TRUE), 
         Adjusted.mean = mean(c(AdjustedR0.01, AdjustedR0.02, AdjustedR0.03,AdjustedR0.04, AdjustedR0.05, AdjustedR0.06,
                                AdjustedR0.07, AdjustedR0.08, AdjustedR0.09, AdjustedR0.10, AdjustedR0.11, AdjustedR0.12), na.rm=TRUE)) %>%
  mutate(meanRatio = Adjusted.mean/Mordecai.mean)


################################################################################################################
## FIND MIN AND MAX R0 RATIO FOR EACH LOCATION
################################################################################################################

R0data_temp$maxRatio <- do.call(pmax, c(R0data_temp[, c("R0Ratio.01", "R0Ratio.02", "R0Ratio.03", "R0Ratio.04", "R0Ratio.05", 
                                              "R0Ratio.06", "R0Ratio.07", "R0Ratio.08", "R0Ratio.09", "R0Ratio.10", 
                                              "R0Ratio.11", "R0Ratio.12")], list(na.rm=TRUE)))
R0data_temp$minRatio <- do.call(pmin, c(R0data_temp[, c("R0Ratio.01", "R0Ratio.02", "R0Ratio.03", "R0Ratio.04", "R0Ratio.05", 
                                              "R0Ratio.06", "R0Ratio.07", "R0Ratio.08", "R0Ratio.09", "R0Ratio.10", 
                                              "R0Ratio.11", "R0Ratio.12")], list(na.rm=TRUE)))

##############################################################################################################c
## Save data file
##############################################################################################################

save(R0data_temp, file=paste0(Sys.Date(),"_original_R0data"))
load("./2020-10-16_R0data")
##############################################################################################################c
### Map difference between new mean and old mean (fig 4B)
##############################################################################################################

R0data_temp$difference <- R0data_temp$meanRatio - R0data$meanRatio 

MEAN  <- R0data_temp %>%
  filter(count_months>0)%>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = difference)) + 
  scale_fill_gradientn(limits=c(-0.01 , 0.02), oob=scales::squish,  na.value = "#EBEBEB", 
                       colours=c("#000dff", "#ffffff","#c90000"), 
                       values = scales::rescale(c(-.01, 0, .02)), 
                       breaks =c(-0.01, 0, 0.01, 0.02),
                       labels= c("- 1%", "No change", "+ 1%", "+ 2%")) + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="R0 Ratio change from previous results") +
  theme(text=element_text(size=14,  family="sans"))

MEAN
ggsave("Figure4_difference.pdf", width=8, height=4, units="in")

