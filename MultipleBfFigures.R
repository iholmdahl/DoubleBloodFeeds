## Load data

R0data.original <- readRDS("R0data.rds")

##
## SET UP CONTINENT BACKGROUND FOR PLOTS
##

background <- setMinMax(raster("spatial.data/wc2.0_5m_tavg_01.tif")) %>% 
  mask(borders) %>%
  as.data.frame(xy=TRUE) %>%
  filter(!is.na(wc2.0_5m_tavg_01))  %>%
  mutate(continent.fill = 0)

##
## Main text figures
##

##
## Figure 4A 
##

MIN <- R0data.original %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = minRatio.1)) + 
  scale_fill_gradientn(colours = c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="A") +
  theme(legend.key.height = unit(1, "cm"), text=element_text(size=14,  family="sans"))

MEAN  <- R0data.original %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = meanRatio)) + 
  scale_fill_gradientn(colours = c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="B") +
  theme(legend.key.height = unit(1, "cm"), text=element_text(size=14,  family="sans"))

MAX <- R0data.original %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = maxRatio.1)) + 
  scale_fill_gradientn(colours = c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="C") + 
  theme(legend.key.height = unit(1, "cm"), text=element_text(size=14,  family="sans"))

ggarrange(MIN, MEAN, MAX, ncol=3, nrow=1, common.legend = TRUE, legend="right")
ggsave("F4.RoRatioMaps.final.pdf", width=10, height=5, units="in")


##
## FIGURE 4B
##

R0data %>%
  filter(!is.na(population)) %>%
  ggplot()+
  #scale_y_log10()+
  geom_density(aes(x=maxRatio.1, weight=population), color="#0d04d9", fill="#0d04d9", alpha=0.1, adjust=1, size=0.7)+
  geom_density(aes(x=meanRatio, weight=population), color="#B12A90", fill="#B12A90", alpha=0.1, adjust=10, size=0.7)+
  geom_density(aes(x=minRatio.1, weight=population), color="#FCA636", fill="#FCA636",alpha=0.1, adjust=50, size=0.7)+
  theme_classic()+
  scale_y_log10(breaks = c(1e+05, 1e+07, 1e+09, 1e+11))+
  coord_cartesian(ylim=c(1e+5, 1e+11))+
  ylab("population affected")+ 
  scale_x_continuous(name="R0 Increase", limits=c(1.1,1.6), breaks=c(1.1,1.2,1.3,1.4,1.5, 1.6), labels=c("10%", "20%", "30%", "40%", "50%", "60%"))
ggsave("4B.ratiodensitycurveszoom.pdf", units=c("in"), height=5, width =5)


## Alt FIGURE 4B
R0data %>%
  filter(!is.na(population)) %>%
  ggplot()+
  geom_bar(aes(x=maxRatio.1, weight=population), color="#0d04d9", fill="#0d04d9", alpha=0.1, binwidth=0.01)+
  geom_bar(aes(x=meanRatio, weight=population), color="#B12A90", fill="#B12A90", alpha=0.1, binwidth=0.01)+
  geom_bar(aes(x=minRatio.1, weight=population), color="#FCA636", fill="#FCA636",alpha=0.1, binwidth=0.01)+
  theme_classic()+
  scale_y_log10(limits=c(1e+0, 1e+09))+
  coord_cartesian(ylim=c(1e+2, 1e+09))+
  ylab("population affected")+ 
  scale_x_continuous(name="R0 Increase", limits=c(1.08,1.6), breaks=c(1.1,1.2,1.3,1.4,1.5, 1.6), labels=c("10%", "20%", "30%", "40%", "50%", "60%"))
ggsave("4Bhist.ratiodensitycurves.pdf", units=c("in"), height=5, width =5)







##
## Supplementary Figures
##


##
## FIGURE S1
##

Ratio.pop.plot <- as.data.frame(matrix(data=NA, nrow=10000, ncol=4))
Ratio.pop.plot$V1 <- (1:10000)

## start with temp = 20 (low transmission temp)
Ratio.pop.plot[,2:4] <- t(apply(Ratio.pop.plot[,1, drop=F], MARGIN = 1, R0pop))

## change temp to 25.5 (max transmission temp)
Ratio.pop.plot <- cbind(Ratio.pop.plot, t(apply(Ratio.pop.plot[,1, drop=F], MARGIN = 1, R0pop)))

## change temp to 30 (med transmission temp)
Ratio.pop.plot <- cbind(Ratio.pop.plot, t(apply(Ratio.pop.plot[,1, drop=F], MARGIN = 1, R0pop)))
colnames(Ratio.pop.plot) <- c("population", "BaselineR0.20", "AdjustedR0.20", "R0ratio.20", "BaselineR0.25", "AdjustedR0.25", "R0ratio.25", "BaselineR0.30", "AdjustedR0.30", "R0ratio.30")


Ratio.pop.plot %>%
  ggplot() +
  geom_line(aes(x=population, y=BaselineR0.20, color="twenty"), size=1) +   
  geom_line(aes(x=population, y=BaselineR0.25, color="twentyfive"), size=1) +   
  geom_line(aes(x=population, y=BaselineR0.30, color="thirty"), size=1) +   
  #geom_vline(xintercept = 250, linetype="dotted",  color = "#777777", size=1)+
  geom_vline(xintercept = 250, linetype="dotted",  color = "#777777", size=1)+
  scale_x_continuous(limits=c(0,2500))+
  scale_y_continuous(limits=c(0,300))+
  theme_classic() +
  ylab("R0") +
  xlab("Population")+
  scale_color_manual(name = "Temperature",
                     breaks = c("twenty", "twentyfive","thirty"),
                     values = c(twenty = "#FCA636FF", twentyfive = "#B12A90FF", thirty= "#6A00A8FF"))


ggsave("R0ratiopop.pdf", units=c("in"), height=5, width =6)


##
## FIGURE S2
##
##
## S2 MONTHLY RATIO FIGURES
##

JAN <- R0data %>%
  filter(popAdjustedR0.01>R0cutoff) %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.01)) + 
  scale_fill_gradientn(colours =c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="January") + 
  theme(legend.key.height = unit(2, "cm"))

FEB <- R0data %>%
  filter(popAdjustedR0.02>R0cutoff) %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.02)) + 
  scale_fill_gradientn(colours =c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="February") + 
  theme(legend.key.height = unit(2, "cm"))

MAR <- R0data %>%
  filter(popAdjustedR0.03>R0cutoff) %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.03)) + 
  scale_fill_gradientn(colours =c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="March") + 
  theme(legend.key.height = unit(2, "cm"))

APR <- R0data %>%
  filter(popAdjustedR0.04>R0cutoff) %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.04)) + 
  scale_fill_gradientn(colours =c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="April") + 
  theme(legend.key.height = unit(2, "cm"))

MAY <- R0data %>%
  filter(popAdjustedR0.05>R0cutoff) %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.05)) + 
  scale_fill_gradientn(colours =c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="May") + 
  theme(legend.key.height = unit(2, "cm"))

JUN <- R0data %>%
  filter(popAdjustedR0.06>R0cutoff) %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.06)) + 
  scale_fill_gradientn(colours =c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="June") + 
  theme(legend.key.height = unit(2, "cm"))

JUL <- R0data %>%
  filter(popAdjustedR0.07>R0cutoff) %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.07)) + 
  scale_fill_gradientn(colours =c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="July") + 
  theme(legend.key.height = unit(2, "cm"))

AUG <- R0data %>%
  filter(popAdjustedR0.08>R0cutoff) %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.08)) + 
  scale_fill_gradientn(colours =c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="August") + 
  theme(legend.key.height = unit(2, "cm"))

SEP <- R0data %>%
  filter(popAdjustedR0.09>R0cutoff) %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.09)) + 
  scale_fill_gradientn(colours =c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="September") + 
  theme(legend.key.height = unit(2, "cm"))

OCT <- R0data %>%
  filter(popAdjustedR0.10>R0cutoff) %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.10)) + 
  scale_fill_gradientn(colours =c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="October") + 
  theme(legend.key.height = unit(2, "cm"))

NOV <- R0data %>%
  filter(popAdjustedR0.11>R0cutoff) %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.11)) + 
  scale_fill_gradientn(colours =c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="November") + 
  theme(legend.key.height = unit(2, "cm"))

DEC <- R0data %>%
  filter(popAdjustedR0.12>R0cutoff) %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = R0Ratio.12)) + 
  scale_fill_gradientn(colours =c("#F0F921",  "#FCA636", "#E16462", "#B12A90", "#6A00A8", "#0D0887", "#03004f"),
                       limits=c(1,1.6), oob=scales::squish, 
                       values = scales::rescale(c(1.0, 1.075, 1.15, 1.225, 1.3, 1.45, 1.60)),
                       guide = "colorbar",
                       breaks=c(1, 1.1, 1.2, 1.4, 1.6), 
                       labels=c("0%", "10%", "20%", "40%", "60%"), 
                       na.value = "#EBEBEB") + 
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="December") + 
  theme(legend.key.height = unit(2, "cm"))

ggarrange(JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC, ncol=4, nrow=3, common.legend = TRUE, legend="right")
ggsave("S2.monthsRatio.pdf", width=7.5, height=6.25, units="in")







##
## Discarded Supplement figures

##
## PLOT R0 RATIOs BY TEMPERATURE
##


Ratio.plot <- as.data.frame(matrix(data=NA, nrow=160, ncol=4))
Ratio.plot$V1 <- (1:160)/4
Ratio.plot[,2:4] <- t(apply(Ratio.plot[,1, drop=F], MARGIN = 1, R0temp))

## change population size in R0temp function to 250 (from 1 million)
Ratio.plot <- cbind(Ratio.plot, t(apply(Ratio.plot[,1, drop=F], MARGIN = 1, R0temp)))
colnames(Ratio.plot) <- c("temperature", "BaselineR0", "AdjustedR0", "R0ratio", "BaselineR0.250", "AdjustedR0.250", "R0ratio.250")

Ratio.plot %>%
  filter(R0ratio<3) %>%
  ggplot() +
  geom_vline(xintercept = 18.5, linetype="dotted",  color = "#777777", size=0.7, alpha=0.5) + 
  geom_vline(xintercept = 16.5, linetype="dotted",  color = "#777777", size=0.7) + 
  geom_vline(xintercept = 33, linetype="dotted",  color = "#777777", size=0.7, alpha=0.5) + 
  geom_vline(xintercept = 33.75, linetype="dotted",  color = "#777777", size=0.7) + 
  geom_hline(yintercept = 0.05, linetype="dotted",  color = "#777777", size=0.7)+
  geom_hline(yintercept = 1.30, linetype="dotted",  color = "#777777", size=0.7)+
  #geom_vline(xintercept = 20, linetype="dotted",  color = "lightblue", size=0.7)+
  #geom_vline(xintercept = 25.5, linetype="dotted",  color = "lightblue", size=0.7)+
  #geom_vline(xintercept = 30, linetype="dotted",  color = "lightblue", size=0.7)+
  geom_line(aes(x=temperature, y=BaselineR0, color="Baseline"), size=1) + 
  geom_line(aes(x=temperature, y=AdjustedR0, color="Adjusted"), size=1) +
  geom_line(aes(x=temperature, y=R0ratio, color="Ratio"), size=1) + 
  geom_line(aes(x=temperature, y=BaselineR0.250, color="Baseline"), size=1, linetype="dashed") + 
  geom_line(aes(x=temperature, y=AdjustedR0.250, color="Adjusted"), size=1, linetype="dashed") +
  theme_classic() +
  ylab("") +
  xlab("Temperature") +
  scale_color_manual(name = "",
                     breaks = c("Baseline", "Adjusted", "Ratio"),
                     values = c(Baseline = "#FCA636FF", Adjusted = "#B12A90FF", Ratio= "black"))

ggsave("R0ratiotemperature250.png", units=c("in"), height=5, width =7)




##
## PLOT R0 ESTIMATES
##

BASELINE <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="white") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = Mordecai.mean)) + 
  scale_fill_viridis_c(option="plasma", direction= -1,limits=c(0,85.42), na.value = "#EBEBEB") + 
  #scale_fill_gradient2(low="#0D0887", mid = "lightgrey", high = "white", midpoint = 42.5, limits=c(0,85.42))+
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="Baseline R0") +
  theme(legend.key.height = unit(2, "cm"))


ADJUSTED <- R0data %>%
  filter(Anopheles>0.05) %>%
  ggplot()+
  geom_raster(data = background, aes(x = x, y = y, fill = continent.fill), fill="#EBEBEB") + 
  coord_quickmap() + 
  theme_void() +
  geom_raster(aes(x = x, y = y, fill = Adjusted.mean)) + 
  scale_fill_viridis_c(option="plasma",  direction= -1, limits=c(0,85.42), na.value = "#EBEBEB")+
  #scale_fill_gradient2(low="#03004f", mid = "#f4cfff", high="#ff4049", midpoint = 42.5, limits=c(0,85.42))+
  geom_path(data=borders.df, aes(x=long, y=lat, group=group), color="black", size=0.2) +
  geom_path(data=SouthSudan.df, aes(x=long, y=lat, group=group), color="black", size=0.25) +
  labs(fill="", title="Adjusted R0") +
  theme(legend.key.height = unit(2, "cm"))

ggarrange(BASELINE, ADJUSTED, ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("R0EstimateMaps.final.png", width=7, height=5, units="in")



## PLOT DENSITY CURVES FOR RO AND POPULATION, GEOGRAPHIC AREA
##

landdensitycurve <- R0data  %>%
  ggplot()+
  geom_density(aes(x=meanMordecaiR0.pop), color="#5DC863", fill="#5DC863", alpha=0.7)+
  geom_density(aes(x=meanAdjustedR0.pop), color="#2a52b5",fill="#2a52b5", alpha=0.5)+
  theme_classic()+
  ylab("geographic area affected")+ 
  scale_x_continuous(name="R0", limits=c(0,90))

popdensitycurve <- R0data %>%
  ggplot()+
  geom_density(aes(x=meanMordecaiR0.pop, weight=population), color="#5DC863", fill="#5DC863", alpha=0.7)+
  geom_density(aes(x=meanAdjustedR0.pop, weight=population), color="#2a52b5",fill="#2a52b5", alpha=0.5)+
  theme_classic()+
  ylab("population affected")+ 
  scale_x_continuous(name="R0", limits=c(0,90))

densitycurves <- ggarrange(landdensitycurve, popdensitycurve, nrow=1, ncol=2)
ggsave("S4.densitycurves.pdf", units=c("in"), height=5, width =10)











