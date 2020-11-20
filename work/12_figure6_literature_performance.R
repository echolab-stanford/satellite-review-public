source("00_init.R")
library(tidyverse)
library(openxlsx)

########################################################################################
# Written by: Marshall Burke, Aug 2020
# Last edited by: Anne, Aug 2020
# Creates a figure showing changes in performance in literature over the years (6 F-G)
########################################################################################


# poverty data
pov <- read.xlsx('written/pov_slum_lit.xlsx',sheet=1)
pov$r2 <- pov$Best.Performance
pov$r2[pov$Metric=="cor"] <- pov$r2[pov$Metric=="cor"]^2
pov$r2[!pov$Metric%in%c("r2","cor")] <- NA
pov <- pov %>% 
    filter(Metric%in%c("r2","cor") & Prediction.Task=="Asset Wealth Index")
length(unique(pov$Paper))
pov$pch <- 1
pov$pch[pov$Geo!="cluster" & is.na(pov$Notes)] <- 2
pov$pch[pov$Geo!="cluster" & is.na(pov$Notes)==F] <- 17
pov$pch[pov$Geo=="cluster" & is.na(pov$Notes)==F] <- 19

#slums
slum <- read.xlsx('written/pov_slum_lit.xlsx',sheet=2)
slum <- filter(slum,Metric=="accuracy")

ggplot() + 
    geom_smooth(data=pov, aes(Year, Best.Performance), se=F, method="lm", lwd=0.5, 
                col="black") +
    geom_point(data=pov[pov$Geo=="cluster" & is.na(pov$Notes), ], shape=1, 
               aes(Year, Best.Performance), colour="black", fill="white", size=3) + 
    geom_point(data=pov[pov$Geo=="cluster" & !is.na(pov$Notes), ], shape=16, 
               aes(Year, Best.Performance), colour="black", fill="black", size=3) + 
    geom_point(data=pov[pov$Geo!="cluster" & is.na(pov$Notes), ], shape=2, 
               aes(Year, Best.Performance), colour="black", fill="white", size=3) + 
    geom_point(data=pov[pov$Geo!="cluster" & !is.na(pov$Notes), ], shape=17, 
               aes(Year, Best.Performance), colour="black", fill="black", size=3) + 
    theme_anne(font="sans")  + ylim(0.5, 1)
ggsave("../viz/raw/Figure_6f.pdf", device="pdf", useDingbats=F, width=4.07, height=4)


ggplot() + 
    geom_smooth(data=slum, aes(Year, Best.Performance), se=F, method="lm", 
                lwd=0.5, col="black") +
    geom_point(data=slum[slum$Num.classes==2, ], 
               aes(Year, Best.Performance), shape=16, colour="grey78", size=3) + 
    geom_point(data=slum[slum$Num.classes==3, ], 
               aes(Year, Best.Performance), shape=16, colour="grey54", size=3) + 
    geom_point(data=slum[slum$Num.classes==4, ], 
               aes(Year, Best.Performance), shape=16, colour="grey40", size=3) + 
    geom_point(data=slum[slum$Num.classes==6, ], 
               aes(Year, Best.Performance), shape=16, colour="black", size=3) + 
    theme_anne(font="sans") + ylim(0.5, 1)
ggsave("../viz/raw/Figure_6g.pdf", device="pdf", useDingbats=F, width=4.07, height=4)

