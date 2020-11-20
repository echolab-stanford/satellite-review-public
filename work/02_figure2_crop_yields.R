source("00_init.R")
require(tidyverse)
library(RColorBrewer)
library(scales)
setwd(data_path)

########################################################################################
# Written by: Marshall Burke, Aug 2020
# Last edited by: Anne, Sept 2020
# Creates a figure showing differences in yield reporting from LSMS and gov
########################################################################################

eldata = read.csv('eAtlasLSMS_Combined.csv')

#for now the units outside of ethiopia are t/10
pd = eldata %>% filter(LSMS_Yield <= 10) %>% group_by(country,region,year) %>%
  summarize(atlyield = mean(eAtlas_Yield),lsprod = sum(LSMS_Yield*LSMSArea,na.rm=T),
            lsarea = sum(LSMSArea,na.rm=T),lssamp = sum(numHouseholds))

pd = pd %>% filter (lssamp >= 30) %>% mutate(lsmsyield = lsprod/lsarea)

ggplot(pd, aes(lsmsyield, atlyield)) + 
    geom_abline(intercept=0, slope=1, color="grey", alpha=0.6) + 
    stat_smooth(geom="line", method="lm", se=F, size=0.7, alpha=0.8) + 
    geom_point(aes(size=lssamp)) + 
    facet_grid(cols=vars(country)) +
    guides(size=guide_legend(title="# of Households", 
                             title.theme=element_text(size=12, family="sans"))) + 
    xlim(0, 3.6) + ylim(0, 3.6) + 
    xlab("LSMS survey based yield (t/ha)") + ylab("Official govt yield (t/ha)") + 
    theme_anne(font="sans", size=12)
ggsave(paste0(git_path,'/figures/raw/Figure_2.pdf'), width=8.8, height=3, 
       useDingbats=F)
