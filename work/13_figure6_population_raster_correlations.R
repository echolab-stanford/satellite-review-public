library(parameters)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(grid)
library(scales)
library(viridis)
library(wesanderson)
source("00_init.R")
setwd(data_path)
setwd("../population")

########################################################################################
# Written by: Jenny Xue
# Last edited by: Anne Driscoll, Feb 2021 for figure proofs
# Compares several population rasters to each other, at various pixel sizes.
########################################################################################


# Read in data ###########################################################################

df = readRDS('extracted_raster_values/pixels_values_1k.rds')

df.rmna = df[complete.cases(df[ ,2:4]),]

africa = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", 
           "Cape Verde", "Central African Republic", "Chad", "Comoros", "Côte d'Ivoire", 
           "Democratic Republic of the Congo", "Republic of Congo", "Djibouti", "Egypt", 
           "Equatorial Guinea", "Eritrea", "Swaziland", "Ethiopia", "Gabon", "Gambia", 
           "Ghana", "Guinea", "Guinea-Bissau",  "Kenya", "Lesotho", "Liberia", 
           "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", 
           "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
           "Senegal", "Sierra Leone", "Seychelles", "Somalia", "South Africa", "South Sudan", 
           "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")
d = df[df$NAME_0 %in% africa, ]
cor_pop_scan = round(cor(df.rmna$worldpop, df.rmna$landscan, use="c")^2, 2)
cor_pop_ghsl = round(cor(df.rmna$worldpop, df.rmna$ghsl, use="c")^2, 2)
cor_ghsl_scan = round(cor(df.rmna$ghsl, df.rmna$landscan, use="c")^2, 2)

options(scipen=10000)


##########################################################################################
# Create scatters comparing estimates to each other
##########################################################################################

ggplot(df.rmna, aes(x=worldpop, y=landscan, fill=log(..count..)) ) +
  geom_bin2d(bins = 80) +
  scale_fill_gradient(low="grey90", high="black", na.value="white", 
                      labels=trans_format("identity", function(x) 
                                                    10^ceiling(log10(round(exp(x),0))))) +
  theme_anne(font="sans") + xlim(0,200000) + ylim(0,200000) + coord_fixed() +
  xlab("Worldpop Estimate (1000's)") + ylab("Landscan Estimate (1000's)") +
  annotate(geom="text", x=25000, y=190000, 
           label=paste0("R^2=", cor_pop_scan, ", p<0.01"))
ggsave(paste0(git_path, "/figures/raw/Figure_6a.pdf"), limitsize = FALSE)

ggplot(df.rmna, aes(x=ghsl, y=landscan, fill=log(..count..)) ) +
    geom_bin2d(bins = 80) +
    scale_fill_gradient(low="grey90", high="black", na.value="white", 
                        labels=trans_format("identity", function(x) 
                            10^ceiling(log10(round(exp(x),0))))) +
    theme_anne(font="sans") + xlim(0,200000) + ylim(0,200000) + coord_fixed() +
    xlab("GHSL Estimate (1000's)") + ylab("Landscan Estimate (1000's)") +
  annotate(geom="text", x=25000, y=190000, 
           label=paste0("R^2=", cor_ghsl_scan, ", p<0.01"))
ggsave(paste0(git_path, "/figures/raw/Figure_6b.pdf"), limitsize = FALSE)

ggplot(df.rmna, aes(x=ghsl, y=worldpop, fill=log(..count..))) +
    geom_bin2d(bins = 80) +
    scale_fill_gradient(low="grey90", high="black", na.value="white", 
                        labels=trans_format("identity", function(x) 
                            10^ceiling(log10(round(exp(x),0))))) +
    theme_anne(font="sans") + xlim(0,200000) + ylim(0,200000) + coord_fixed() +
    xlab("GHSL Estimate (1000's)") + ylab("Worldpop Estimate (1000's)") +
  annotate(geom="text", x=25000, y=190000, 
           label=paste0("R^2=", cor_pop_ghsl, ", p<0.01"))
ggsave(paste0(git_path, "/figures/raw/Figure_6c.pdf"), limitsize = FALSE)


##########################################################################################
# Plot correlations by country on map
##########################################################################################

countries = readRDS("../boundaries/gadm_simp.RDS") #low res
df = read.csv("extracted_raster_values/level_0_correlation_by_country.csv")
world = readRDS("../boundaries/gadm_outline_simp.RDS")
df = df[c(2,4,5,6,7)]

countries[countries$id=="SJM", "id"] = "NOR"
countries[countries$id=="GRL", "id"] = "DNK"

merged = merge(countries, df, by.x="id",by.y="X.1", all.x=T)
merged = merged[order(merged$group, merged$piece, merged$order), ]

g = ggplot(merged, aes(long, lat, group=group)) + # the data
    geom_polygon(color="white", lwd=.04, aes(fill=avg)) + # make polygons w fill and outline
    scale_fill_viridis(option="plasma") + 
    theme(axis.ticks=element_blank(),  # remove the background, tickmarks, etc
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank(), 
          text=element_text(size=15,  family="Times New Roman"))
ggplot2::ggsave(paste0(git_path, "/figures/raw/Figure_6d.pdf"), plot=g,  width=4.75, height=3.325)


#####################################################################
# Plot global correlations at different levels of aggregation
#####################################################################

df = readRDS("extracted_raster_values/pixels_values_1k.rds")

cor1_1k = cor(df$worldpop, df$landscan, method="pearson", use="complete.obs")
cor2_1k = cor(df$ghsl, df$landscan, method="pearson", use="complete.obs")
cor3_1k = cor(df$ghsl, df$worldpop, method="pearson", use="complete.obs")

df = readRDS("extracted_raster_values/pixels_values_5k.rds")

cor1_5k = cor(df$worldpop, df$landscan, method="pearson", use="complete.obs")
cor2_5k = cor(df$ghsl, df$landscan, method="pearson", use="complete.obs")
cor3_5k = cor(df$ghsl, df$worldpop, method="pearson", use="complete.obs")

df = readRDS("extracted_raster_values/pixels_values_10k.rds")

cor1_10k = cor(df$worldpop, df$landscan, method="pearson", use="complete.obs")
cor2_10k = cor(df$ghsl, df$landscan, method="pearson", use="complete.obs")
cor3_10k = cor(df$ghsl, df$worldpop, method="pearson", use="complete.obs")

df = readRDS("extracted_raster_values/pixels_values_100k.rds")

cor1_100k = cor(df$worldpop, df$landscan, method="pearson", use="complete.obs")
cor2_100k = cor(df$ghsl, df$landscan, method="pearson", use="complete.obs")
cor3_100k = cor(df$ghsl, df$worldpop, method="pearson", use="complete.obs")

df = data.frame(correlation=c(cor1_1k,cor1_5k,cor1_10k,cor1_100k,cor2_1k,cor2_5k,
                               cor2_10k,cor2_100k,cor3_1k,cor3_5k,cor3_10k,cor3_100k), 
                 resolution=c("1km","5km","10km","100km","1km","5km","10km","100km",
                              "1km","5km","10km","100km"), 
                 dataset=c("Worldpop vs Landscan","Worldpop vs Landscan",
                           "Worldpop vs Landscan","Worldpop vs Landscan", 
                           "GHSL vs Landscan","GHSL vs Landscan","GHSL vs Landscan",
                           "GHSL vs Landscan","GHSL vs Worldpop","GHSL vs Worldpop",
                           "GHSL vs Worldpop","GHSL vs Worldpop"))
df$resolution = factor(df$resolution,levels = c("1km", "5km", "10km", "100k"))

ggplot(df, aes(x=resolution, y=correlation, group=dataset)) + 
  geom_line(aes(col=dataset)) + geom_point(aes(col=dataset)) +
  scale_color_manual(values = wes_palette("GrandBudapest1", n = 3)) +
  labs(col = "Dataset", y = "Correlation", x = "Resolution") + theme_anne(font="sans")
ggsave(paste0(git_path, "/figures/raw/Figure_6e.pdf"), limitsize = FALSE)

