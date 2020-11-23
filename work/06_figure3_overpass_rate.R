source("00_init.R")
library(colorspace)
setwd(burke_census_path)

########################################################################################
# Last edited by: Anne, Aug 2020
# Creates a figure showing the overpass rates of various individual satellites as well as
#   their different bands and indices (All for Figure 3 D)
########################################################################################


reverselog_trans <- function(base = 10) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

overpass = read.csv("data/output/overpass.csv")
scaleFUN <- function(x) sprintf("%.0f", x)
options(scipen=1000)
overpass = overpass[overpass$variable %in% c("all_s", "all_l", "dg", "planetscope"),]
overpass = rbind(overpass, data.frame(year=2000:2018, variable="nl", value=365))
satellites = unique(overpass$variable)

for (i in 1:length(satellites)) {
  plot = ggplot() + 
    geom_hline(yintercept=7, alpha=0.8, size=.4, color="grey") + 
    geom_hline(yintercept=30, alpha=0.8, size=.4, color="grey") + 
    geom_hline(yintercept=365, alpha=0.8, size=.4, color="grey") +
    geom_hline(yintercept=73000, alpha=0.8, size=.4, color="grey") +
    geom_hline(yintercept=2555000, alpha=0.8, size=.4, color="grey") +
      
    geom_line(aes(year, value, group=variable), alpha=0.3, size=0.8, overpass) +
    geom_line(aes(year, value, group=variable), size=0.8, overpass[overpass$variable==satellites[i], ]) +
    scale_y_continuous(trans=reverselog_trans(10), limits = c(2555, 0.5),
                       breaks = c(1, 7, 30, 100, 365, 1000, 2555), 
                       labels = c(1, "", "", 100, "", 1000, "")) + #1, week, 10, yr, 1000, 7 yr
    scale_x_continuous(limits = c(2000, 2018), breaks=seq(2000, 2018, 2)) + 
    ylab("") + xlab("Year") + theme_anne("Times New Roman", size=23) + 
      theme(legend.position = "none")
  ggsave(paste0(git_path, "/figures/raw/Figure_3d_", satellites[i], ".pdf"), plot, width=7, height=4)
}