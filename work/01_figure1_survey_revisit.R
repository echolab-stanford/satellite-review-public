library(reshape2)
library(dplyr)
library(rgdal)
library(ggplot2)
library(census.tools)
library(rgeos)
library(BAMMtools)
library(gdata)
library(countrycode)
library(tidyr)
library(extrafont)
loadfonts()
source("00_init.R")
setwd(data_path)

########################################################################################
# Written by: Anne, Jun 2020
# Last edited by: Anne, Nov 2020
# Creates all panels for Figure 1 
########################################################################################

#setup
projection = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")

# read in polity data
polity = read.xls("polity5v2018.xls") 
cow_cross = codelist[, c("iso3c", "cowc")] #correlates of war (cowc) ids to iso3
polity = merge(polity, cow_cross, by.x="scode", by.y="cowc", all.x=T)
polity = polity %>% dplyr::filter(year <= 2019 & year >= 1993) %>%
    dplyr::select(iso3c, polity2) %>% dplyr::group_by(iso3c) %>% 
    dplyr::summarise(polity2 = mean(polity2, na.rm=T))

# read in world income inequality database
surveys = read.csv("WIID_06MAY2020.csv", stringsAsFactors=F, na.strings=c(""))
surveys$survey[is.na(surveys$survey)] = surveys$source_comments[is.na(surveys$survey)]
surveys$survey[is.na(surveys$survey)] = surveys$source_detailed[is.na(surveys$survey)]
surveys = surveys[surveys$areacovr=="All" & surveys$quality %in% c("Average", "High") &
                      surveys$popcovr == "All", 
            c("country", "c3", "year", "resource", "population", "incomegroup", 
              "source_detailed", "survey")]
names(surveys) = c("country", "iso3", "year", "resource", "pop", "incomegroup", "source", 
                   "survey")
surveys$is_dup = duplicated(surveys[, c("iso3", "year", "resource", "survey")])

#get the average number of years between surveys
surveys = surveys %>% 
  dplyr::filter(year <= 2019 & year >= 1993 & !is_dup) %>%
  dplyr::group_by(country, iso3) %>%
  dplyr::summarise(diff = (2019-1993+1)/n(), 
            tot = n(), incomegroup = last(na.omit(incomegroup)))
surveys$country = as.character(surveys$country)
surveys$iso3 = as.character(surveys$iso3)
surveys = rbind(surveys, list(country="Saudi Arabia", iso3="SAU", diff=NaN, tot=0), 
                list(country="Somalia", iso3="SOM", diff=NaN, tot=0), 
                list(country="North Korea", iso3="PRK", diff=NaN, tot=0), 
                list(country="Cuba", iso3="CUB", diff=NaN, tot=0), 
                list(country="French Guiana", iso3="GUF", diff=NaN, tot=0), 
                list(country="Western Sahara", iso3="ESH", diff=NaN, tot=0), 
                list(country="Eritrea", iso3="ERI", diff=NaN, tot=0))

# get number for text on median in Africa
africa = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", 
      "Cape Verde", "Central African Republic", "Chad", "Comoros", 
      "Congo, Democratic Republic of the", "Congo, Republic of the", "Djibouti", "Egypt", 
      "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia, The", 
      "Ghana", "Guinea", "Guinea-Bissau", "Cote d'Ivoire", "Kenya", "Lesotho", "Liberia", 
      "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", 
      "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
      "Senegal", "Sierra Leone", "Seychelles", "Somalia", "South Africa", "South Sudan", 
      "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")
summary(surveys$diff[surveys$country %in% africa])

# bring in world bank GDP data, since we only have GDP in years where there is a survey
inc = read.csv("../world_bank/wb_gdp.csv")
inc = gather(inc, year, gdp, X1960:X2018, factor_key=TRUE)
inc$year = as.numeric(substr(inc$year, 2, 5))
inc = inc[complete.cases(inc), ]
pop = read.csv("../world_bank/population_time.csv")
pop = gather(pop, year, pop, X1960:X2016, factor_key=TRUE)
pop$year = as.numeric(substr(pop$year, 2, 5))
inc = merge(inc, pop, by.x=c("country", "iso3", "year"), 
            by.y=c("country", "code", "year"), all.x=T)
inc$gdppc = inc$gdp/inc$pop
inc = inc %>% group_by(country, iso3) %>% summarise(gdppc = mean(gdppc, na.rm=T))

# merge in the GDP and polity measures to survey measures
surveys = merge(surveys, inc[, c("iso3", "gdppc")], by="iso3", all.x=T)
surveys = merge(surveys, polity, by.x="iso3", by.y="iso3c", all.x=T)

#get world shapefile and merge to data
if (!"gadm_simp.RDS" %in% list.files("../boundaries")) {
    map = readOGR("../boundaries/gadm/gadm36_0", "gadm36_0")
    area = vapply(slot(map, "polygons"), slot, "area", FUN.VALUE=0)
    map = map[area>1.5,] #remove tiny islands etc
    map = map[!map@data$NAME_0 %in% c("Caspian Sea", "Antarctica"),]
    map = map[!grepl("sland", map$NAME_0), ]
    map = spTransform(map, CRS("+proj=robin"))
    
    simp = gSimplify(map, 3500)
    world = simp %>% gBuffer(byid=F, width=2500)
    world = ggplot2::fortify(world)
    saveRDS(world, "../boundaries/gadm_outline_simp.RDS")
    
    simp = SpatialPolygonsDataFrame(simp, map@data)
    simp = ggplot2::fortify(simp, region="GID_0")
    saveRDS(simp, "../boundaries/gadm_simp.RDS")
    map = simp
} else {
    map = readRDS("../boundaries/gadm_simp.RDS")
    world = readRDS("../boundaries/gadm_outline_simp.RDS")
}

surveys$diff = as.numeric(as.character(surveys$diff))
surveys$tot = as.numeric(as.character(surveys$tot))
surveys$iso3 = as.character(surveys$iso3)
map$id = as.character(map$id)

# normalize territories (eg greenland shows same revisit as denmark)
map[map$id=="SJM", "id"] = "NOR"
map[map$id=="GRL", "id"] = "DNK"
map = merge(map, surveys, by.x="id", by.y="iso3", all=T)
map = map[order(map$group, map$piece, map$order),]

##############################################################################
# create first 3 panels (using surveys from wiid)
##############################################################################

diff = c(-0.1, 3, 6, 9, 27)
map$cut_diff = cut(map$diff, diff)
cols = function(x) {return(c("#ff0000", "#f67e00", "#f6c000", "#f6f600"))} 
lim = rev(levels(map$cut_diff))

g = ggplot(map, aes(long, lat, group=group)) + # the data
    geom_polygon(color="white", lwd=.04, aes(fill=cut_diff)) + # make polygons w fill and outline
    geom_polygon(color="white", lwd=.04, aes(fill=cut_diff),
                 data=map[map$country %in% c("Lesotho", "Rwanda", "Burundi"),]) +
    #geom_polygon(color="grey50", fill=NA, lwd=0.05, data=world) +
    scale_fill_discrete(palette=cols, breaks=diff, limits=lim) + 
    geom_polygon(aes(long, lat, group=group), fill="black", color="white", lwd=.04, 
                 data=map[map$tot == 0, ]) +
    theme(line = element_blank(),  # remove the background, tickmarks, etc
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank(), 
          text=element_text(size=15,  family="Times New Roman"))
ggplot2::ggsave(paste0(git_path, "/figures/raw/Figure_1a.pdf"), plot=g,  width=4.75, height=3.325)

model = lm(tot ~ log(gdppc), data=surveys)
r2 = format(round(summary(model)$r.squared, 2), nsmall = 2)
lab = paste("r^2 =='", r2, "'")
g = ggplot(surveys, aes(gdppc, tot)) + geom_point(alpha=0.2) + 
    geom_smooth(method = "lm", se = F, alpha = 0.4, size = 0.5) + ylim(0, 190) +
    scale_x_log10(breaks = c(100, 1000, 10000, 50000), limits=c(100, 50000)) +
    xlab("Log GDP per capita") + ylab("Number of Surveys") +
    annotation_logticks(short = unit(-0.5,"mm"), mid = unit(-1.2,"mm"), long = unit(-2,"mm"), 
                        sides = "b", color = "#333333") + 
    coord_cartesian(clip="off") +
    theme_bw(base_family = "sans", base_size = 14) +
    theme(panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          plot.background = element_blank(),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          axis.line.x = element_line(color = "black", size = 0.2), 
          axis.line.y = element_line(color = "black", size = 0.2), 
          axis.text.x = element_text(vjust=0), 
          text=element_text(size=15,  family="Times New Roman")) + 
    annotate("text", x = 150, y = 190, label = lab, color = "grey46", hjust = 0, 
             vjust = 1, size = 12/2.5, family = "Times New Roman", parse = T)
ggplot2::ggsave(paste0(git_path, "/figures/raw/Figure_1b.pdf"), plot=g,  width=2.25, height=2.25)

g =  panel(surveys[, c("polity2", "tot")], betal=T, font="Times New Roman", size=15, 
           ybounds=c(0, 190)) + 
    xlab("Polity") + ylab("Number of Surveys")
ggplot2::ggsave(paste0(git_path, "/figures/raw/Figure_1c.pdf"), plot=g,  width=2.25, height=2.25)


##############################################################################
# create panels 4-6 using ag census
##############################################################################

# read in map and ag and prep
ag = read.csv(paste0(burke_census_path, "/data/surveys/ag_surveys.csv"))
ag[ag$country == "South Sudan", 2:4] = ag[ag$country == "Sudan", 2:4]
ag$years = NA

# set the years variable to year of most recent census
ag[ag$twenties > 2018 & !is.na(ag$twenties), ]$twenties = NA
ag[, 2:6][ag[, 2:6] < 1993] = NA

longest_diff = function(x) {
    x = x[!is.na(x)]
    x = c(1976, x, 2018)
    x = x[-1] - x[-length(x)]
    return(max(x))
}
ag$diff =  apply(ag[, 2:6], 1, longest_diff)
ag$years = apply(ag[, 2:6], 1, max, na.rm=T)
ag$years[is.infinite(ag$years)] = NA
ag = dplyr::select(ag, years, country, diff)

# get the iso3 for merging, merge in gdp and polity
cross = read.csv(paste0(burke_census_path, "/data/crosswalks/crosswalk_countries.csv"))
ag = merge(ag, cross, by.x="country", by.y="country_simp", all.x=T)
ag$most_recent = 2018-ag$years
ag = merge(ag[, c("country", "iso3", "years", "most_recent")], 
           inc[,2:3], by="iso3", all.x=T)
ag = merge(ag, polity, by.x="iso3", by.y="iso3c", all.x=T)


# merge data to map
map = readRDS("../boundaries/gadm_simp.RDS")
map$id = as.character(map$id)
map[map$id=="SJM", "id"] = "NOR"
map[map$id=="GRL", "id"] = "DNK"
map = merge(map, ag[, c("iso3", "years", "most_recent")], by.x="id", by.y="iso3", all=T)
map = map[order(map$group, map$piece, map$order),]

diff = c(-0.1, 3, 6, 8, 10, 26)
cols = function(x) { return(rev(c("#00268c", "#023ad1", "#1762ff", "#5c90ff", "#9cbcff")))}
map$cut_year = cut(map$most_recent, diff)
lim = levels(map$cut_year)

g = ggplot(map, aes(long, lat, group=group)) + # the data
    geom_polygon(color="white", lwd=.04, aes(fill=cut_year)) + # make polygons w fill and outline
    scale_fill_discrete(palette=cols, breaks=diff, limits=lim) + 
    geom_polygon(aes(long, lat, group=group), fill="black", color="white", lwd=.04, 
                 data=map[map$id %in% c("GRL", "ESH", "CUB") | is.na(map$years), ]) +
    theme(axis.ticks=element_blank(),  # remove the background, tickmarks, etc
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank(), 
          text=element_text(size=15,  family="sans"))
ggplot2::ggsave(paste0(git_path, "/figures/raw/Figure_1d.pdf"), plot=g,  width=4.75, height=3.325)


model = lm(years_since ~ log(gdppc), data=ag[!is.infinite(ag$years_since),])
r2 = format(round(summary(model)$r.squared, 2), nsmall = 2)
lab = paste("r^2 =='", r2, "'")
g = ggplot(ag, aes(gdppc, years_since)) + geom_point(alpha=0.2) + 
    geom_smooth(method = "lm", se = F, alpha = 0.4, size = 0.5) + 
    annotate("text", x = 100, y = 41, label = lab, color = "grey46", hjust = 0, 
             vjust = 1, size = 12/2.5, family = "sans", parse = T) + ylim(2, 41) +
    scale_x_log10(breaks = c(100, 1000, 10000, 50000), limits=c(100, 50000)) +
    annotation_logticks(short = unit(-0.5,"mm"), mid = unit(-1.2,"mm"), long = unit(-2,"mm"), 
                        sides = "b", color = "#333333") + 
    coord_cartesian(clip="off") +
    theme_bw(base_family = "Times New Roman", base_size = 14) +
    theme(panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          plot.background = element_blank(),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          axis.line.x = element_line(color = "black", size = 0.2), 
          axis.line.y = element_line(color = "black", size = 0.2), 
          axis.text.x = element_text(vjust=0), 
          text=element_text(size=15,  family="Times New Roman")) +
    xlab("Log GDP per capita") + ylab("Years Since")
ggplot2::ggsave(paste0(git_path, "/figures/raw/Figure_1e.pdf"), plot=g,  width=2.25, height=2.25)

g =  panel(ag[!is.infinite(ag$years_since), 
              c("polity2", "years_since")], betal=T, font="Times New Roman", size=15) + 
    xlab("Polity") + ylab("Years Since")
ggplot2::ggsave(paste0(git_path, "/figures/raw/Figure_1f.pdf"), plot=g,  width=2.25, height=2.25)


##############################################################################
# create panels 7-9 using population census
##############################################################################

# read in population data 
pop = read.csv(paste0(burke_census_path, "/data/surveys/pop_surveys.csv"), na.strings="")

#deal with observations that haven't happened yet or are after study period
pop$nineties[pop$nineties < 1993] = NA
pop$ninetie2s[pop$nineties2 < 1993] = NA
pop$twenties[!is.na(pop$twenties) & grepl(")", as.character(pop$twenties))] = NA
pop$twenties2[!is.na(pop$twenties2) & grepl(")", as.character(pop$twenties2))] = NA
pop[, 2:9] = apply(pop[,2:9], 2, function(x){as.numeric(as.character(x))})
pop$twenties[pop$twenties > 2018] = NA
pop$twenties2[pop$twenties2 > 2018] = NA

# set the years variable to year of most recent census
pop$diff =  apply(pop[, 2:9], 1, longest_diff)
pop$years = apply(pop[, 2:9], 1, max, na.rm=T)
pop$years[is.infinite(pop$years)] = NA
pop = dplyr::select(pop, country, years, diff)

# get the iso3 for merging, merge in gdp and polity
cross = read.csv(paste0(burke_census_path, "/data/crosswalks/crosswalk_countries.csv"))
pop = merge(pop, cross[, c("country_simp", "iso3")], 
            by.x="country", by.y="country_simp", all.x=T)
pop$years_since = 2018-pop$years
pop$years_since[pop$years_since==2018] = NA
pop = merge(pop, inc[,2:3], by="iso3", all.x=T)
pop = merge(pop, polity, by.x="iso3", by.y="iso3c", all.x=T)

# merge data to map
map = readRDS("../boundaries/gadm_simp.RDS")
map$id = as.character(map$id)
map[map$id=="SJM", "id"] = "NOR"
map[map$id=="GRL", "id"] = "DNK"
map = merge(map, pop[, c("iso3", "years", "years_since")], by.x="id", by.y="iso3", all=T)
map = map[order(map$group, map$piece, map$order),]

map$cut_year = cut(map$years_since, diff)
lim = levels(map$cut_year)

g = ggplot(map, aes(long, lat, group=group)) + # the data
    geom_polygon(color="white", lwd=.04, aes(fill=cut_year)) + # make polygons w fill and outline
    scale_fill_discrete(palette=cols, breaks=diff, limits=lim) + 
    geom_polygon(aes(long, lat, group=group), fill="black", color="white", lwd=.04, 
                 data=map[is.na(map$years_since) | (map$id=="ESH" & !is.na(map$id)), ]) +
    #geom_polygon(color="grey50", fill=NA, lwd=0.05, data=world) +
    theme(axis.ticks=element_blank(),  # remove the background, tickmarks, etc
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank(), 
          text=element_text(size=15,  family="Times New Roman"))
ggplot2::ggsave(paste0(git_path, "/figures/raw/Figure_1g.pdf"), plot=g,  width=4.75, height=3.325)


model = lm(years_since ~ log(gdppc), data=pop[!is.infinite(pop$years_since),])
r2 = format(round(summary(model)$r.squared, 2), nsmall = 2)
lab = paste("r^2 =='", r2, "'")
g = ggplot(pop, aes(gdppc, years_since)) + geom_point(alpha=0.2) + 
    geom_smooth(method = "lm", se = F, alpha = 0.4, size = 0.5) + 
    annotate("text", x = 100, y = 41, label = lab, color = "grey46", hjust = 0, 
             vjust = 1, size = 12/2.5, family = "sans", parse = T) + ylim(2, 41) +
    scale_x_log10(breaks = c(100, 1000, 10000, 50000), limits=c(100, 50000)) +
    annotation_logticks(short = unit(-0.5,"mm"), mid = unit(-1.2,"mm"), long = unit(-2,"mm"), 
                        sides = "b", color = "#333333") + 
    coord_cartesian(clip="off") +
    theme_bw(base_family = "sans", base_size = 14) +
    theme(panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          plot.background = element_blank(),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          axis.line.x = element_line(color = "black", size = 0.2), 
          axis.line.y = element_line(color = "black", size = 0.2), 
          axis.text.x = element_text(vjust=0), 
          text=element_text(size=15,  family="Times New Roman")) +
    xlab("Log GDP per capita") + ylab("Years Since")
ggplot2::ggsave(paste0(git_path, "/figures/raw/Figure_1h.pdf"), plot=g,  width=2.25, height=2.25)

g =  panel(pop[!is.infinite(pop$years_since), 
              c("polity2", "years_since")], betal=T, font="Times New Roman", size=15) + 
    xlab("Polity") + ylab("Years Since")
ggplot2::ggsave(paste0(git_path, "/figures/raw/Figure_1i.pdf"), plot=g,  width=2.25, height=2.25)
