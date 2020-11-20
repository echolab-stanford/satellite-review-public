library(sf)
library(data.table)
library(tidyr)
library(dplyr)
library(stringr)
source("00_init.R")

########################################################################################
# Last edited by: Anne, Aug 2020
# Plot figure panels on overall revisit rates using LandInfo data. Creates Figure 3 A-C
########################################################################################

setwd(paste0(data_path, "/pop_samples_overpass"))

getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

reverselog_trans = function(base = 10) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    trans_new(paste0("reverselog-", format(base)), trans, inv, 
              log_breaks(base = base), 
              domain = c(1e-100, Inf))
}

# figure out set of ids
files = list.files()
files = files[grepl(".kml", files)]

output = as.list(rep(NA, length(files)))
i = 1


######################################################################
# read in the data from landinfo and structure correctly
#   these data are manually downloaded from landinfo using the locations sampled in 
#   02_sample_locations_revisit
######################################################################

for (f in files) {
    
    # find right file and get the layer names
    ll = st_layers(f)  #this gives you total satellites
    ll = ll$name
    ll = ll[!grepl("Polygon", ll)]
    
    # get resolution for each image
    res = sapply(ll, function(x) {
        y = read_sf(f, layer = x)
        x = regexpr("[0-9]+.?m", y$Description)
        res = substr(y$Description, x, x+attr(x, "match.length")-1)
        
        x = regexpr("[0-9]+%", y$Description)
        x = substr(y$Description, x, x+attr(x, "match.length")-1)
        cloud = as.numeric(substr(x, 1, nchar(x)-1))
        
        x = regexpr("[0-9.]+°", y$Description)
        x = substr(y$Description, x, x+attr(x, "match.length")-1)
        nadir = as.numeric(substr(x, 1, nchar(x)-1))
        
        return(list(res, cloud, nadir))
    })
    nadir = unlist(res[3,])
    cloud = unlist(res[2,])
    res = unlist(res[1,])
    
    # get the name of hte satellite it came from
    sat = sapply(names(res), function(x) {strsplit(x, "  -  ")[[1]][1]})
    date = sapply(names(res), function(x) {strsplit(x, "  -  ")[[1]][2]})
    date = str_trim(gsub("\\(#[0-9]\\)", "", date))
    year = substr(date[1], 1, 4)
    id = str_split(f, "_")[[1]]
    id = paste(substr(id[1], nchar(id[1]), nchar(id[1])), gsub(".kml", "", id[3]), sep="_")
    location = str_split(f, "_")[[1]][1]
    location = gsub("[0-9]*", "", location)
    
    # combine data and save
    df = data.frame(year=year, id=id, date=date, satellite=sat, cloud=cloud, 
                    nadir=nadir, resolution=res, location=location)
    #df = df[df$cloud<=30 & df$nadir<=20, ]
    df = df[!duplicated(df[, c("satellite", "date")]), ] #only keep first fly over by day
    output[[i]] = df
    i = i + 1
    
    if (i %% 50 == 0) {print(i)}
    
}

o = rbindlist(output)
o = o[!is.na(o$date), ]
o$resolution = gsub("[A-Za-z]+", "", o$resolution)
o$resolution = as.numeric(as.character(o$resolution))


######################################################################
# aggregate landinfo data
######################################################################

o$satellite = as.character(o$satellite)
o$satellite[o$satellite %in% c("SPOT-4", "SPOT-5")] = "SPOT 4/5"
o$satellite[o$satellite %in% c("SPOT-6", "SPOT-7")] = "SPOT 6/7"
o$satellite[o$satellite %in% c("Pléiades-1A", "Pléiades-1B")] = "Pléiades"
o$satellite[o$satellite %in% c("KOMPSAT-3", "KOMPSAT-3A", "KOMPSAT-2")] = "KOMPSAT"
o$satellite[o$satellite %in% c("WorldView-1", "WorldView-2", "WorldView-3", "WorldView-4",
                               "GeoEye-1", "QuickBird-2", "IKONOS")] = "DigitalGlobe"

o = o %>% filter(!satellite %in% c("Copernicus Sentinel-2","Landsat8","RapidEye","SkySat") 
                 & !is.na(satellite)) %>% 
    group_by(satellite) %>% mutate(resolution = getmode(resolution))
o = o %>% 
    dplyr::group_by(year, satellite, resolution, location) %>% 
    summarise(n = n(), revisit = (365*100)/n) %>% as.data.frame()


######################################################################
# read in the GEE data and aggregate
######################################################################

# read in the GEE file for S2, Landsat, and modis
read_in = function(file, year) {
    x = read.csv(file)[, c("num_l5", "num_l7", "num_l8", "num_modis", "num_s2")]
    x = x %>% summarise(LandSat = 100*365 / (sum(num_l5)+sum(num_l7)+sum(num_l8)),
                        MODIS = 100*365 / sum(num_modis), Sentinel = 100*365 / sum(num_s2))
    x = gather(x, satellite, revisit,LandSat:Sentinel, factor_key=TRUE)
    x = x %>% mutate(year=year, n=NA)
}

w = read_in("Africa_overpass_2010-01-01.csv", 2010)
x = read_in("Africa_overpass_2019-01-01.csv", 2019)
y = read_in("Europe_overpass_2010-01-01.csv", 2010)
z = read_in("Europe_overpass_2019-01-01.csv", 2019)
x = rbind(w, x)
x$location = "Africa"
y = rbind(y, z)
y$location = "Europe"
y = rbind(x, y)

res = data.frame(satellite=c("LandSat", "MODIS", "Sentinel"), 
                 resolution=c(3000, 50000, 1000))
y = merge(y, res, by="satellite")
y = y[, c("year", "satellite", "resolution", "location", "n", "revisit")]

o = rbind(o, y)

######################################################################
# read in the data from planet and structure to match
######################################################################

e_planet = read.csv("europe_loc/europe_bbox.csv")
e_planet$location = "Europe"
a_planet = read.csv("africa_loc/africa_bbox.csv")
a_planet$location = "Africa"
planet = rbind(e_planet, a_planet)

planet = planet %>% group_by(location) %>%
    summarise(ps2010 = 100*365 / sum(num_PS_2010), ps2019 = 100*365 / sum(num_PS_2019),
              re2010 = 100*365 / sum(num_RE_2010), re2019 = 100*365 / sum(num_RE_2019), 
              ss2010 = 100*365 / sum(num_SS_2010), ss2019 = 100*365 / sum(num_SS_2019))
planet = gather(planet, year, revisit, ps2010:ss2019, factor_key=TRUE)
planet$satellite = c(rep("Planet", 4), rep("RapidEye", 4), rep("SkySat", 4))
planet$year = as.numeric(substr(planet$year, 3, 7))
planet$resolution = c(rep(370, 4), rep(500, 4), rep(80, 4))
planet$n = NA
planet = planet[, c("year", "satellite", "resolution", "location", "n", "revisit")]
o = rbind(o, planet)

o$pix_per = (1000^2)/(o$resolution^2)
o$revisit[is.infinite(o$revisit)] = NA

saveRDS(o, "revisit_rate.RDS")


######################################################################
# get all the plots ready
######################################################################

which_10 = unique(o$satellite[o$year == "2010" & !is.na(o$revisit)])
which_19 = unique(o$satellite[o$year == "2019" & !is.na(o$revisit)])
which_new = c(setdiff(which_19, which_10), setdiff(which_10, which_19))

private = c("DigitalGlobe", "RapidEye", "SkySat", "Pléiades", "TripleSat", 
            "SkySat", "Planet")

# Get the PPF ----------------------------------------------------------------------------

get_ppf = function(ppf, n=100) {
    res = sort(unique(ppf$resolution))
    res = c(res, max(res)+500)
    get_ppf = data.frame(res = res, revisit = NA)
    ppf$n[is.na(ppf$n)] = (100*365)/ppf$revisit[is.na(ppf$n)]
    
    for (i in 1:length(res)) {
        cur = ppf[ppf$resolution<=res[i],]
        revisit = (n*365)/sum(cur$n, na.rm=T)
        get_ppf[i, 2] = revisit
    }
    get_ppf_2 = data.frame(res = get_ppf$res, 
                       revisit = c(50000, get_ppf$revisit[-nrow(get_ppf)]))
    get_ppf = rbind(get_ppf_2, get_ppf)
    get_ppf = get_ppf[order(get_ppf$res, -get_ppf$revisit),]
    get_ppf = unique(get_ppf)
    return(get_ppf)
}

# Plot 2019 Africa -----------------------------------------------------------------------
ppf_2010 =  o[o$year == 2010 & o$location == "Africa", ] 
ppf_2010 = get_ppf(ppf_2010, 100)
ppf = o[o$year == 2019 & o$location == "Africa", ] 
ppf = get_ppf(ppf, 100)

plot_lines = o$year == "2019" & o$location == "Africa"
g = ggplot(o[o$year == "2019" & o$location == "Africa", ]) + 
    geom_vline(xintercept=7, col="grey", alpha=0.5) + 
    geom_vline(xintercept=30, col="grey", alpha=0.5) + 
    geom_vline(xintercept=365, col="grey", alpha=0.5) + 
    geom_vline(xintercept=3650, col="grey", alpha=0.5) + 
    geom_vline(xintercept=18250, col="grey", alpha=0.5) + 
    
    geom_line(aes(revisit, res, group=1), data=ppf_2010, color="grey70") +
    geom_line(aes(revisit, res, group=1), data=ppf, color="grey40") +
    
    #create temporal breaks for week, 10, yr, 1000, 10 yr, 50 yr
    scale_x_continuous(trans=reverselog_trans(10), limits=c(5840, 0.77),
                       breaks = c(1, 7, 30, 365, 3650), 
                       labels = c("day", "week", "month", "year", "10 years")) +
    scale_y_continuous(trans=reverselog_trans(10), limits=c(50500, 10),
                       breaks = c(10, 50, 100, 500, 1000, 5000, 10000, 50000), 
                       labels = c(paste(c(10, 50), "cm"), 
                                  paste(c(1, 5, 10, 50, 100, 500), "m"))) + 
    
    theme_anne(size=28, font="Times New Roman") +
    xlab("cloud-free revisit rate") + ylab("Sensor resolution")
ggsave(paste0(git_path, "/figures/raw/Figure_3b.pdf"), g, width=8, height=7,
       device="pdf", useDingbats=F)

# Plot 2019 Africa vs EU -----------------------------------------------------------------
ppf = o[o$year == 2019 & o$location == "Africa", ] 
ppf_africa = get_ppf(ppf, 100)
ppf = o[o$year == 2019 & o$location == "Europe", ] 
ppf_europe = get_ppf(ppf, 100)

africa_lines = o$year == "2019" & o$location == "Africa"
europe_lines = o$year == "2019" & o$location == "Europe"
g = ggplot() + 
    geom_vline(xintercept=7, col="grey", alpha=0.5) + 
    geom_vline(xintercept=30, col="grey", alpha=0.5) + 
    geom_vline(xintercept=365, col="grey", alpha=0.5) + 
    geom_vline(xintercept=3650, col="grey", alpha=0.5) + 
    geom_vline(xintercept=18250, col="grey", alpha=0.5) + 
    theme_anne(size=28, font="Times New Roman") +
    
    geom_line(aes(revisit, res, group=1), data=ppf_europe, color="grey70") +
    geom_line(aes(revisit, res, group=1), data=ppf_africa, color="grey40") +
    
    #create temporal breaks for week, 10, yr, 1000, 10 yr, 50 yr
    scale_x_continuous(trans=reverselog_trans(10), limits=c(5840, 0.77),
                       breaks = c(1, 7, 30, 365, 3650), 
                       labels = c("day", "week", "month", "year", "10 years")) +
    scale_y_continuous(trans=reverselog_trans(10), limits=c(50501, 10),
                       breaks = c(10, 50, 100, 500, 1000, 5000, 10000, 50000), 
                       labels = c(paste(c(10, 50), "cm"), 
                                  paste(c(1, 5, 10, 50, 100, 500), "m"))) + 
    
    xlab("cloud-free revisit rate") + ylab("Sensor resolution")
ggsave(paste0(git_path, "/figures/raw/Figure_3c.pdf"), g, width=8, height=7,
       device="pdf", useDingbats=F)

# Plot just the points for 2019 ----------------------------------------------------------

plot_lines = o$year == "2019" & o$location == "Africa"
g = ggplot(o[o$year == "2019" & o$location == "Africa", ]) + 
    geom_vline(xintercept=7, col="grey", alpha=0.5) + 
    geom_vline(xintercept=30, col="grey", alpha=0.5) + 
    geom_vline(xintercept=365, col="grey", alpha=0.5) + 
    geom_vline(xintercept=3650, col="grey", alpha=0.5) + 
    geom_vline(xintercept=18250, col="grey", alpha=0.5) + 
    
    geom_point(aes(revisit, resolution), size=4, fill="grey40", color="grey40", shape=21, 
               o[plot_lines & !o$satellite %in% private, ]) +
    geom_point(aes(revisit, resolution), size=4, color="grey40", fill="orange", shape=21, 
               o[plot_lines & !o$satellite %in% private & o$satellite %in% which_new, ]) + 
    
    geom_point(aes(revisit, resolution), size=4, fill="grey40", color="grey40", shape=24, 
               o[plot_lines & o$satellite %in% private, ]) + 
    geom_point(aes(revisit, resolution), size=4, color="grey40", fill="orange", shape=24, lwd=1, 
               o[plot_lines & o$satellite %in% private & o$satellite %in% which_new, ]) + 
    
    #create temporal breaks for week, 10, yr, 1000, 10 yr, 50 yr
    scale_x_continuous(trans=reverselog_trans(10), limits=c(5840, 0.77),
                       breaks = c(1, 7, 30, 365, 3650), 
                       labels = c("day", "week", "month", "year", "10 years")) +
    scale_y_continuous(trans=reverselog_trans(10), limits=c(50500, 10),
                       breaks = c(10, 50, 100, 500, 1000, 5000, 10000, 50000), 
                       labels = c(paste(c(10, 50), "cm"), 
                                  paste(c(1, 5, 10, 50, 100, 500), "m"))) + 
    
    theme_anne(size=28, font="Times New Roman") +
    ggrepel::geom_text_repel(size=7, aes(revisit, resolution, label=satellite),
                             hjust=-0.1, vjust=0, box.padding=0.27, 
                             data=o[o$year == "2019" & o$location == "Africa", ], 
                             family="Times New Roman") +
    xlab("cloud-free revisit rate") + ylab("Sensor resolution")
ggsave(paste0(git_path, "/figures/raw/Figure_3a.pdf"), g, width=8, height=7,
       device="pdf", useDingbats=F)
       