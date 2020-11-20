library(rgdal)
library(rgeos)
library(raster)
source("00_init.R")
setwd(data_path)
setwd("..")

########################################################################################
# Last edited by: Anne, Aug 2020
# Used to sample 100 locations each from africa and europe, as pulled weighted by 
#   population from a global population raster. These locations are used to find the 
#   revisit rate of satellites as those locations to create Fig 3
########################################################################################

# which countries to keep
europe = c("United States", "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
           "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", 
           "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
           "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", 
           "Slovenia", "Spain", "Sweden")
africa = c("Angola", "Benin", "Burkina Faso", "Cameroon", "Côte d'Ivoire", 
           "Democratic Republic of the Congo", "Ethiopia", "Ghana", "Guinea", "Kenya", 
           "Lesotho", "Malawi", "Mali", "Mozambique", "Nigeria", "Rwanda", "Senegal", 
           "Sierra Leone", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe")

# read in the world and only keep africa, europe, us
world = readOGR("boundaries/gadm/gadm36_0", "gadm36_0")
world = world[world$NAME_0 %in% c(africa, europe),]
w = gSimplify(world, 0.3, topologyPreserve = T)
row.names(world) = names(w)
w = SpatialPolygonsDataFrame(w, world@data)
africa = w[w$NAME_0 %in% africa, ] 
europe = w[w$NAME_0 %in% europe, ] 

# read in pop and 
pop = raster("population/gpw_v4_population_count_rev11_2010_2pt5_min.tif")
e_pop = crop(pop, europe)
e_pop = mask(e_pop, europe)

a_pop = crop(pop, africa)
a_pop = mask(a_pop, africa)


get_pop_vals = function(pop, n=100, seed=19472) {
    # get the values to sample from
    x = values(pop)
    x[is.na(x)] = 0
    
    # sample
    set.seed(seed)
    s = sample(1:length(pop), n, prob=x)
    
    # get the actual locations of the cells
    locations = xyFromCell(pop, s)
    locations = SpatialPoints(locations)
    locations = gBuffer(locations, width=0.0001, byid=T)
    locations = SpatialPolygonsDataFrame(locations, as.data.frame(coordinates(locations)))
    
    return(locations)
}

a_loc = get_pop_vals(a_pop, seed=11293485)
a_loc$id = 1:nrow(a_loc)
writeOGR(a_loc, "satellite_review_paper/pop_samples_overpass/africa_loc", 
         "africa_loc", driver="ESRI Shapefile")

e_loc = get_pop_vals(e_pop)
e_loc$id = 1:nrow(e_loc)
writeOGR(e_loc, "satellite_review_paper/pop_samples_overpass/europe_loc", 
         "europe_loc", driver="ESRI Shapefile")
