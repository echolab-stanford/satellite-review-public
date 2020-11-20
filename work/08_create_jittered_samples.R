source('00_init.R')
setwd(paste0(data_path, "/jitter-impact"))

########################################################################################
# Written by: Anne, July 2019
# Last edited by: Anne, Sept 2019
# We want to look at error as a function of additional jitter. To do that we want to 
#   create models that are trained by additionally jittered data. This file uses 
#   nightlights data to pull the "true" location of the DHS sample, plus several 
#   runs of randomly adding even more jitter.
# This file saves the nightlights data for each DHS point, and then for each DHS point 
#   randomly jittered 100 times and saves that data under "dhs_features_jitter"
########################################################################################

#figures out what ids from a vector would correspond to a square in a matrix
box_to_vec = function(x, y, box_dim, total_dim) {
  #x and y are the jittered point locations
  #box_dim is how much to take
  #total_dim is edge length of matrix
  
  length = total_dim^2
  x = x %% total_dim
  
  if (box_dim %% 2 != 0) {
    dir_left = dir_right = (box_dim-1)/2
  } else {
    dir_right = box_dim/2
    dir_left = dir_right - 1
  }
  
  minx = x - dir_left
  maxx = x + dir_right
  
  miny = y - dir_left
  maxy = y + dir_right
  
  ids = as.list(rep(NA, length(minx)))
  
  for (i in 1:length(ids)) {
    cols = (minx[i]:maxx[i]) - 1  #minus 1 bcz you don't add anything to the num in first row
    rows = (miny[i]:maxy[i])
    cols = cols*total_dim
    id = sapply(rows, FUN=function(x) {x + cols})
    ids[[i]] = as.vector(t(id))
  }
  
  ids = as.data.frame(do.call(rbind, ids))
  
  return(ids)
}

#takes an input lat/lon and jitters it in a circle with radius tol (in km)
#gets the x/y in terms of pixels based on the km you want to jitter
sample_jitter = function(n, center, tol=5) {
  
  tol = floor(tol*2)
  
  #randomize how far from original point
  random_distance = sample(0:tol, n, replace=T) 
  #randomize direction moved in xy
  lat_sign = sample(c(-1, 1), n, replace=T)
  lon_sign = sample(c(-1, 1), n, replace=T)
  
  #randomize the lat difference (up to chosen distance)
  lat_change = sapply(X = random_distance, FUN= function(y) {sample(0:y, 1)})
  #calculate lon difference that results in correct 
  lon_change = round(sqrt(random_distance^2 - lat_change^2))
  
  #add the jitter
  new_lat = center + (lat_sign*lat_change)
  new_lon = center + (lon_sign*lon_change)
  
  return(data.frame(new_lat, new_lon))
}

#takes a vector of values and counts how many in each equally divided bin
get_hist = function(df, bins = 10) {
  breaks = seq(0, 255, by=255/bins)
  breaks = breaks[2:length(breaks)]
  
  x = apply(df, 1, function(x) {sapply(breaks, function(y) {sum(x <= y, na.rm=T)})})
  x = as.data.frame(t(x))
  
  y = cbind(rep(0, nrow(x)), x[1:(ncol(x)-1)])
  z = x - y
  
  return(z)
}

#takes lat/lon df and velox, and extracts `km` squared boxes around each lat/lon passed
extract_boxes = function(ll_temp, vx, dhs, km=6.72){
  
  km_deg = 0.0089982311916
  radius = (km/2)*km_deg
  
  #create the bounds of the 7km boxes around each of the locations
  y_max = ll_temp$lat+(radius)
  x_max = ll_temp$lon+(radius)
  y_min = ll_temp$lat-(radius)
  x_min = ll_temp$lon-(radius)
  square = cbind(x_min, y_max, x_max, y_max, x_max, y_min, x_min,y_min, x_min, y_max)
  
  #make those boxes in to spatial polygons
  sample = SpatialPolygons(mapply(function(poly, id) 
            {xy = matrix(poly, ncol=2, byrow=TRUE)
            Polygons(list(Polygon(xy)), ID=id)}, 
            split(square, row(square)), ll_temp$id),
            proj4string=CRS("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  x = Sys.time()
  
  #extract those polygons from the velox
  features = vx$extract(sp=sample, fun=NULL) #4 minutes to run
  y = Sys.time()
  print(sprintf("For feature extract: %s", y-x))
  
  #reattach the features we need
  features = as.data.frame(t(sapply(features, `length<-`, max(lengths(features)))))
  features$id = row.names(features)
  dhs_merge = merge(dhs, features, by="id", all.x=T)
  
  #choose the columns we want
  #dhs_merge = dhs_merge[, 1:(ceiling(km*2)^2 + ncol(dhs))]
  
  return(dhs_merge)
}

#takes data, ground truth, and model type and returns performance metrics
model = function(train, test_jitter, test, linear=T) {
  
  #train model
  if (linear) {
    m = lm(index ~ ., train[,c(8:ncol(train))])
  } else {m = randomForest(train[,9:ncol(train)], train$index)}
  
  #predict on the true set
  preds = predict(m, test[9:ncol(test)])
  true_cor = cor(preds, test$index)^2 
  
  #predict on the test set
  preds = predict(m, test_jitter[9:ncol(test_jitter)])
  test_cor = cor(preds, test_jitter$index)^2 
  
  #predict on the train set
  preds = predict(m, train[9:ncol(train)])
  train_cor =cor(preds, train$index)^2
  
  return(c(true_cor, test_cor, train_cor))
}

#bring in the dhs data and get cluster level index
dhs = readRDS(paste0(dhs_path, "/output/DHS_output_DHS6_7_2019-03-19.RDS"))
dhs = dhs[dhs$cname %in% c("ZA", "ET", "MZ", "MW", "EG", "RW", "LS", "ZM", "SL"), ]
dhs = dhs[complete.cases(dhs[, c(10:15, 18)]), ]
dhs$index = stata_index(dhs[, c(10:15, 18)])
dhs$sum = rowSums(dhs[, c(10:15, 18)])
dhs = aggregate(dhs[, c("index", "sum")], by=dhs[, c("cluster_id", "year", "cname", "urban", "lat", "lon")], FUN=mean)
dhs$id = paste(dhs$cname, dhs$year, dhs$cluster_id)
dhs = dhs[complete.cases(dhs),]
ll = unique(dhs[, c("lon", "lat", "id")])

#bring in the raster NL data and convert to velox
#source: https://earthobservatory.nasa.gov/features/NightLights/page3.php
nl = raster("BlackMarble_2016_Africa.tif")
vx = velox(nl)

####################################
#create training data
####################################

fullboxes = extract_boxes(ll, vx, dhs, km=70) #6 minutes
fullboxes = fullboxes[order(fullboxes$lat, fullboxes$lon), ] #each row can be different num of col/row
fullboxes$vals = apply(fullboxes, 1, function(x)  max(which(!is.na(x[9:ncol(fullboxes)])))) #how many vals in each row
vals = sort(unique(fullboxes$vals))
goal_size = sqrt(min(vals))
for (i in 1:length(vals)) {
  if (vals[i] != goal_size^2) {
    if (vals[i] == goal_size*(goal_size+1)) { #has an extra column or row so we just drop it
      rows = which(fullboxes$vals == vals[i]) 
      fullboxes[rows, 9:ncol(fullboxes)] = NA
    } else if (vals[i] == (goal_size+1)^2) { #has an extra row and column so we drop a row and col
      rows = which(fullboxes$vals == vals[i]) 
      for (j in rows) {
        temp = matrix(as.numeric(fullboxes[j, 9:(vals[i]+8)]), ncol=goal_size+1, byrow=F)
        fullboxes[j, 9:((goal_size^2)+8)] = temp[1:goal_size, 1:goal_size]
      }
    } else { #isn't a shape we know, so we drop 
      rows = which(fullboxes$vals == vals[i])
      fullboxes[rows, 9:ncol(fullboxes)] = NA
    }
  }
}
fullboxes = fullboxes[, 1:(goal_size^2 +8)]
fullboxes = fullboxes[complete.cases(fullboxes),]
extraction = as.matrix(fullboxes[, 9:ncol(fullboxes)])
ll = unique(fullboxes[, c("lon", "lat", "id")]) # get the lat lon points from the dhs data
ll = ll[complete.cases(ll),]


jitter_levels = c(0:25)
n = 200
km_deg = 0.0089982311916
km = 6.72
pixels = ceiling(km*2)
box_size = sqrt(ncol(fullboxes)-8)

for (j in 1:length(jitter_levels)){ #for each level of jitter
  
  set.seed(8472011) #inside loop so a single jitter level is reproducible without running everything
  print(sprintf("For jitter level %s.", jitter_levels[j]))
  
  for (k in 1:n) {
    
    #jitter the lat/lon
    ll_temp = sample_jitter(nrow(ll), center=round(box_size/2), tol=jitter_levels[j]) #jitter the points
    
    #get x pixels from around each jittered point
    ids = box_to_vec(ll_temp$new_lat, ll_temp$new_lon, pixels, box_size) #figure out ids 
    ids$row = seq(1:nrow(ids)) #id to which point the boxes are from
    ids = tidyr::gather(ids, "x", "column", 1:(ncol(ids)-1)) #turn to matrix of x,y to query
    ids = as.matrix(ids[, c("row", "column")])
    
    extract = as.data.frame(matrix(NA, nrow(fullboxes), (pixels^2)+8)) #create empty matrix
    #populate with dhs stuff
    extract[, 1:8] = fullboxes[, c("id","cluster_id","year","cname","urban","lat","lon","index")]
    names(extract)[1:8] = c("id","cluster_id","year","cname","urban","lat","lon","index")
    #query extraction with x/y points, and coerce to matrix with correct # rows
    extract[, 9:ncol(extract)] = matrix(extraction[ids], nrow = nrow(extract))
    
    write.csv(extract, paste0("jittered-dhs/dhs_features_jitter", jitter_levels[j],"_", k, ".csv"), row.names=F) #save
    
    svMisc::progress(k, max.value=n)
    
    if (jitter_levels[j] == 0) {
      #only do once if it's unjittered
      next
    }
  }
  
  svMisc::progress(j, max.value=length(jitter_levels))
}


####################################
#read in training data, model it and save
####################################

#settings
linear = T
bins = 35
num_splits = 4
kfolds = T
  folds = 6
size = 0.4 #used if kfolds = F

#load in
files = list.files("jittered-dhs")  #get list of all the saved jittered spots
files = files[grepl("r[0-2]*[0-9]_", files)]
gt = as.data.frame(data.table::fread("jittered-dhs/dhs_features_jitter0_1.csv"))
gt = cbind(gt[,1:8], get_hist(gt[,9:204], bins))
countries = as.list(as.character(unique(gt$cname)))
n = length(files) * length(countries) * num_splits
if (kfolds) {n = n * folds}

#create empty matrix with a row for each file*num countries*num loops*num folds
output = matrix(rep(NA, n*8), ncol=8)
row = 1

for (i in 1:length(files)) {
  
  #read in file, find jitter level
  f = files[i]
  num = gsub("[A-Za-z.]+", "", f)
  num = strsplit(substr(num, 3, nchar(num)), "_")[[1]]
  jitter = as.numeric(num[1])
  num = as.numeric(num[2])
  if (!is.na(jitter)) {if (jitter > 25) {next}} else {next}
  
  #read in the file and convert to bins
  dhs_merge = as.data.frame(data.table::fread(paste0("jittered-dhs/", f)))
  dhs_merge = cbind(dhs_merge[,1:8], get_hist(dhs_merge[,9:204], bins))
  
  #for each country build a model and save the performance in output
  for (j in 1:length(countries)) {
    
    #here so that any file or country is individually reproducible
    #not ideal because that means that drawing the same sample for each 
    #country/jitter_run combo. 
    set.seed(9234701)
    
    c_merge = dhs_merge[dhs_merge$cname %in% countries[[j]], ]
    
    for (l in 1:num_splits) {
      
      if (kfolds) {
        
        #number of folds runs
        folds_i = caret::createFolds(c_merge$index, folds)
        
        for (k in 1:length(folds_i)) {
          
          #for each fold build a model and save
          
          train = c_merge[-folds_i[[k]], ]
          test_jitter = c_merge[!c_merge$id %in% train$id, ]
          test = gt[gt$id %in% test_jitter$id, ]
          
          cor = model(train, test_jitter, test, linear=linear)
          output[row, ] = c(cor[3], cor[2], cor[1], jitter, num, l, k, as.character(countries[[j]]))
          row = row+1
        }
      } else {
        train = dplyr::sample_frac(c_merge, size=size)
        test_jitter = c_merge[!c_merge$id %in% train$id,]
        test = gt[gt$id %in% test_jitter$id,]
        
        cor = model(train, test_jitter, test, linear=T)
        output[row, ] = c(cor[3], cor[2], cor[1], jitter, num, l, NA, as.character(countries[[j]]))
        row = row+1
      }
      
    }
    
  }
  
  if (i%%10 == 0) {
    svMisc::progress(i, max.value=length(files))
  }
}

output = as.data.frame(output)
names(output) = c("r2_train", "r2_test", "r2_true", "jitter", "jitter_num", "run_num", "fold_num", "countries")

#complete cases, add 5 to jitter, and save file
output = output[complete.cases(output), ]
output$jitter = as.numeric(as.character(output$jitter)) + 5
output[is.na(output$jitter), "jitter"] = Inf
output$r2_train = as.numeric(as.character(output$r2_train))
output$r2_test = as.numeric(as.character(output$r2_test))
output$r2_true = as.numeric(as.character(output$r2_true))

write.csv(output, paste0("dhs_jitter_results_linear",linear,"_histbins", bins, 
                         "_", folds, "foldsby", num_splits, "_jitters.csv"), row.names = F)



####################################
#read in training data, model it and save THIS TIME FOR ALL DATA
####################################

#settings
linear = T
bins = 35
num_splits = 4
kfolds = T
folds = 6
size = 0.4 #used if kfolds = F

#load in
files = list.files("jittered-dhs")  #get list of all the saved jittered spots
files = files[grepl("r[0-2]*[0-9]_", files)]
gt = as.data.frame(data.table::fread("jittered-dhs/dhs_features_jitter0_1.csv"))
gt = cbind(gt[,1:8], get_hist(gt[,9:204], bins))
n = length(files) * num_splits
if (kfolds) {n = n * folds}

#create empty matrix with a row for each file*num loops*num folds
output = matrix(rep(NA, n*7), ncol=7)
row = 1

for (i in 1:length(files)) {
    
    #read in file, find jitter level
    f = files[i]
    num = gsub("[A-Za-z.]+", "", f)
    num = strsplit(substr(num, 3, nchar(num)), "_")[[1]]
    jitter = as.numeric(num[1])
    num = as.numeric(num[2])
    if (!is.na(jitter)) {if (jitter > 25) {next}} else {next}
    
    #read in the file and convert to bins
    dhs_merge = as.data.frame(data.table::fread(paste0("jittered-dhs/", f)))
    dhs_merge = cbind(dhs_merge[,1:8], get_hist(dhs_merge[,9:204], bins))
    
    #here so that any file or country is individually reproducible
    #not ideal because that means that drawing the same sample for each 
    #country/jitter_run combo. 
    set.seed(9234701)
    
    c_merge = dhs_merge
    
    for (l in 1:num_splits) {
        
        if (kfolds) {
            
            #number of folds runs
            folds_i = caret::createFolds(c_merge$index, folds)
            
            for (k in 1:length(folds_i)) {
                
                #for each fold build a model and save
                
                train = c_merge[-folds_i[[k]], ]
                test_jitter = c_merge[!c_merge$id %in% train$id, ]
                test = gt[gt$id %in% test_jitter$id, ]
                
                cor = model(train, test_jitter, test, linear=linear)
                output[row, ] = c(cor[3], cor[2], cor[1], jitter, num, l, k)
                row = row+1
            }
        } else {
            train = dplyr::sample_frac(c_merge, size=size)
            test_jitter = c_merge[!c_merge$id %in% train$id,]
            test = gt[gt$id %in% test_jitter$id,]
            
            cor = model(train, test_jitter, test, linear=T)
            output[row, ] = c(cor[3], cor[2], cor[1], jitter, num, l, NA)
            row = row+1
        }
        
    }
    
    if (i%%10 == 0) {
        svMisc::progress(i, max.value=length(files))
    }
}

output = as.data.frame(output)
names(output) = c("r2_train", "r2_test", "r2_true", "jitter", "jitter_num", "run_num", "fold_num")

#complete cases, add 5 to jitter, and save file
output = output[complete.cases(output), ]
output$jitter = as.numeric(as.character(output$jitter)) + 5
output[is.na(output$jitter), "jitter"] = Inf
output$r2_train = as.numeric(as.character(output$r2_train))
output$r2_test = as.numeric(as.character(output$r2_test))
output$r2_true = as.numeric(as.character(output$r2_true))

write.csv(output, paste0("dhs_jitter_international_results_linear",linear,"_histbins", 
                         bins, "_", folds, "foldsby", num_splits, "_jitters.csv"),
          row.names = F)
