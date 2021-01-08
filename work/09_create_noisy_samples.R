source("init.R")
setwd(data_path)

########################################################################################
# Written by: Anne, July 2019
# Last edited by: Anne, Sept 2019
# This file adds additional noise to the DHS index responses, and finds model performance
#   metrics for different levels of extra noise.
########################################################################################


##########################################################################################
# functions
##########################################################################################

model = function(train, test, linear=T) {
  
  #train model
  if (linear) {
    m = lm(index ~ ., train[,c(2, 5:ncol(train))])
  } else {m = randomForest(train[,5:ncol(train)], train$index)}
  
  #predict on the test set
  preds = predict(m, test[5:ncol(test)])
  true_test_cor = cor(preds, test$true_index)^2 
  test_cor = cor(preds, test$index)^2 
  
  return(c(true_test_cor, test_cor))
}
get_hist = function(df, bins = 10) {
  breaks = seq(0, 255, by=255/bins)
  breaks = breaks[2:length(breaks)]
  
  x = apply(df, 1, function(x) {sapply(breaks, function(y) {sum(x <= y, na.rm=T)})})
  x = as.data.frame(t(x))
  
  y = cbind(rep(0, nrow(x)), x[1:(ncol(x)-1)])
  z = x - y
  
  return(z)
}


##########################################################################################
# Read in DHS data
##########################################################################################

#bring in the dhs data and get cluster level index
dhs_data = read.csv("jitter-impact/jittered-dhs/dhs_features_jitter0_1.csv")
dhs_data = cbind(dhs_data$id, get_hist(dhs_data[9:204], bins=35))
names(dhs_data)[1] = "id"

dhs = readRDS(paste0(dhs_path, "/output/DHS_output_DHS6_7_2019-03-19.RDS"))
dhs$n = 1
dhs$id = paste(dhs$cname, dhs$year, dhs$cluster_id)
dhs = dhs[dhs$id %in% dhs_data$id, ]
dhs[dhs==99] = NA
dhs[dhs==9] = NA
dhs = dhs[complete.cases(dhs[, c(10:15, 18)]), ] # only using binaries
dhs$index = stata_index(dhs[, c(10:15, 18)])
num = aggregate(dhs$n, by = list(dhs$id), FUN=sum)
dhs = merge(dhs, num, by.x="id", by.y="Group.1")
dhs = dhs[dhs$x >= 15, ]
true_villages = aggregate(dhs[, "index"], by=dhs[, c("id", "x")], FUN=mean)
names(true_villages) = c("id", "n", "true_index")

dhs_agg = dhs %>% group_by(id) %>% summarise(index = mean(index, na.rm=T), n=n())
dhs_agg = merge(dhs_agg, dhs_data, by="id")


##########################################################################################
# Test model performance when downsampling village size
##########################################################################################

#using leave group out cv
maxx = 15
num_samples = 10
num_group_out = 1
row = 1
n = maxx*num_samples*num_group_out
output = data.frame(n = rep(NA, n), fold = rep(NA, n), run = rep(NA, n), 
                    true_test_cor = rep(NA, n), test_cor = rep(NA, n))

for (i in 1:maxx) {
    
    set.seed(98501) #so each n is reproducible
    
    for (j in 1:num_samples) {
        downsampled_villages = dhs %>% group_by(id) %>% sample_n(i, replace=F)
        if (length(unique(downsampled_villages$id)) == nrow(downsampled_villages) & i != 1) {
            print("nothing to aggregate")
        } else {
            downsampled_villages = aggregate(downsampled_villages[, "index"], 
                                             by=list(downsampled_villages$id), FUN=mean)
        }
        
        names(downsampled_villages) = c("id", "index")
        
        downsampled_villages = merge(downsampled_villages, true_villages, by="id")
        downsampled_villages = merge(downsampled_villages, dhs_data, by="id")
        
        for (k in 1:num_group_out) {
            
            train = sample_frac(downsampled_villages, size=.7)
            test = downsampled_villages[!downsampled_villages$id %in% train$id, ]
            cors = model(train, test)
            output[row, ] = c(i, k, j, cors)
            row = row + 1
            
        }
    }
    
    print(i)
}

output = output[complete.cases(output), ]
c = tidyr::gather(output, type, r2, true_test_cor:test_cor, factor_key=TRUE)

c = aggregate(c$r2, by=c[, c("n", "type")], 
              FUN = function(x) c(mean = mean(x, na.rm=T), max = max(x, na.rm=T), 
                                  min = min(x, na.rm=T), sd=sd(x, na.rm=T)))
c = do.call(data.frame, c)
write.csv(c, "sample_model.csv", row.names = F)

c = c[c$type %in% c("test_cor", "true_test_cor"),]

m = mean(dhs$index)
group_mean = dhs %>% group_by(id) %>% summarise(group_mean = mean(index))
dhs = merge(dhs, group_mean, by="id")
total_var = sum((dhs$index-m)^2)
within_var = sum((dhs$index - dhs$group_mean)^2)
group_var = total_var - within_var
perc_var_group = group_var/total_var


##########################################################################################
# Test model performance when random noise is added to the asset wealth index
##########################################################################################

#using leave group out cv
maxx = 0.43 # half of the sd of dhs_agg$index
num_samples = 10 
num_group_out = 1
row = 1
n = maxx*num_samples*num_group_out
output = data.frame(n = rep(NA, n), fold = rep(NA, n), run = rep(NA, n), 
                    true_test_cor = rep(NA, n), test_cor = rep(NA, n))

for (i in seq(0, maxx, length.out=20)) {
    
    # loop through to create different levels of noise
    set.seed(98501) #so each n is reproducible
    
    for (j in 1:num_samples) {
        
        # create a new randomized version of the questions
        randomized_villages = dhs_agg
        randomized_villages$true_index = randomized_villages$index
        randomized_villages = randomized_villages[,c(1, 2, 39, 3, 4:38)]
        
        #adding rnorm(0, i) noise to the index
        randomized_villages$index = randomized_villages$index + rnorm(nrow(dhs_agg), 0, i)
        
        for (k in 1:num_group_out) {
            
            # split in to train and test k times
            train = sample_frac(randomized_villages, size=.7)
            test = randomized_villages[!randomized_villages$id %in% train$id, ]
            cors = model(train, test)
            output[row, ] = c(i, k, j, cors)
            row = row + 1
            
        }
    }
    
    print(i)
}

output = output[complete.cases(output), ]
c = tidyr::gather(output, type, r2, true_test_cor:test_cor, factor_key=TRUE)

c = aggregate(c$r2, by=c[, c("n", "type")], 
              FUN = function(x) c(mean = mean(x, na.rm=T), max = max(x, na.rm=T), 
                                  min = min(x, na.rm=T), sd=sd(x, na.rm=T)))
c = do.call(data.frame, c)
write.csv(c, "responsenoise_model.csv", row.names = F)
