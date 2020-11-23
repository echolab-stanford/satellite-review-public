source("00_init.R")
library(tidyverse)
library(data.table)
library(grDevices)
library(wesanderson)
setwd(data_path)

########################################################################################
# Written by: Marshall Burke
# Last edited by: Anne, Nov 2020
# Creates Figure 5 A-D
########################################################################################

# process data for panels c and d

#kenya 2015 maize
kenya = read_csv('kenya.2015VI.updated11082016.csv')
kenya = kenya[duplicated(kenya[,c("fid.y","cropcode.x.y")])==F,]  #removing duplicated households
kenya = kenya %>% 
    mutate(yield=prod.kg.y/poly.area.y * 10, 
           area=poly.area.y/10000, 
           purestand=(intercrop.y=="no")) %>%
    rename(v1=may15.corrgcvi.1m, v2=jul3.corrgcvi.1m)  %>%
    dplyr::select(area,yield,v1,v2,purestand) %>% 
    na.omit()

# mali 2017 sorghum
mali = read_csv('mali_table_Wfieldsize_yields_harmpeaks.csv')
mali = mali %>% 
    mutate(yield = yield_cc/1e3, 
           area=area_ha, 
           v1=hpeakGCVI_cc, 
           purestand=TRUE) %>% 
    dplyr::select(area,yield,v1,purestand) %>% 
    na.omit()

# uganda maize
ug = read_csv('maps2016.dataforrep.csv')
ug = ug %>% 
    rename(area=plot_area_GPS,
           yield=fullplot_yield,
           v1=DOY_mean_151.6,
           v2=DOY_mean_171.6) %>%
    dplyr::select(area,yield,v1,v2,purestand) %>% 
    na.omit()


# performance as a function of # of training examples
samples=seq(5,300,5)

#define function to run 100 replications of train test split at each chosen # of training examples
runregs = function(dt,vars="v1") {
  nn = dim(dt)[1]
  trainsize = round(nn*0.75)
  est = c()
  for (j in 1:length(samples)) {
    if (trainsize>=samples[j]) {  #only run if training data is larger than required sample size
    r2 = rmse = c()
    for (k in 1:100) { #split data into train and test and run 100 replicates for each training sample size, average r2 over 100 replicates
      train_ind = sample(1:nn,trainsize,replace = F)
      train = dt[train_ind,]
      test = dt[-train_ind,]
      train = train[sample(1:dim(train)[1],samples[j],replace=F),] # sample dataset of required size from train
      fmla = as.formula(paste0("yield ~ ",vars))
      mod = lm(fmla, data=train)
      test$yhat = predict(mod,test)
      test$err = test$yhat - test$yield
      tmod = lm(yield ~ yhat, data=test)
      r2 = c(r2,summary(tmod)$r.squared)
      rmse = c(rmse,sqrt(mean(test$err^2)))
      }
      est = rbind(est,c(sampsize=samples[j],r2=mean(r2),rmse=mean(rmse)))
    } #end if
  }
  return(est)
  }

#function to calculate out of sample r2/rmse using all the data (i.e not limiting sample size)
bestmodel =  function(dt,vars="v1") {
  nn = dim(dt)[1]
  trainsize = round(nn*0.75)
   r2 = rmse = c()
    for (k in 1:100) { #split data into train and test and run 100 replicates for each training sample size, average r2 over 100 replicates
        train_ind = sample(1:nn,trainsize,replace = F)
        train = dt[train_ind,]
        test = dt[-train_ind,]
        fmla = as.formula(paste0("yield ~ ",vars))
        mod = lm(fmla, data=train)
        test$yhat = predict(mod,test)
        test$err = test$yhat - test$yield
        tmod = lm(yield ~ yhat, data=test)
        r2 = c(r2,summary(tmod)$r.squared)
        rmse = c(rmse,sqrt(mean(test$err^2)))
      }
      est = data.frame(r2=mean(r2),rmse=mean(rmse))
      return(est)
}


out = list()
out[[1]] = as.data.frame(runregs(ug %>% filter(area>0.1),vars="v1 + v2"))
out[[2]] = as.data.frame(runregs(kenya %>% filter(area>0.1),vars="v1 + v2"))
out[[3]] = as.data.frame(runregs(mali %>% filter(area>0.1),vars="v1"))

out[[1]]$type = "Uganda, maize"
out[[2]]$type = "Kenya, maize"
out[[3]]$type = "Mali, sorghum"

outb = list()
outb[[1]] = as.data.frame(bestmodel(ug %>% filter(area>0.1),vars="v1 + v2"))
outb[[2]] = as.data.frame(bestmodel(kenya %>% filter(area>0.1),vars="v1 + v2"))
outb[[3]] = as.data.frame(bestmodel(mali %>% filter(area>0.1),vars="v1"))


#####################
# performance as a function of field size

sizes = seq(0,0.5,0.02)
fieldsize =  function(dt,vars="v1") {
  est = c()
  for (i in sizes) { #split data into train and test and run 100 replicates for each training sample size, average r2 over 100 replicates
    ds = dt %>% filter(area>=i)
    n = dim(ds)[1]
    if (n>=20) { #only run if we have 20 obs or more
      fmla = as.formula(paste0("yield ~ ",vars))
      mod = lm(fmla, data=ds)
      est = rbind(est,c(size=i,n=n,r2=summary(mod)$adj.r.squared,rmse=sqrt(mean(mod$residuals^2))))
    } #end if
  }
  return(est)
}

outf = list()
outf[[1]] = as.data.frame(fieldsize(ug,vars="v1 + v2"))
outf[[2]] = as.data.frame(fieldsize(kenya,vars="v1 + v2"))
outf[[3]] = as.data.frame(fieldsize(mali,vars="v1"))

outf[[1]]$type = "Uganda, maize"
outf[[2]]$type = "Kenya, maize"
outf[[3]]$type = "Mali, sorghum"



##################################3
#  NOW make plots


# Panel A:  average yields by crop across published studies
dt = read_csv('SmallholderYieldEstimates.csv')
dt = rename(dt, resolution=`sensor resolution (meter)`, num_images=`number of images`, 
                obs=`number of obs`,year=`study year`,type=`training data`)
dt$crop = factor(dt$crop, levels = c("wheat", "cotton", "maize", "millet", "sorghum", 
                                     "rice", "beans"))

ggplot(dt) + 
    geom_point(aes(crop, r2), alpha=0.6, size=2) + 
    theme_anne(font="sans", size=5) + ylim(0, 1)
ggsave(paste0(git_path, "/figures/raw/Figure_5a.pdf"), width=2, height=2, useDingbats=F)

# Panel B - by type of training data
studies = data.frame(study=c("Jain et al 2016","Lobell et al 2020","Lobell et al 2019"),num=1:3)
type = data.frame(type=c("crop cuts","self reported",'full plot'),ctype=c("grey","black","white"))
dtt = dt %>% 
    filter(study %in% studies$study & sensor!="landsat")
dtt = dtt %>% 
    group_by(study, type) %>% 
    dplyr::summarise(r2=mean(r2)) 
dtt = left_join(dtt, studies) %>% as.data.frame()

# add in results for all crops across all studies, dropping full plot measurements (which we only have for uganda)  
add = dt %>% 
    group_by(type) %>% 
    dplyr::summarise(r2=mean(r2), n=n()) %>% 
    filter(type!="full plot") %>%
    mutate(study="all", num=4) %>%
    dplyr::select(study, type, r2, num) %>%
    as.data.frame()
dtt = rbind(dtt, add)
dtt = left_join(dtt, type)

lines = cbind(dtt[c(1,3,6,8), c("num", "r2")], dtt[c(2,4,7,9), "r2"])
names(lines) = c("x", "y1", "y2")

ggplot() + 
    geom_segment(data=lines, aes(x=x, xend=x, y=y1, yend=y2)) +
    geom_point(data=dtt, aes(num, r2, fill=type), pch=21, color="black", size=2) + 
    geom_text(aes(x=1:4, y=c(.35, .29, .6, .47),
                  label=c("India wheat","Mali sorghum","Uganda maize","all"))) +
    scale_fill_manual(values = c("grey99", "grey50", "grey0")) +
    theme_anne(font="sans") + ylim(0, 1) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          axis.line.x=element_blank()) +
    guides(fill=F)
ggsave(paste0(git_path, "/figures/raw/Figure_5b.pdf"), width=2, height=2, useDingbats=F)


# panel C - field size

d = rbindlist(outf)

ggplot(d) +  
    geom_point(aes(size, r2, color=type), alpha=0.5, size=0.5) +
    geom_smooth(aes(size, r2, color=type), se=F, method="loess",  lwd=0.2) +
    scale_color_manual(values = wes_palette("Darjeeling1")[3:5]) +
    theme_anne(font="sans") + guides(color=F) + 
    xlab("minimum field size (ha)") + ylab("r2") 
ggsave(paste0(git_path, "/figures/raw/Figure_5c.pdf"), width=2, height=2, useDingbats=F)


# panel D - number of training examples

b = rbindlist(outb)[, "rmse"]
names(b) = "max_rmse"
b$type = c("Uganda, maize", "Kenya, maize", "Mali, sorghum")

d = rbindlist(out) %>%
    filter(sampsize <= 150) %>%
    left_join(b) %>%
    group_by(type) %>%
    mutate(perc_rmse = (rmse/max_rmse-1)*100)

ggplot(d) + 
    geom_point(aes(sampsize, perc_rmse, color=type), alpha=0.5, size=0.5) +  
    geom_line(aes(sampsize, perc_rmse, group=type, color=type), lwd=0.2) +
    scale_color_manual(values = wes_palette("Darjeeling1")[3:5]) +
    guides(color=F) + 
    coord_cartesian(ylim=c(0, 61), clip="off") + theme_anne(font="sans") +
    xlab("training samples") + ylab("% change in RMSE relative to full model")
ggsave(paste0(git_path, "/figures/raw/Figure_5d.pdf"), width=2, height=2, useDingbats=F)
