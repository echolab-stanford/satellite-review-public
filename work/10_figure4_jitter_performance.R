source("00_init.R")
library(RColorBrewer)
library(boot)

########################################################################################
# Written by: Anne, Oct 2019
# Last edited by: Anne, Nov 2020
# This file creates Figure 4 based on the noisy data created in the previous two files
########################################################################################


#################################################################
#read in data
#################################################################
setwd(data_path)           
jitter_file = "jitter-impact/dhs_jitter_international_results_linearTRUE_histbins35_6foldsby4_jitters.csv"
sample_file = "jitter-impact/sample_model.csv"
response_file = "responsenoise_model.csv"
jitter = read.csv(jitter_file)
sample = read.csv(sample_file)
response = read.csv(response_file)
crop = read.csv("maps2016.dataforrep.csv")

#################################################################
#calculate mean jitter for each level we add
#################################################################
jitter_calc = function(xy, distance, og=F) {
  
  n = length(xy$x)
  
  d = runif(n, 0, distance)
  if(og) {
    id = sample(1:length(d), 0.05*length(d))
    d[id] = runif(length(id), -distance*2, distance*2)
  }
  
  angle = runif(n, 0, 2*pi)
  
  dx = d * cos(angle)
  dy = d * sin(angle)
  
  xy = data.frame(x = xy$x+dx, y = xy$y+dy)
  
  return(xy)
}

distance = function(xy) {
  d = sqrt(xy$x^2 + xy$y^2)
  return(d)
}

set.seed(1000)
jitter_means = 0:25
jitter_means = data.frame(jitter=jitter_means, mean=rep(NA, length(jitter_means)))
zero = data.frame(x=rep(0, 1000000), y=rep(0,1000000))
zero = jitter_calc(zero, 5)
jitter_means$mean[1] = mean(distance(zero))

for (i in 2:nrow(jitter_means)) {
  distance_jittered = jitter_means$jitter[i]
  jitter_means$mean[i] = mean(distance(jitter_calc(zero, distance_jittered)))
}

jitter_means$jitter = jitter_means$jitter + 5


#################################################################
#jitter figure
#################################################################

jitter = merge(jitter, jitter_means, by="jitter")[, 2:8]
names(jitter)[7] = "jitter"

jitter = tidyr::gather(jitter[,-5], type, r2, r2_train:r2_true, factor_key=TRUE)
jitter = jitter[!(jitter$type=="r2_test"&jitter$jitter==min(jitter$jitter)), ]
jitter$r2 = as.numeric(jitter$r2)
jitter = aggregate(jitter$r2, by=jitter[, c("jitter", "type")], 
                FUN = function(x) c(mean = mean(x, na.rm=T), max = max(x, na.rm=T), 
                                    min = min(x, na.rm=T), sd=sd(x, na.rm=T)))
jitter = do.call(data.frame, jitter)
jitter$type = as.character(jitter$type)
jitter$type[jitter$type == "r2_true"] = "true_test_cor"

myColors = c("#1FB24A", "#6F95CE")
names(myColors) = levels(jitter$type)
colScale = scale_colour_manual(name = "grp",values = myColors)
fillScale = scale_fill_manual(name = "grp",values = myColors)
  
#plot
jitter_plot = ggplot(jitter[jitter$jitter!=Inf & jitter$type != "r2_train",]) + 
    geom_ribbon(aes(x=jitter, ymin=x.mean-2*x.sd, ymax=x.mean+2*x.sd, fill=type), alpha=0.2) +
    geom_line(aes(jitter, x.mean, color=type, group=type)) + 
    stat_smooth(method="lm", se=F, formula=y ~ poly(x, 3), colour="#1FB24A", linetype=2,
                aes(jitter, x.mean), data=jitter[jitter$type=="r2_test", ], size=0.4, 
                fullrange=T) + 
    ylim(0.55, 0.9) + xlim(0, 12) + theme_anne("Times", size=18) +
    xlab("average km of jitter") + ylab("r^2") + 
    colScale + fillScale

ggsave(paste0(git_path, "/figures/raw/Figure_4a.pdf"), jitter_plot, 
         "pdf", width=7.5, height=4, dpi=300)

#################################################################
#sample figure
#################################################################

myColors = c("#1FB24A", "#6F95CE")
names(myColors) = levels(sample$type)
colScale = scale_colour_manual(name = "grp",values = myColors)
fillScale = scale_fill_manual(name = "grp",values = myColors)

sample = sample[sample$type %in% sample("test_cor", "true_test_cor"),]

sample_plot = ggplot(sample) + 
  geom_ribbon(aes(x=n, ymin=x.mean-2*x.sd, ymax=x.mean+2*x.sd, fill=type), alpha=0.2) +
  geom_line(aes(n, x.mean, color=type, group=type)) + 
  theme_anne("Times", size=18) + ylim(0.55, 0.9) + xlim(15, 1) + 
  xlab("n samples in village") + ylab("") + 
  colScale + fillScale

sample_plot

ggsave(paste0(git_path, "/figures/raw/Figure_4b.pdf"), sample_plot, "pdf", 
       width=7.5, height=4, dpi=300)



#################################################################
# noise from responses figure
#################################################################

myColors = c("#1FB24A", "#6F95CE")
names(myColors) = levels(response$type)
colScale = scale_colour_manual(name = "grp",values = myColors)
fillScale = scale_fill_manual(name = "grp",values = myColors)

response_plot = ggplot(response) + 
    geom_ribbon(aes(x=n, ymin=x.mean-2*x.sd, ymax=x.mean+2*x.sd, fill=type), alpha=0.2) +
    geom_line(aes(n, x.mean, color=type, group=type)) + 
    theme_anne("Times", size=18)  + xlab("added error = N(0, x)") + ylab("") + 
    colScale + fillScale + ylim(0.55, .9)

ggsave(paste0(git_path, "/figures/raw/Figure_4c.pdf"), response_plot, "pdf", 
       width=7.5, height=4, dpi=300)


#################################################################
# sat yield vs crop cut yield figure
#################################################################

ind = which(crop$plot_area_GPS >= .1 & (crop$purestand == 1)) #from David's code

model = lm(cc_yield ~ DOY_mean_151.6 + DOY_mean_171.6, data=crop[ind,])
crop$satpred = predict(model, crop)

g = panel(crop[ind, c("satpred", "cc_yield")], lm=F, a=0.5, size=16.5, font="Times") +
    geom_abline(intercept=0, slope=1, color="darkgrey") +
    xlab('Satellite yield (Mg/ha)') + ylab('Crop cut yield (Mg/ha)') 
    
ggsave(paste0(git_path, "/figures/raw/Figure_4d.pdf"), g, "pdf", 
       width=5, height=5, dpi=300)

#################################################################
# sat yield vs crop cut yield figure
#################################################################

g = panel(crop[ind, c("satpred", "fullplot_yield")], 
          lm=F, a=0.5, size=16.5, font="Times") +
    geom_abline(intercept=0, slope=1, color="darkgrey") +
    xlab('Satellite yield (Mg/ha)') + ylab('Full plot yield (Mg/ha)') 
    
ggsave(paste0(git_path, "/figures/raw/Figure_4e.pdf"), g, "pdf", 
       width=5, height=5, dpi=300)


#################################################################
# sat yield correlation figure
#################################################################

getcors = function(x, indices) cor(x[indices, ], use = 'p')[, 1]
ps_cors = list()
yieldvars = c('cc_yield', 'satpred')
xvars = c("used_inorganic", 'wave1_SQI')

for (i in yieldvars) {
    use = which(is.finite(apply(crop[ind, c(i, xvars)], 1, sum)))
    temp = boot(data = crop[ind[use], c(i, xvars)],
                statistic = getcors,
                R = 1000)
    ps_cors[[i]] = t(apply(temp$t, 2, function(x)
        c(mean(x, na.rm = T), sd(x, na.rm = T), quantile(x, c(0.05, 0.95), na.rm = T)))[, -1])
}

pdf(paste0(git_path, "/figures/raw/Figure_4f.pdf"))
par(mfrow=c(1,1), lwd=3, mar=c(5,4.5,1,1), cex.lab=1.5, cex.axis=1.5, 
    cex=1.5, family="Times")
mids = t(sapply(ps_cors,function(x) x[,1]))
lows = t(sapply(ps_cors,function(x) x[,3]))
his = t(sapply(ps_cors,function(x) x[,4]))
var.names=c('Used \nFertilizer','Soil Quality \nIndex')
a=barplot(mids,beside=T,plot=F)
nmod=nrow(mids)
cols=c(brewer.pal(3,'Set1'))[1:length(var.names)]
ylim=c(0,1)
ylab='Correlation'
plot(a, mids, col=cols, ylim=ylim, ylab=ylab, xlab='', axes=F, xlim=c(1,6), pch=19)
axis(2,las=1)
axis(1,at=(a[1,]+a[2,])/2,labels = var.names,cex.axis=1.0,tcl=0)
abline(h=0,col=gray(.7), lwd=.8)
for (i in 1:length(a)) arrows(a[i],lows[i],a[i],his[i],length=.1,angle=90,code=3,col=rep(cols,10)[i])
legend('topleft',
       leg=c('Crop cut yields','Satellite yields'),
       text.col=cols,bty='n', lwd=2, col=cols,
       title='Correlation with inputs', title.col=1, title.adj=1.5)
dev.off()