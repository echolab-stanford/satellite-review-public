source("00_init.R")
setwd(data_path)

########################################################################################
# Written by: Marshall Burke, Nov 2020
# Last edited by: Anne, Nov 2020
# Creates a figure showing performance in literature for panel C of summary figure
########################################################################################


# parameters for width of mean line and horizontal jitter to points, and color of points
off=0.3
sd=0.05
cex=1  #size of points
cll = apply(sapply("black", col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=0.5)) 

pdf(paste0(git_path, '/figures/raw/Figure_SummaryC.pdf'),width=4,height=4,useDingbats = F)
par(mfrow=c(1,2),mar=c(0,4,0,0))

plot(1,type="n",xlim=c(0.5,2.5),ylim=c(0,1),axes=F,xlab="",ylab="r2")
axis(2,las=1)

# smallholder ag
dt = read_csv('SmallholderYieldEstimates.csv')
dt = dt %>% rename(resolution=`sensor resolution (meter)`, num_images=`number of images`, 
            obs=`number of obs`, year=`study year`, type=`training data`) %>%
    filter(type=="crop cuts")
att = 1
n = dim(dt)[1]
mn = mean(dt$r2)
points(rep(att,n)+rnorm(n,0,sd), dt$r2, pch=19, col=alpha("orange", 0.5), cex=cex)
segments(att-off, mn, att+off, mn, col="blue", lwd=2)


# asset wealth
dt = read.xlsx(paste0(git_path, '/work/written/pov_slum_lit.xlsx'), sheet=1)
dt$r2 = dt$Best.Performance
dt$r2[dt$Metric=="cor"] = dt$r2[dt$Metric=="cor"]^2
dt$r2[dt$Metric%in%c("r2","cor")==F] = NA
dt = dt %>% 
    filter(is.na(r2)==F & Prediction.Task=="Asset Wealth Index")
att = 2
n = dim(dt)[1]
mn = mean(dt$r2)
points(rep(att,n)+rnorm(n,0,sd), dt$r2, pch=19, col=alpha("deepskyblue2", 0.5), cex=cex)
segments(att-off, mn, att+off, mn, col="blue", lwd=2)


# slums - doing an additional axis since measure is different
dt = read.xlsx(paste0(git_path, '/work/written/pov_slum_lit.xlsx'), sheet=2)
dt = filter(dt,Metric=="accuracy")
att=1
plot(1, type="n", xlim=c(0.5,1.5), ylim=c(0,1), axes=F, xlab="", ylab="accuracy")
axis(2, las=1)
n = dim(dt)[1]
mn = mean(dt$Best.Performance)
points(rep(att,n)+rnorm(n,0,sd), dt$Best.Performance, pch=19, 
       col=alpha("black", 0.5), cex=cex)
segments(att-off,mn,att+off,mn,col="blue",lwd=2)

dev.off()
