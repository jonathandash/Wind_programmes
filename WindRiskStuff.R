source("D:\\WindRisk\\ForestGALES.r")
library(plyr)
library(sp)
library(rgdal)
library(raster)
library(car)
library(MASS)
library(extRemes)
library(raster)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


setwd('D:\\WindRisk')


#Bring in the data from csvs for ease
cldMet<-read.csv("Cloudmetrics.csv")
PltDat<-read.csv("PlotData.csv")
colnames(cldMet)[colnames(cldMet)=="Identifier"] <- "ID" #Make the id column names match for merge
df<-merge(PltDat, cldMet, by="ID") #Merge the modelling datagrame

TrDat<-read.csv("LIR14Tree.csv") #This is for the mean dbh response



##Plots to check the data looks reasonable
plot(TopHeight ~ Age, data=df)
plot(TopHeight ~ Elev.P99, data=df)
plot(Stocking ~ Age, data=df)


df<-subset(df, df$ID!=212)#Remove outlier

########################################################################################
#Model Top Height with P99
########################################################################################


my.th.reg<-lm(TopHeight ~ Elev.P95, data=df)
plot(TopHeight ~ Elev.P95, data=df, xlab="P95")
abline(my.th.reg)
summary(my.th.reg)

########################################################################################
#Model Stand density
########################################################################################
#Fit a stepwise regression to choose predictors for stocking
sdf<- subset(df, select=-c(ID, FileTitle, DataFile, TotalStemVolume, TopHeight, BasalArea, SiteIndex, I300))
cors<-cor(sdf)
write.csv(cors, file="cors.csv")


#Only include the most correlated predictors in the modelling data frame for stocking
sdf<- subset(df, select=c(Stocking, Elev.maximum,
Elev.P99,
Elev.AAD,
Elev.stddev,
Elev.strata..20.00.to.30.00..mean,
Age,
Elev.L2,
Elev.strata..10.00.to.20.00..mean,
Elev.strata..0.00.to.2.50..return.proportion,
Int.P75,
Int.P70,
Int.P80,
Int.mean,
Int.L1,
Int.P90,
Elev.L.skewness))

#Stepwise regresssion for stand density
my.stepsph<-step(lm(Stocking~., data=sdf), direction="backward") # This will do for now but should be reviewed
summary(my.stepsph)

#Plot up the variables if required
plot(Stocking ~ Elev.strata..10.00.to.20.00..mean, data=sdf)
lw1<-loess(Stocking ~ Elev.strata..10.00.to.20.00..mean, data=sdf)
j <- order(sdf$Elev.strata..10.00.to.20.00..mean)
lines(sdf$Elev.strata..10.00.to.20.00..mean[j],lw1$fitted[j],col="grey",lwd=1)

plot(Stocking ~ Elev.strata..0.00.to.2.50..return.proportion, data=sdf)
lw1<-loess(Stocking ~ Elev.strata..0.00.to.2.50..return.proportion, data=sdf)
j <- order(sdf$Elev.strata..0.00.to.2.50..return.proportion)
lines(sdf$Elev.strata..0.00.to.2.50..return.proportion[j],lw1$fitted[j],col="grey",lwd=1)

###Residual diagnostics
lev = hat(model.matrix(my.stepsph)) 
plot(lev) 
par(mfrow=c(2,2))
qqPlot(my.stepsph, main="QQ Plot")
plot(my.stepsph$fitted, my.stepsph$res) 
hist(my.stepsph$res)
spreadLevelPlot(my.stepsph)
dev.off()

########################################################################################
#Model Mean DBH
########################################################################################
ddf<- ddply(TrDat, .(StratumName), summarize, meanDBH = mean(DBH))
colnames(ddf)[colnames(ddf)=="StratumName"] <- "ID" #Make the id column names match for merge
ddf<-merge(ddf, cldMet, by="ID")

ddf<- subset(ddf, select=-c(ID, FileTitle, DataFile))
corsd<-cor(ddf)
write.csv(corsd, file="cosd.csv")

ddf$Elev.P50_2<-ddf$Elev.P50^2
ddf$Elev.P50_05<-ddf$Elev.P50^0.5
ddf$meanDBH_2<-ddf$meanDBH^2

plot(meanDBH ~ Elev.P50, data=ddf)
plot(meanDBH ~ Elev.P50_2, data=ddf)
plot(meanDBH_2 ~ Elev.P50, data=ddf)
plot(meanDBH ~ Elev.P50_05, data=ddf)
my.regdbh<-lm(meanDBH ~ Elev.P50_05, data=ddf)
abline(my.regdbh)
summary(my.regdbh)

###Residual diagnostics
lev = hat(model.matrix(my.regdbh)) 
plot(lev) 
par(mfrow=c(2,2))
qqPlot(my.regdbh, main="QQ Plot")
plot(my.regdbh$fitted, my.regdbh$res) 
hist(my.regdbh$res)
spreadLevelPlot(my.regdbh)
dev.off()



########################################################################################
#Model Mean Tree Size
########################################################################################
df$MTV<-df$TotalStemVolume/df$Stocking

vdf<- subset(df, select=-c(ID, FileTitle, DataFile, TotalStemVolume, TopHeight, BasalArea, SiteIndex, I300, Stocking))

corv<-cor(vdf)
write.csv(corv, file="corv.csv")

vdf$Elev.variance_05<-vdf$Elev.variance^0.5
vdf$MTV_05<-vdf$MTV^0.5
plot(MTV ~ Elev.variance, data=vdf)
plot(MTV_05 ~ Elev.variance, data=vdf)
plot(MTV ~ Elev.variance_05, data=vdf)
my.regV<-lm(MTV ~ Elev.variance, data=vdf)
abline(my.regV)
summary(my.regV)

###Residual diagnostics
lev = hat(model.matrix(my.regV)) 
plot(lev) 
par(mfrow=c(2,2))
qqPlot(my.regV, main="QQ Plot")
plot(my.regV$fitted, my.regV$res) 
hist(my.regV$res)
spreadLevelPlot(my.regV)
dev.off()

########################################################################################
#Bring in the P95 raster to model top height  
#Using the raster package for this because it seems to work well
########################################################################################
p95rast<-raster("D:\\WindRisk\\Products2\\Metrics_25METERS\\elev_P95_0p5plus_25METERS.asc")  
plot(p95rast)
names(p95rast)<-'Elev.P95' #Rename raster so that it can be used with predict

mth.rast<-predict(p95rast, my.th.reg) #Predict MTH
plot(mth.rast) #Plot it for a sanity check
writeRaster(mth.rast, filename="MTH.tiff") #Write the raster layer to file and send to Juan for use in WaSP


rough.rast<-mth.rast *0.05  #Calcualte roughness raster
plot(rough.rast)            #Plot it for a sanity check
writeRaster(rough.rast, filename="RoughnessRaster.asc") #Write the raster layer to file and send to Juan for use in WaSP




########################################################################################
#Bring in the predictor rasters to model sph  
#Using the raster package for this because it seems to work well
########################################################################################

Elv.10_20.mean.rast<-raster("D:\\WindRisk\\Products2\\Metrics_25METERS\\strata4_mean_25METERS.asc")
Elv.0_2.prop.rast<-raster("D:\\WindRisk\\Products2\\Metrics_25METERS\\strata1_return_proportion_25METERS.asc")
Int.P70.rast<-raster("D:\\WindRisk\\Products2\\Metrics_25METERS\\int_P70_0p5plus_25METERS.asc")
Int.P90.rast<-raster("D:\\WindRisk\\Products2\\Metrics_25METERS\\int_P90_0p5plus_25METERS.asc")
Elv.skew.rast<-raster("D:\\WindRisk\\Products2\\Metrics_25METERS\\elev_skewness_0p5plus_25METERS.asc")

#Now put all rasters into a stack
sph.pred<-stack(Elv.10_20.mean.rast, Elv.0_2.prop.rast, Int.P70.rast, Int.P90.rast, Elv.skew.rast)

#Rename raster bands to matcht the names stored by lm
names(sph.pred)<-c('Elev.strata..10.00.to.20.00..mean', 'Elev.strata..0.00.to.2.50..return.proportion',
                   'Int.P70', 'Int.P90', 'Elev.L.skewness')

sph.rast<-predict(sph.pred, my.stepsph) #Predict SPH using the stepwise model
NAvalue(mth.rast)
plot(sph.rast)




########################################################################################
#Bring in the predictor raster to model mean dbh 
#Using the raster package for this because it seems to work well
########################################################################################
p50rast<-raster("D:\\WindRisk\\Products2\\Metrics_25METERS\\elev_P50_0p5plus_25METERS.asc")
plot(p50rast)
p50rast<-p50rast^0.5

names(p50rast)<-'Elev.P50_05' #Rename raster so that it can be used with predict
mdbh.rast<-predict(p50rast, my.regdbh) #Predict Mean dbh
plot(mdbh.rast)

########################################################################################
#Bring in the predictor raster to model MTV
#Using the raster package for this because it seems to work well
########################################################################################
Elv.var.rast<-raster("D:\\WindRisk\\Products2\\Metrics_25METERS\\elev_variance_0p5plus_25METERS.asc")
plot(Elv.var.rast)
names(Elv.var.rast)<-'Elev.variance' #Rename raster so that it can be used with predict

mtv.rast<-predict(Elv.var.rast, my.regV) #Predict MTH
plot(mtv.rast)

###################################################################################
#Bring all predictors together into a stack to use for predicting wind risk probability
###################################################################################
gales.pred.rast<-stack(mth.rast, sph.rast, mdbh.rast, mtv.rast)

###################################################################################
#Calcualte the wind speed parameters
###################################################################################
#Based on the anemometer data from cpt 1008 
winds<-read.csv('Cpt1008WindSpeeds.csv')

par(bg="white", las=1, cex=1.1)
plot(density(winds$WS_ms_Avg, bw=0.5, cut=0), las=1, lwd=2, xlim=c(0,5),col="steelblue")
weibull.parms<-fitdistr(winds$WS_ms_Avg, densfun="weibull")
qqPlot(winds$WS_ms_Avg, distribution="weibull", shape=1.30727865, scale=1.97676851)

gumb.dist<-fevd(winds$WS_ms_Av, type="Gumbel")
plot(gumb.dist)


####################################################################################
#Implement ForestGALES
####################################################################################

test.data<-data.frame(UID=c(2,3), hght=c(30.5, 50), dbh=c(42,50), sph=c(400, 350), age=c(26, 30), vol=c(2.45, 3.6), soil.type=c('B', 'BF'), mu=c(1.241, 1.241), sigma=c(1.029507 ,1.029507))


pixel.list<-unique(test.data$UID)
  
for (i in 1:length(pixel.list)) {
x<-subset(test.data, test.data$UID == i)    
ForestGALES(hght = x$hght,
            sph = x$sph,
            dbh = x$dbh,
            age = x$age,
            vol = x$vol,
            soil.type = x$soil.type,
            mu = x$mu,
            sigma = x$sigma)
}






ForestGALES(hght = test.data$hght,
            sph = test.data$sph,
            dbh = test.data$dbh,
            age = test.data$age,
            vol = test.data$vol,
            soil.type = test.data$soil.type,
            mu = test.data$mu,
            sigma = test.data$sigma)