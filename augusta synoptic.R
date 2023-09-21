# Subcatchment Leverage, Variance Collapse, and Stability Examples
# Code written by AJS, adapted by CRW
# 06-20-2021 - 7/21/2021

# Remove all variables
rm(list=ls(all=T))

# ---- Load Required Packages ---- 
### LOAD REQUIRED PACKAGES
library(dplyr)
require(reshape2)
require(ggplot2)
require(RColorBrewer)
require(gridExtra)
require(sciplot)
#install.packages("changepoint")
require(ggthemes)
require(ggExtra)
require(grid)
require(cowplot)
library(rgdal)
library(ggplot2)
library(ggsn)
library(wesanderson)
library(sp)
library(rgdal)
library(broom)
library(scales)
library(readxl)
library(data.table)
library(strucchange)
library(ggfortify)
# ---- Load Data ----
setwd("C:/Users/Caroline/Google Drive/Research/Lab/Data Files/Synoptic Analysis")
db21 = read_excel("C:/Users/Caroline/Google Drive/Research/Lab/Data Files/Synoptic Analysis/Augusta_Synoptic_Data.xlsx", 
                  sheet = "Sheet1",  na = c("bd", "-bd","-0.0001", ""))


# Chemistry data from Augusta 2021
season.21 = db21$season
date.21 = db21$sampledate
month.21 = db21$Month
subcatchment.21 = db21$subcatchment
year.21 = db21$Year
site.21 = db21$site
area.21 = (as.numeric(db21$area))
area.21 = area.21/1000000
lat.21 = as.numeric(db21$x)
long.21 = as.numeric(db21$y)
doc.21 = as.numeric(db21$`NPOC (mg/L)`)
no3.21 = as.numeric(db21$`NO3-N (mg/L)`)
nh4.21 = as.numeric(db21$`NH4-N (mg/L)`)
don.21 = db21$`TDN (mg/L-N)` - db21$`NO3-N (mg/L)`
tdn.21 = as.numeric(db21$`TDN (mg/L-N)`)
suva.21 = as.numeric(db21$suva)
spectralrat.21 = as.numeric(db21$spectralratio)
abs254.21 = as.numeric(db21$abs254)
so4.21 = as.numeric(db21$`SO4 (mg/L)`)
ca.21 = as.numeric(db21$`Ca (mg/L)`)
cl.21 = as.numeric(db21$`Cl (mg/L)`)
k.21 = as.numeric(db21$`K (mg/L)`)
mg.21 = as.numeric(db21$`Mg (mg/L)`)
na.21 = as.numeric(db21$`Na (mg/L)`)
tn.21 = as.numeric(db21$`TN (mg/L-N)`)	
temp = as.numeric(db21$Temp)
do = as.numeric(db21$`DO%`)
spc = as.numeric(db21$SPC)
wetland = as.numeric(db21$`wetland %`)

df21 = data.frame(season.21,date.21,month.21, subcatchment.21, year.21, site.21, area.21,wetland, lat.21, long.21, doc.21, no3.21, nh4.21, don.21, tdn.21, so4.21, ca.21, cl.21, k.21, mg.21, na.21, tn.21,temp,do,spc)
names(df21) = c("season", "date", "month", "subcatchment", "year", "site", "area", "PercentWetland", "lat", "long", "doc", "no3", "nh4", "don", "tdn", "so4", "Ca", "Cl", "K", "Mg", "Na", "tn","temp","DO%","SPC")
head(df21)

# ---- Subcatchment Leverage ----
# Melt datafroma
melt21=reshape2::melt(df21, id.vars=c("subcatchment", "date", "site", "season", "area", "lat", "long"), measure.vars = c("doc", "no3", "nh4",  "don", "tdn", "so4", "Ca", "Cl", "K", "Mg", "Na", "tn"), na.rm=TRUE)
#melt21 = melt21[melt21$area < 200,] # We want data only below aufeis field

melt21$subcatchment = as.factor(melt21$subcatchment)
melt21$season = as.factor(melt21$season)
melt21$variable = as.factor(melt21$variable)
melt21$site = as.factor(melt21$site)

# Calculates subcatchment leverage
lev.21 = melt21 %>% 
  #select(subcatchment, date, variable) %>%
  group_by(date, variable) %>%
  #mutate(leverage.area = ((value - value[which.max(area)])*100/ value[which.max(area)]) * discharge/discharge[which.max(area)]) %>% # use if you have estimates of instantaneous Q at nested site, in L/km2/s
  mutate(leverage.Q = ((value - value[which.max(area)])*100/ value[which.max(area)]) * area/max(area)) # use if you don’t have specific Q values
  #mutate(leverage.Q = ((value - value[which.max(area)])*(area/max(area)))) # units of concentration

head(lev.21)

# Make data table of the subcatchment means 
DT21 = data.table(lev.21, na.rm = TRUE)
mean2021 = DT21[ , .(Meanval = mean(value), ValSD = sd(value), MeanLev = mean(leverage.Q), MeanSD = sd(leverage.Q)), by = .(site,variable,season)]
mean2021

# break out by season
fall = subset(DT21[season == "Fall"])
winter = subset(DT21[season == "Winter"])
spring = subset(DT21[season == "Spring"])
summer = subset(DT21[season == "Summer"])

# boxplot(fall$leverage.Q~fall$site,ylab="Subcatchment Leverage",xlab="Site", main="DOC - Fall")
# boxplot(winter$leverage.Q~winter$site,ylab="Subcatchment Leverage",xlab="Site", main="DOC Winter")
# boxplot(spring$leverage.Q~spring$site,ylab="Subcatchment Leverage",xlab="Site", main="DOC Spring")

# DOC
DOC = subset(DT21[variable == "doc"])
DOCfall = subset(subset(fall[variable == "doc"]))
DOCwinter = subset(subset(winter[variable == "doc"]))
DOCspring = subset(subset(spring[variable == "doc"]))
DOCsummer = subset(subset(summer[variable == "doc"]))

boxplot(mean2021$MeanLev~mean2021$variable,ylab="Subcatchment Leverage",xlab="Variable")
plot(mean2021$variable,mean2021$MeanLev,ylab="Subcatchment Leverage",xlab="Variable")
boxplot(DOC$leverage.Q~DOC$date,ylab="Subcatchment Leverage",xlab="Date",main="DOC")
boxplot(DOC$leverage.Q~DOC$area,ylab="Subcatchment Leverage",xlab="Area",main="DOC")


par(mfrow=c(1,1))
boxplot(DOCfall$leverage.Q~DOCfall$site,ylab="Subcatchment Leverage",xlab="Site", main="DOC - Fall")
boxplot(DOCwinter$leverage.Q~DOCwinter$site,ylab="Subcatchment Leverage",xlab="Site", main="DOC Winter")
boxplot(DOCspring$leverage.Q~DOCspring$site,ylab="Subcatchment Leverage",xlab="Site", main="DOC Spring")
boxplot(DOCsummer$leverage.Q~DOCsummer$site,ylab="Subcatchment Leverage",xlab="Site", main="DOC Summer")


plot(DOCfall$area,DOCfall$leverage.Q,ylab="Subcatchment Leverage",xlab="Area", main="DOC - Fall")
boxplot(DOCwinter$leverage.Q~DOCwinter$area,ylab="Subcatchment Leverage",xlab="Area", main="DOC Winter")
boxplot(DOCspring$leverage.Q~DOCspring$area,ylab="Subcatchment Leverage",xlab="Area", main="DOC Spring")
boxplot(DOCsummer$leverage.Q~DOCsummer$area,ylab="Subcatchment Leverage",xlab="Area", main="DOC Summer")


# NO3
NO3 = subset(DT21[variable == "no3"])
NO3fall = subset(subset(fall[variable == "no3"]))
NO3winter = subset(subset(winter[variable == "no3"]))
NO3spring = subset(subset(spring[variable == "no3"]))
NO3summer = subset(subset(summer[variable == "no3"]))

boxplot(mean2021$MeanLev~mean2021$variable,ylab="Subcatchment Leverage",xlab="Variable")
boxplot(NO3$leverage.Q~NO3$date,ylab="Subcatchment Leverage",xlab="Date",main="NO3-")
boxplot(NO3$leverage.Q~NO3$area,ylab="Subcatchment Leverage",xlab="Area",main="NO3-")
plot(NO3$area,NO3$leverage.Q, ylab="Subcatchment Leverage",xlab="Area",main="NO3-")


par(mfrow=c(1,1))
boxplot(NO3fall$leverage.Q~NO3fall$site,ylab="Subcatchment Leverage",xlab="Site", main="NO3 - Fall")
boxplot(NO3winter$leverage.Q~NO3winter$site,ylab="Subcatchment Leverage",xlab="Site", main="NO3 Winter")
boxplot(NO3spring$leverage.Q~NO3spring$site,ylab="Subcatchment Leverage",xlab="Site", main="NO3 Spring")
boxplot(NO3summer$leverage.Q~NO3summer$site,ylab="Subcatchment Leverage",xlab="Site", main="NO3 Summer")


boxplot(NO3fall$leverage.Q~NO3fall$area,ylab="Subcatchment Leverage",xlab="Area", main="NO3 - Fall")
boxplot(NO3winter$leverage.Q~NO3winter$area,ylab="Subcatchment Leverage",xlab="Area", main="NO3 Winter")
boxplot(NO3spring$leverage.Q~NO3spring$area,ylab="Subcatchment Leverage",xlab="Area", main="NO3 Spring")
boxplot(NO3summer$leverage.Q~NO3summer$area,ylab="Subcatchment Leverage",xlab="Area", main="NO3 Summer")


# ---- Variance Collapse ----
# Example from the Augusta Creek 2021 dataset
require(changepoint)
head(df21)
# kup21=subset(df21, subcatchment == "KUP")
 aug21F=subset(df21, season == "Fall")
 aug21W=subset(df21, season == "Winter")
 aug21Sp=subset(df21, season == "Spring")
 aug21Su=subset(df21, season == "Summer")
 
 
 aug21Oct=subset(df21, month == "October")
 aug21Nov=subset(df21, month == "November")
 aug21Dec=subset(df21, month == "December")
 aug22Jan=subset(df21, month == "January")
 aug22Feb=subset(df21, month == "February")
 aug22Mar=subset(df21, month == "March")
 aug22Apr=subset(df21, month == "April")
 aug22May=subset(df21, month == "May")
 aug22Jun=subset(df21, month == "June")
 aug22Jul=subset(df21, month == "July")
 aug22Aug=subset(df21, month == "August")


# The changepoint function cannot handle rows with NAs. Should delete/omit. 
# Here I just changed the NA values to 0 to keep the points in the analysis for simplicity.
 aug21F[is.na(aug21F)] = 0
 aug21F[aug21F <0] = 0
 
 aug21W[is.na(aug21W)] = 0
 aug21W[aug21W <0] = 0
 
 aug21Sp[is.na(aug21Sp)] = 0
 aug21Sp[aug21Sp <0] = 0
 
 aug21Su[is.na(aug21Su)] = 0
 aug21Su[aug21Su <0] = 0
 
 aug21Oct[is.na(aug21Oct)] = 0
 aug21Oct[aug21Oct <0] = 0
 aug21Nov[is.na(aug21Nov)] = 0
 aug21Nov[aug21Nov <0] = 0
 aug21Dec[is.na(aug21Dec)] = 0
 aug21Dec[aug21Dec <0] = 0
 aug22Jan[is.na(aug22Jan)] = 0
 aug22Jan[aug22Jan <0] = 0
 aug22Feb[is.na(aug22Feb)] = 0
 aug22Feb[aug22Feb <0] = 0
 aug22Mar[is.na(aug22Mar)] = 0
 aug22Mar[aug22Mar <0] = 0
 aug22Apr[is.na(aug22Apr)] = 0
 aug22Apr[aug22Apr <0] = 0
 

augbp21F = apply(aug21F[, 10:21], 2, function(x) cpts(cpt.var(x, method="PELT")))
#View(augbp21F) # - this gives you the value of the significant breakpoint

augbp21W = apply(aug21W[, 10:21], 2, function(x) cpts(cpt.var(x, method="PELT")))
#View(augbp21W) # - this gives you the value of the significant breakpoint)

augbp21Sp = apply(aug21Sp[, 10:11], 2, function(x) cpts(cpt.var(x, method="PELT")))
View(augbp21Sp) # - this gives you the value of the significant breakpoint

augbp21Su = apply(aug21Su[, 10:21], 2, function(x) cpts(cpt.var(x, method="PELT")))

augbp21Oct = apply(aug21Oct[, 10:21], 2, function(x) cpts(cpt.var(x, method="PELT")))
augbp21Nov = apply(aug21Nov[, 10:21], 2, function(x) cpts(cpt.var(x, method="PELT")))
augbp21Dec = apply(aug21Dec[, 10:21], 2, function(x) cpts(cpt.var(x, method="PELT")))
augbp22Jan = apply(aug22Jan[, 10:21], 2, function(x) cpts(cpt.var(x, method="PELT")))
augbp22Feb = apply(aug22Feb[, 10:21], 2, function(x) cpts(cpt.var(x, method="PELT")))
augbp22Mar = apply(aug22Mar[, 10:21], 2, function(x) cpts(cpt.var(x, method="PELT")))
augbp22Apr = apply(aug22Apr[, 10:21], 2, function(x) cpts(cpt.var(x, method="PELT")))

# Variance collapse try 2
# Fall
syn.fall <- aug21F %>%
  group_by(site) %>%
  mutate(meandoc = mean(doc,na.rm=TRUE)) %>% #column containing mean 
  mutate(sddoc = sd(doc, na.rm=TRUE)) %>% #col containing sd
  mutate(scaleddoc = (doc-meandoc)/sddoc) #calculate scaled early DOC

syn.fall <- syn.fall %>%
  group_by(site) %>%
  mutate(meanno3 = mean(no3,na.rm=TRUE)) %>%
  mutate(sdno3 = sd(no3, na.rm=TRUE)) %>%
  mutate(scaledno3 = (no3-meanno3)/sdno3)

syn.fall <- arrange(syn.fall, area) #order dataframe by area

#Find breaks for fall doc using package "strucchange", function "breakpoints. This code is saying: for the regression calculated between subcatchment area and scaled doc, find where the slope of the line significantly changes. 
fall.breaks.doc <- as.numeric(strucchange::breakpoints(syn.fall$area~syn.fall$scaleddoc, breaks=1)[1]) #calcualte breakpoint and save value. 
fall.breaks.doc <- as.numeric(syn.fall[fall.breaks.doc,7]) #pulls corresponding cubcatchment area.

fall.breaks.no3 <- as.numeric(strucchange::breakpoints(syn.fall$area~syn.fall$scaledno3, breaks=1)[1]) #calcualte breakpoint and save value. 
fall.breaks.no3 <- as.numeric(syn.fall[fall.breaks.no3,7]) #pulls corresponding cubcatchment area.

# Winter
syn.winter <- aug21W %>%
  group_by(site) %>%
  mutate(meandoc = mean(doc,na.rm=TRUE)) %>% #column containing mean 
  mutate(sddoc = sd(doc, na.rm=TRUE)) %>% #col containing sd
  mutate(scaleddoc = (doc-meandoc)/sddoc) #calculate scaled early DOC

syn.winter <- syn.winter %>%
  group_by(site) %>%
  mutate(meanno3 = mean(no3,na.rm=TRUE)) %>%
  mutate(sdno3 = sd(no3, na.rm=TRUE)) %>%
  mutate(scaledno3 = (no3-meanno3)/sdno3)

syn.winter <- arrange(syn.winter, area) #order dataframe by area

#Find breaks for winter  using package "strucchange", function "breakpoints. This code is saying: for the regression calculated between subcatchment area and scaled doc, find where the slope of the line significantly changes. 
winter.breaks.doc <- as.numeric(strucchange::breakpoints(syn.winter$area~syn.winter$scaleddoc, breaks=1)[1]) #calcualte breakpoint and save value. 
winter.breaks.doc <- as.numeric(syn.winter[winter.breaks.doc,7]) #pulls corresponding cubcatchment area.

winter.breaks.no3 <- as.numeric(strucchange::breakpoints(syn.winter$area~syn.winter$scaledno3, breaks=1)[1]) #calcualte breakpoint and save value. 
winter.breaks.no3 <- as.numeric(syn.winter[winter.breaks.no3,7]) #pulls corresponding cubcatchment area.

# spring
syn.spring <- aug21Sp %>%
  group_by(site) %>%
  mutate(meandoc = mean(doc,na.rm=TRUE)) %>% #column containing mean 
  mutate(sddoc = sd(doc, na.rm=TRUE)) %>% #col containing sd
  mutate(scaleddoc = (doc-meandoc)/sddoc) #calculate scaled early DOC

syn.spring <- syn.spring %>%
  group_by(site) %>%
  mutate(meanno3 = mean(no3,na.rm=TRUE)) %>%
  mutate(sdno3 = sd(no3, na.rm=TRUE)) %>%
  mutate(scaledno3 = (no3-meanno3)/sdno3)

syn.spring <- arrange(syn.spring, area) #order dataframe by area

#Find breaks for spring doc using package "strucchange", function "breakpoints. This code is saying: for the regression calculated between subcatchment area and scaled doc, find where the slope of the line significantly changes. 
spring.breaks.doc <- as.numeric(strucchange::breakpoints(syn.spring$area~syn.spring$scaleddoc, breaks=1)[1]) #calcualte breakpoint and save value. 
spring.breaks.doc <- as.numeric(syn.spring[spring.breaks.doc,7]) #pulls corresponding cubcatchment area.

spring.breaks.no3 <- as.numeric(strucchange::breakpoints(syn.spring$area~syn.spring$scaledno3, breaks=1)[1]) #calcualte breakpoint and save value. 
spring.breaks.no3 <- as.numeric(syn.spring[spring.breaks.no3,7]) #pulls corresponding cubcatchment area.

# summer
syn.summer <- aug21Su %>%
  group_by(site) %>%
  mutate(meandoc = mean(doc,na.rm=TRUE)) %>% #column containing mean 
  mutate(sddoc = sd(doc, na.rm=TRUE)) %>% #col containing sd
  mutate(scaleddoc = (doc-meandoc)/sddoc) #calculate scaled early DOC

syn.summer <- syn.summer %>%
  group_by(site) %>%
  mutate(meanno3 = mean(no3,na.rm=TRUE)) %>%
  mutate(sdno3 = sd(no3, na.rm=TRUE)) %>%
  mutate(scaledno3 = (no3-meanno3)/sdno3)

syn.summer <- arrange(syn.summer, area) #order dataframe by area

#Find breaks for summer doc using package "strucchange", function "breakpoints. This code is saying: for the regression calculated between subcatchment area and scaled doc, find where the slope of the line significantly changes. 
summer.breaks.doc <- as.numeric(strucchange::breakpoints(syn.summer$area~syn.summer$scaleddoc, breaks=1)[1]) #calcualte breakpoint and save value. 
summer.breaks.doc <- as.numeric(syn.summer[summer.breaks.doc,7]) #pulls corresponding cubcatchment area.

summer.breaks.no3 <- as.numeric(strucchange::breakpoints(syn.summer$area~syn.summer$scaledno3, breaks=1)[1]) #calcualte breakpoint and save value. 
summer.breaks.no3 <- as.numeric(syn.summer[summer.breaks.no3,7]) #pulls corresponding cubcatchment area.

syn.all <- rbind(syn.fall,syn.winter,syn.spring,syn.summer)
syn.all$date <- as.factor(syn.all$date)
syn.winter$date <- as.factor(syn.winter$date)
syn.fall$date <- as.factor(syn.fall$date)
#basic structure for how we (RIOS) plot variance collapse:
ggplot(dat=syn.fall, aes(x=area,y=scaleddoc, fill=date)) + geom_point(shape=23, size=3) +
  xlim(0,80) + 
  geom_hline(yintercept=0, size=1.25) + 
  geom_vline(xintercept = fall.breaks.doc, color = "cornflowerblue", size=4, alpha=0.3) + #gives semi-opaque bar at location of breakpoint  
  theme_bw() + 
  #Amelia's settings:
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) + scale_alpha_manual(guide='none') +
  theme(legend.position="none") + #legend.text = element_text(size = 16),
  #legend.title=element_text(size=18, face="bold")) +
  #legend.box="horizontal",legend.box.background = element_rect(),
  #legend.box.margin = margin(4, 4, 4, 4))+
  theme(axis.title.x=element_text(size=20,color="black",face="bold"))+
  theme(axis.title.y=element_text(size=20,color="black"))+ 
  theme(axis.text.y=element_text(size=18,color="black"))+ 
  theme(axis.text.x=element_text(size=18,color="black"))+
  theme(plot.title = element_text(size = 18, face = "bold")) +
  guides(fill=guide_legend(title="Season")) +
  ggtitle("Fall DOC") + 
  ylab("DOC (mg/L)") + 
  xlab(expression("Subcatchment Area "  (km^2))) 

ggplot(dat=syn.all, aes(x=area,y=scaledno3,fill=month))  +
   geom_point(shape=23,size=3) +
  xlim(0,80) + 
  geom_hline(yintercept=0, size=1.25) + 
  geom_vline(xintercept = fall.breaks.no3, color = "cornflowerblue", size=4, alpha=0.3) + #gives semi-opaque bar at location of breakpoint  
  theme_bw() + 
  #Amelia's settings:
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) + scale_alpha_manual(guide='none') +
  theme(legend.position="none") + #legend.text = element_text(size = 16),
  #legend.title=element_text(size=18, face="bold")) +
  #legend.box="horizontal",legend.box.background = element_rect(),
  #legend.box.margin = margin(4, 4, 4, 4))+
  theme(axis.title.x=element_text(size=20,color="black",face="bold"))+
  theme(axis.title.y=element_text(size=20,color="black"))+ 
  theme(axis.text.y=element_text(size=18,color="black"))+ 
  theme(axis.text.x=element_text(size=18,color="black"))+
  theme(plot.title = element_text(size = 18, face = "bold")) +
  guides(fill=guide_legend(title="Season")) +
  ggtitle("Winter NO3-") + 
  ylab("NO3- (mg/L)") + 
  xlab(expression("Subcatchment Area "  (km^2))) 

# Variance collapse try 3
out.lm<-lm(doc~area,data=syn.fall)
o <- segmented(out.lm,seg.Z=~area)
summary(o)
plot(o)
points(syn.fall$area,syn.fall$doc)

out.lm<-lm(doc~area,data=syn.winter)
o <- segmented(out.lm,seg.Z=~area)
summary(o)
plot(o)
points(syn.winter$area,syn.winter$doc)

out.lm<-lm(doc~area,data=syn.spring)
o <- segmented(out.lm,seg.Z=~area)
summary(o)
plot(o)
points(syn.spring$area,syn.spring$doc)

out.lm<-lm(doc~area,data=syn.summer)
o <- segmented(out.lm,seg.Z=~area)
summary(o)
plot(o)
points(syn.summer$area,syn.summer$doc)

out.lm<-lm(no3~area,data=syn.fall)
o <- segmented(out.lm,seg.Z=~area)
summary(o)
plot(o)
points(syn.fall$area,syn.fall$no3)



model = list(doc~0+area)  # three intercept-only segments

fit_mcp = mcp(model,data=syn.fall)

summary(fit_mcp)
# ---- Spatial stability ----

melt21.aug = melt21[melt21$subcatchment == "AUG",]
melt21.augF = melt21.aug[melt21.aug$season == "Fall",]
melt21.augW = melt21.aug[melt21.aug$season == "Winter",]
melt21.augSp = melt21.aug[melt21.aug$season == "Spring",]
melt21.augSu = melt21.aug[melt21.aug$season == "Summer",]

# doc
# Unmelt Fall Season
um21.augF = reshape2::dcast(melt21.augF, site ~ variable, sum)
um21.augF.DOC = data.frame(um21.augF$site, um21.augF$doc)
names(um21.augF.DOC) = c("site", "doc.fall")

# Unmelt Winter Season
um21.augW = reshape2::dcast(melt21.augW, site ~ variable, sum)
um21.augW.DOC = data.frame(um21.augW$site, um21.augW$doc)
names(um21.augW.DOC) = c("site", "doc.winter")

# Unmelt Spring Season
um21.augSp = reshape2::dcast(melt21.augSp, site ~ variable, sum)
um21.augSp.DOC = data.frame(um21.augSp$site, um21.augSp$doc)
names(um21.augSp.DOC) = c("site", "doc.spring")

# Unmelt Summer Season
um21.augSu = reshape2::dcast(melt21.augSu, site ~ variable, sum)
um21.augSu.DOC = data.frame(um21.augSu$site, um21.augSu$doc)
names(um21.augSu.DOC) = c("site", "doc.summer")

# Line up data by Site ID
doc.aug.21.df = merge(um21.augF.DOC,um21.augW.DOC, by = "site")
str(doc.aug.21.df)
doc.aug.21.df2 = merge(um21.augW.DOC,um21.augSp.DOC, by = "site")
str(doc.aug.21.df)
doc.aug.21.df3 = merge(um21.augSp.DOC,um21.augSu.DOC, by = "site")
str(doc.aug.21.df)

# Correlation Test between Fall and winter Season sampling events
rho.doc.aug = cor.test(doc.aug.21.df$doc.fall, doc.aug.21.df$doc.winter, method = "spearman") # 0.19, p = 0.25
rho.doc.aug
rho.doc.aug2 = cor.test(doc.aug.21.df2$doc.winter, doc.aug.21.df2$doc.spring, method = "spearman") # 0.19, p = 0.25
rho.doc.aug2
rho.doc.aug3 = cor.test(doc.aug.21.df3$doc.spring, doc.aug.21.df3$doc.summer, method = "spearman") # 0.19, p = 0.25
rho.doc.aug3
# no3
# Unmelt Fall Season
um21.augF = reshape2::dcast(melt21.augF, site ~ variable, sum)
um21.augF.no3 = data.frame(um21.augF$site, um21.augF$no3)
names(um21.augF.no3) = c("site", "no3.fall")

# Unmelt Winter Season
um21.augW = reshape2::dcast(melt21.augW, site ~ variable, sum)
um21.augW.no3 = data.frame(um21.augW$site, um21.augW$no3)
names(um21.augW.no3) = c("site", "no3.winter")

# Unmelt Spring Season
um21.augSp = reshape2::dcast(melt21.augSp, site ~ variable, sum)
um21.augSp.no3 = data.frame(um21.augSp$site, um21.augSp$no3)
names(um21.augSp.no3) = c("site", "no3.spring")

# Unmelt Summer Season
um21.augSu = reshape2::dcast(melt21.augSu, site ~ variable, sum)
um21.augSu.no3 = data.frame(um21.augSu$site, um21.augSu$no3)
names(um21.augSu.no3) = c("site", "no3.summer")

# Line up data by Site ID
no3.aug.21.df = merge(um21.augSp.no3, um21.augSu.no3, by = "site")
str(no3.aug.21.df)

# Correlation Test between Fall and winter Season sampling events
rho.no3.aug = cor.test(no3.aug.21.df$no3.spring, no3.aug.21.df$no3.summer, method = "spearman") # 0.19, p = 0.25
rho.no3.aug

# Wetland percent
wetland <- read_excel("Catchmentwetlandpercent.xlsx")

wetland$Subcatchment <- as.factor(wetland$Subcatchment)
wetland$`% wetland in catchment` <- as.numeric(wetland$`% wetland in catchment`)

plot(wetland$Subcatchment, wetland$`% wetland in catchment`,xlab="Site",ylab="Wetland %")

# Leverage vs wetland percent colorized by season
DOCwetland <- read_excel("LeverageWetlandPercent.xlsx", sheet = "DOC")
#DOCwetland <- DOCwetland[order(DOCwetland$Season,DOCwetland$WetlandPercent),]
NO3wetland <- read_excel("LeverageWetlandPercent.xlsx", sheet = "NO3")
NO3wetland$MEAN_leverage_Q <- as.numeric(NO3wetland$MEAN_leverage_Q)
#NO3wetland <- NO3wetland[order(NO3wetland$Season,NO3wetland$WetlandPercent),]


# DOCwetland$Colour="black"
# DOCwetland$Colour[DOCwetland$Season=="Winter"]="red"
# DOCwetland$Colour[DOCwetland$Season=="Spring"]="blue"
# DOCwetland$Colour[DOCwetland$Season=="Summer"]="green"
# NO3wetland$Colour="black"
# NO3wetland$Colour[NO3wetland$Season=="Winter"]="red"
# NO3wetland$Colour[NO3wetland$Season=="Spring"]="blue"
# NO3wetland$Colour[NO3wetland$Season=="Summer"]="green"

# #plot(DOCwetland$WetlandPercent,DOCwetland$MEAN_leverage_Q,ylab="Subcatchment Leverage (%)",xlab="Wetland Coverage (%)", main="DOC",col=DOCwetland$Colour,pch=16)
# #lines(DOCwetland$WetlandPercent,DOCwetland$MEAN_leverage_Q,col=DOCwetland$Colour)
# #legend("bottomright", legend=c("Fall", "Winter","Spring","Summer"),
#        col=c("black","red", "blue","green"), pch=16, cex=0.8)
levVwetDOC<-ggplot(DOCwetland,
       aes(x = WetlandPercent,
           y = MEAN_leverage_Q,
           col = Season, size = area)) +
  geom_point()+  
  geom_hline(yintercept=0, size=0.5) +
  ggtitle("DOC")+
  labs(y= "Subcatchment Leverage (%)", x = "Wetland Coverage (%)")+
  theme(rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())
levVwetDOC + scale_color_manual(values=c("#FF6633", "#993399","#66CC99","#3399CC"))


levVareaDOC<-ggplot(DOCwetland,
                   aes(x = area,
                       y = MEAN_leverage_Q,
                       col = Season)) +
  geom_point(size=3)+  
  geom_hline(yintercept=0, size=0.5) +
  ggtitle("DOC")+
  labs(y= "Subcatchment Leverage (%)", x = "Area")+
  theme(rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())
levVareaDOC + scale_color_manual(values=c("#FF6633", "#993399","#66CC99","#3399CC"))

levVagDOC<-ggplot(DOCwetland,
                    aes(x = AgPercent,
                        y = MEAN_leverage_Q,
                        col = Season,size = area)) +
  geom_point()+  
  geom_hline(yintercept=0, size=0.5) +
  ggtitle("DOC")+
  labs(y= "Subcatchment Leverage (%)", x = "Ag %")+
  theme(rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())
levVagDOC + scale_color_manual(values=c("#FF6633", "#993399","#66CC99","#3399CC"))

levVforDOC<-ggplot(DOCwetland,
                  aes(x = ForestPercent,
                      y = MEAN_leverage_Q,
                      col = Season, size =area)) +
  geom_point()+  
  geom_hline(yintercept=0, size=0.5) +
  ggtitle("DOC")+
  labs(y= "Subcatchment Leverage (%)", x = "Forest %")+
  theme(rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())
levVforDOC + scale_color_manual(values=c("#FF6633", "#993399","#66CC99","#3399CC"))

# 
# plot(NO3wetland$WetlandPercent,NO3wetland$MEAN_leverage_Q,ylab="Subcatchment Leverage (%)",xlab="Wetland Coverage (%)", main="NO3",col=NO3wetland$Colour,pch=16)
# #lines(NO3wetland$WetlandPercent,NO3wetland$MEAN_leverage_Q,col=NO3wetland$Colour)
# legend("bottomright", legend=c("Fall", "Winter","Spring","Summer"),
#        col=c("black","red", "blue","green"), pch=16, cex=0.8)

levVwetNO3 <- ggplot(NO3wetland,
       aes(x = WetlandPercent,
           y = MEAN_leverage_Q,
           col = Season,size = area)) +
  geom_point()+
  geom_hline(yintercept=0, size=0.5) +
  ggtitle(bquote(NO[3]-N))+
  labs(y= "Subcatchment Leverage (%)", x = "Wetland Coverage (%)")+
  theme(rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())
levVwetNO3 + scale_color_manual(values=c("#FF6633", "#993399","#66CC99","#3399CC"))

levVareaNO3 <- ggplot(NO3wetland,
                     aes(x = area,
                         y = MEAN_leverage_Q,
                         col = Season)) +
  geom_point(size=3)+
  geom_hline(yintercept=0, size=0.5) +
  ggtitle(bquote(NO[3]-N))+
  labs(y= "Subcatchment Leverage (%)", x = "Area")+
  theme(rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())
levVareaNO3 + scale_color_manual(values=c("#FF6633", "#993399","#66CC99","#3399CC"))

levVagNO3 <- ggplot(NO3wetland,
                      aes(x = AgPercent,
                          y = MEAN_leverage_Q,
                          col = Season, size = area)) +
  geom_point()+
  geom_hline(yintercept=0, size=0.5) +
  ggtitle(bquote(NO[3]-N))+
  labs(y= "Subcatchment Leverage (%)", x = "Ag %")+
  theme(rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())
levVagNO3 + scale_color_manual(values=c("#FF6633", "#993399","#66CC99","#3399CC"))

levVforNO3 <- ggplot(NO3wetland,
                      aes(x = ForestPercent,
                          y = MEAN_leverage_Q,
                          col = Season, size = area)) +
  geom_point()+
  geom_hline(yintercept=0, size=0.5) +
  ggtitle(bquote(NO[3]-N))+
  labs(y= "Subcatchment Leverage (%)", x = "Forest %")+
  theme(rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())
levVforNO3 + scale_color_manual(values=c("#FF6633", "#993399","#66CC99","#3399CC"))


ggplot(NO3wetland,
       aes(x = WetlandPercent,
           y = MEAN_value,
           col = Season,size = area)) +
  geom_point()+
  ggtitle("NO3")+
  labs(y= "Concentration (mg/L)", x = "Wetland Coverage (%)")+
  theme(rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())

ggplot(DOCwetland,
       aes(x = WetlandPercent,
           y = MEAN_value,
           col = Season,size = area)) +
  geom_point()+
  ggtitle("DOC")+
  labs(y= "Concentration (mg/L)", x = "Wetland Coverage (%)")+
  theme(rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())

# Principal component analysis
# load in R packages for PCA
library(missMDA)
library(FactoMineR)

# subset the data (independent variables only)
db21_subset <- db21[ ,13:31]

# Normalizing the data

# trying to do PCA
pc <- prcomp(db21_subset,
             center = TRUE,
             scale. = TRUE)

nb <- estim_ncpPCA(db21_subset,ncp.min = 0, ncp.max = 5, scale=TRUE,method.cv = "gcv")

db.comp <- imputePCA(db21_subset,ncp=6)
db.pca <- PCA(db.comp$completeObs)
graph.var(db.pca)
gr <- plot(db.pca)

plot(db.pca, col=site)
plotellipses(db.pca,keepvar=13)


# figures
boxplot(db21$`NPOC (mg/L)`~db21$sampledate,ylab="Concentration (mg/L)",xlab="Date", main="DOC")
boxplot(db21$`NO3-N (mg/L)`~db21$sampledate,ylab="Concentration (mg/L)",xlab="Date", main="NO3")
db21$sampledate <- as.factor(db21$sampledate)

DOCtime<- ggplot(db21, aes(x=sampledate, y=`NPOC (mg/L)`,fill=season)) +
  geom_boxplot()+  
  ggtitle("DOC")+
  labs(y= "DOC (mg/L)", x = "Date")+
  scale_x_discrete(labels=c('October','','','', 'December','','','', 'February', '','','April','','','June','','','September','','','December','','','')) +
  theme(axis.text.x = element_text(angle = 0),rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())
DOCtime + scale_fill_manual(values=c("#FF6633", "#993399","#66CC99","#3399CC"))

NO3time<- ggplot(db21, aes(x=sampledate, y=`NO3-N (mg/L)`,fill=season)) +
  geom_boxplot()+  
  ggtitle(bquote(NO[3]-N))+
  labs(y= bquote(NO[3]-N), x = "Date")+
  scale_x_discrete(labels=c('October','','','', 'December','','','', 'February', '','','April','','','June','','','September','','','December','','','')) +
  theme(axis.text.x = element_text(angle = 0),rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())
NO3time + scale_fill_manual(values=c("#FF6633", "#993399","#66CC99","#3399CC"))

Cltime<- ggplot(db21, aes(x=sampledate, y=`Cl (mg/L)`,fill=season)) +
  geom_boxplot()+  
  ggtitle(bquote(Cl))+
  labs(y= bquote(Cl), x = "Date")+
  scale_x_discrete(labels=c('October','','','', 'December','','','', 'February', '','','April','','','June','','','September','','','December')) +
  theme(axis.text.x = element_text(angle = 0),rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())
Cltime + scale_fill_manual(values=c("#FF6633", "#993399","#66CC99","#3399CC"))

SO4time<- ggplot(db21, aes(x=sampledate, y=`SO4 (mg/L)`,fill=season)) +
  geom_boxplot()+  
  ggtitle(bquote(SO[4]))+
  labs(y= bquote(SO[4]), x = "Date")+
  scale_x_discrete(labels=c('October','','','', 'December','','','', 'February', '','','April','','','June','','','September','','','December')) +
  theme(axis.text.x = element_text(angle = 0),rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())
SO4time + scale_fill_manual(values=c("#FF6633", "#993399","#66CC99","#3399CC"))

#par(mfrow=c(5,1))
boxplot(db21$`NPOC (mg/L)`~db21$site,ylab="Concentration (mg/L)",xlab="Site", main="DOC")
boxplot(db21$`NO3-N (mg/L)`~db21$site,ylab="Concentration (mg/L)",xlab="Site", main="NO3")
boxplot(db21$`Cl (mg/L)`~db21$site,ylab="Concentration (mg/L)",xlab="Site", main="Cl")
boxplot(db21$`SO4 (mg/L)`~db21$site,ylab="Concentration (mg/L)",xlab="Site", main="SO4")
boxplot(db21$`Na (mg/L)`~db21$site,ylab="Concentration (mg/L)",xlab="Site", main="Na")
boxplot(db21$`TC (mg/L)`~db21$site,ylab="Concentration (mg/L)",xlab="Site", main="TC")
boxplot(db21$`TDN (mg/L-N)`~db21$site,ylab="Concentration (mg/L-N)",xlab="Site", main="TDN")
boxplot(db21$`TN (mg/L-N)`~db21$site,ylab="Concentration (mg/L-N)",xlab="Site", main="TN")
boxplot(db21$`K (mg/L)`~db21$site,ylab="Concentration (mg/L)",xlab="Site", main="K")
boxplot(db21$`Mg (mg/L)`~db21$site,ylab="Concentration (mg/L)",xlab="Site", main="Mg")
boxplot(db21$`Ca (mg/L)`~db21$site,ylab="Concentration (mg/L)",xlab="Site", main="Ca")
boxplot(db21$Temp~db21$site,ylab="Temp (C))",xlab="Site", main="Temperature")
boxplot(db21$`DO%`~db21$site,ylab="DO %",xlab="Site", main="Dissolved Oxygen")
boxplot(db21$SPC~db21$site,ylab="SPC",xlab="Site", main="Conductivity")



barplot(DOCwetland$WetlandPercent[1:28],main="Wetland Percent")
barplot(DOCwetland$AgPercent[1:28],main="Agriculture Percent")
barplot(DOCwetland$ForestPercent[1:28],main="Forest Percent")

par(mfrow=c(1,1))

boxplot(df21$doc~df21$area,ylab="Concentration (mg/L)",xlab="Area", main="DOC")
boxplot(df21$no3~df21$area,ylab="Concentration (mg/L)",xlab="Area", main="NO3")


df21$Colour="#FF6633"
df21$Colour[df21$season=="Winter"]="#3399CC"
df21$Colour[df21$season=="Spring"]="#993399"
df21$Colour[df21$season=="Summer"]="#66CC99"
plot(df21$area,df21$doc,ylab="Concentration (mg/L)",xlab="Area", main="DOC", log='y',col=df21$Colour,pch=16,ylim=c(1,15))
legend("bottomright", legend=c("Fall", "Winter","Spring","Summer"),
       col=c("#FF6633","#3399CC", "#993399","#66CC99"), pch=16, cex=0.8)

plot(df21$area,df21$no3,ylab="Concentration (mg/L)",xlab="Area", main="NO3", log='y',col=df21$Colour,pch=16)
legend("bottomright", legend=c("Fall", "Winter","Spring","Summer"),
       col=c("#FF6633","#3399CC", "#993399","#66CC99"), pch=16, cex=0.8)

df21$site <- factor(df21$site)
plot(as.numeric(df21$site),df21$doc,ylab="Concentration (mg/L)",xlab="Site", main="DOC",col=df21$Colour,pch=16)
plot(as.numeric(df21$site),df21$no3,ylab="Concentration (mg/L)",xlab="Site", main="NO3",col=df21$Colour,pch=16)
plot(as.numeric(df21$site),df21$don,ylab="Concentration (mg/L)",xlab="Site", main="DON",col=df21$Colour,pch=16)
plot(as.numeric(df21$site),df21$tdn,ylab="Concentration (mg/L)",xlab="Site", main="TDN",col=df21$Colour,pch=16)
plot(as.numeric(df21$site),df21$so4,ylab="Concentration (mg/L)",xlab="Site", main="SO4",col=df21$Colour,pch=16)
plot(as.numeric(df21$site),df21$Ca,ylab="Concentration (mg/L)",xlab="Site", main="Ca",col=df21$Colour,pch=16)
plot(as.numeric(df21$site),df21$Cl,ylab="Concentration (mg/L)",xlab="Site", main="Cl",col=df21$Colour,pch=16)
plot(as.numeric(df21$site),df21$K,ylab="Concentration (mg/L)",xlab="Site", main="K",col=df21$Colour,pch=16)
plot(as.numeric(df21$site),df21$Mg,ylab="Concentration (mg/L)",xlab="Site", main="Mg",col=df21$Colour,pch=16)
plot(as.numeric(df21$site),df21$Na,ylab="Concentration (mg/L)",xlab="Site", main="Na",col=df21$Colour,pch=16)
plot(as.numeric(df21$site),df21$tn,ylab="Concentration (mg/L)",xlab="Site", main="TN",col=df21$Colour,pch=16)
plot(as.numeric(df21$site),df21$temp,ylab="Temperature (C)",xlab="Site", main="Temp",col=df21$Colour,pch=16)
plot(as.numeric(df21$site),df21$`DO%`,ylab="Dissolved Oxygen (%)",xlab="Site", main="DO %",col=df21$Colour,pch=16)
plot(as.numeric(df21$site),df21$SPC,ylab="SPC",xlab="Site", main="Conductivity",col=df21$Colour,pch=16)

# Plotting Concentration over Area for all Solutes
plot(as.numeric(df21$area),df21$doc,ylab="Concentration (mg/L)",xlab="Area (sq. km)", main="DOC",col=df21$Colour,pch=16)
plot(as.numeric(df21$area),df21$no3,ylab="Concentration (mg/L)",xlab="Area (sq. km)", main="NO3",col=df21$Colour,pch=16)
plot(as.numeric(df21$area),df21$don,ylab="Concentration (mg/L)",xlab="Area (sq. km)", main="DON",col=df21$Colour,pch=16)
plot(as.numeric(df21$area),df21$tdn,ylab="Concentration (mg/L)",xlab="Area (sq. km)", main="TDN",col=df21$Colour,pch=16)
plot(as.numeric(df21$area),df21$so4,ylab="Concentration (mg/L)",xlab="Area (sq. km)", main="SO4",col=df21$Colour,pch=16)
plot(as.numeric(df21$area),df21$Ca,ylab="Concentration (mg/L)",xlab="Area (sq. km)", main="Ca",col=df21$Colour,pch=16)
plot(as.numeric(df21$area),df21$Cl,ylab="Concentration (mg/L)",xlab="Area (sq. km)", main="Cl",col=df21$Colour,pch=16)
plot(as.numeric(df21$area),df21$K,ylab="Concentration (mg/L)",xlab="Area (sq. km)", main="K",col=df21$Colour,pch=16)
plot(as.numeric(df21$area),df21$Mg,ylab="Concentration (mg/L)",xlab="Area (sq. km)", main="Mg",col=df21$Colour,pch=16)
plot(as.numeric(df21$area),df21$Na,ylab="Concentration (mg/L)",xlab="Area (sq. km)", main="Na",col=df21$Colour,pch=16)
plot(as.numeric(df21$area),df21$tn,ylab="Concentration (mg/L)",xlab="Area (sq. km)", main="TN",col=df21$Colour,pch=16)
plot(as.numeric(df21$area),df21$temp,ylab="Temperature (C)",xlab="Area (sq. km)", main="Temp",col=df21$Colour,pch=16)
plot(as.numeric(df21$area),df21$`DO%`,ylab="Dissolved Oxygen (%)",xlab="Area (sq. km)", main="DO %",col=df21$Colour,pch=16)
plot(as.numeric(df21$area),df21$SPC,ylab="SPC",xlab="Area (sq. km)", main="Conductivity",col=df21$Colour,pch=16)


# Plotting as function of area and wetland %
AreaNO3 <- ggplot(df21,
                     aes(x = area,
                         y = no3,
                         col = season, size = PercentWetland)) +
  geom_point()+
  geom_hline(yintercept=0, size=0.5) +
  ggtitle(bquote(NO[3]-N))+
  labs(y= "Concentration (mg/L)", x = "Subcatchment Area")+
  theme(rect = element_rect(fill = "transparent"),strip.background = element_blank(),
        panel.background = element_blank(),panel.grid=element_line(color = "grey90"),
        legend.background=element_blank(),legend.key = element_blank())
AreaNO3 + scale_color_manual(values=c("#FF6633", "#993399","#66CC99","#3399CC"))

plot(df21$date,df21$SPC,ylab="SPC",xlab="Site", main="Conductivity",col=df21$Colour,pch=16)

boxplot(DOCfall$value~DOCfall$area,ylab="Concentration (mg/L)",xlab="Area", main="DOC")
boxplot(NO3fall$value~NO3fall$area,ylab="Concentration (mg/L)",xlab="Area", main="NO3")


plot(DOCfall$area,DOCfall$value,ylab="Concentration (mg/L)",xlab="Area", main="DOC")
plot(NO3fall$area,NO3fall$value,ylab="Concentration (mg/L)",xlab="Area", main="NO3")


plot(DOCwinter$area,DOCwinter$value)
plot(NO3winter$area, NO3winter$value)

plot(DOC$area,DOC$leverage.Q)

plot(wetland$SUM_Shape_Area_Wetland, wetland$SUM_Shape_Area_Catchement)

# Wrtiting excel sheet
write.csv(lev.21, "C:/Users/Caroline/Google Drive/Research/Lab/Data Files/Synoptic Analysis\\Leverage.csv")
write.csv(DOCfall, "C:/Users/Caroline/Google Drive/Research/Lab/Data Files/Synoptic Analysis\\DOCfall.csv")
write.csv(DOCwinter, "C:/Users/Caroline/Google Drive/Research/Lab/Data Files/Synoptic Analysis\\DOCwinter.csv")
write.csv(DOCspring, "C:/Users/Caroline/Google Drive/Research/Lab/Data Files/Synoptic Analysis\\DOCspring.csv")
write.csv(DOCsummer, "C:/Users/Caroline/Google Drive/Research/Lab/Data Files/Synoptic Analysis\\DOCsummer.csv")

write.csv(NO3fall, "C:/Users/Caroline/Google Drive/Research/Lab/Data Files/Synoptic Analysis\\NO3fall.csv")
write.csv(NO3winter, "C:/Users/Caroline/Google Drive/Research/Lab/Data Files/Synoptic Analysis\\NO3winter.csv")
write.csv(NO3spring, "C:/Users/Caroline/Google Drive/Research/Lab/Data Files/Synoptic Analysis\\NO3spring.csv")
write.csv(NO3summer, "C:/Users/Caroline/Google Drive/Research/Lab/Data Files/Synoptic Analysis\\NO3summer.csv")
