#####Deleting Environment Variables#####
rm(list = ls())

#####Getting Working Directory#####
getwd()

#####Setting Working Directory#####
setwd("C:/Users/Tej/Desktop")

#####Reading Data#####
data = read.csv(file= "datafile.csv", header = T)
head(data)
attach(data)


#####Names of the columns in Data#####
names(data)

#####length of data#####
data_length = length(Avg_Temp)
data_length

#####Excluding Stemplot as observations are more than 50#####
stem(Avg_Temp)
#####Boxplot#####
boxplot(Avg_Temp, main = "Box Plot for Average Temperature", ylab = "Average Temperature")

#####Finding Summary values#####
summary(Avg_Temp)
fivenum(Avg_Temp)
hist(Avg_Temp)

#####Finding letter values#####

#####Finding Fivenum summary values#####
f = fivenum(Avg_Temp)
fourth_spread = f[4]-f[2]
fourth_spread
STEP<- 1.5*fourth_spread
STEP

#####Finding the inner and outer fences#####
Lower_inner_fence<-f[2]-STEP 
Lower_inner_fence
Upper_inner_fence<-f[4]+STEP
Upper_inner_fence
Lower_outer_fence<-f[2]-2*STEP 
Lower_outer_fence
Upper_outer_fence<-f[4]+2*STEP
Upper_outer_fence

#####Finding Outliers#####

#####Identifying the mild outliers#####
outlier_mild<-Avg_Temp[Avg_Temp>Upper_inner_fence | Avg_Temp<Lower_inner_fence]
outlier_mild

#####Identifying extreme outliers#####
outlier_extreme<-Avg_Temp[Avg_Temp>Upper_outer_fence | Avg_Temp<Lower_outer_fence]
outlier_extreme

#####Comparing Grouped Data#####

#####Grouping the entire data by aggregate function#####
A<-aggregate(Avg_Temp,list(group=year), fivenum)
A$group
gf = A$x

#####Finding letter values by Country groups#####
gspread<- (gf[,4]-gf[,2])
gSTEP<- 1.5*(gf[,4]-gf[,2])
gLower_inner_fence<-gf[,2]-gSTEP
gUpper_inner_fence<-gf[,4]+gSTEP
gLower_outer_fence<-gf[,2]-2*gSTEP
gUpper_outer_fence<-gf[,4]+2*gSTEP

#####assigning data based on the Country Group#####
library(base64enc)
Avg_Temp.China = subset(Avg_Temp, Country=="China")
Avg_Temp.China
Avg_Temp.India = subset(Avg_Temp, Country=="India")
Avg_Temp.India
Avg_Temp.Japan = subset(Avg_Temp, Country=="Japan")
Avg_Temp.Japan
Avg_Temp.Malaysia = subset(Avg_Temp, Country=="Malaysia")
Avg_Temp.Malaysia
Avg_Temp.Pakistan = subset(Avg_Temp, Country=="Pakistan")
Avg_Temp.Pakistan
Avg_Temp.Philippines = subset(Avg_Temp, Country=="Philippines")
Avg_Temp.Philippines
Avg_Temp.Russia = subset(Avg_Temp, Country=="Russia")
Avg_Temp.Russia
Avg_Temp.SaudiArabia = subset(Avg_Temp, Country=="Saudi Arabia")
Avg_Temp.SaudiArabia
Avg_Temp.Singapore = subset(Avg_Temp, Country=="Singapore")
Avg_Temp.Singapore
Avg_Temp.Thailand = subset(Avg_Temp, Country=="Thailand")
Avg_Temp.Thailand
Avg_Temp.UnitedArabEmirates = subset(Avg_Temp, Country=="United Arab Emirates")
Avg_Temp.UnitedArabEmirates

Avg_Temp.2011 = subset(Avg_Temp, year==2011)
Avg_Temp.2011
Avg_Temp.2012 = subset(Avg_Temp, year=="2012")
Avg_Temp.2012
Avg_Temp.2013 = subset(Avg_Temp, year=="2013")
Avg_Temp.2013



#####Finding the length of the Grouped Data#####
length(Avg_Temp.China)
length(Avg_Temp.India)
length(Avg_Temp.Japan)
length(Avg_Temp.Malaysia)
length(Avg_Temp.Pakistan)
length(Avg_Temp.Philippines)
length(Avg_Temp.Russia)
length(Avg_Temp.SaudiArabia)
length(Avg_Temp.Singapore)
length(Avg_Temp.Thailand)
length(Avg_Temp.UnitedArabEmirates)
length(Avg_Temp.2011)
length(Avg_Temp.2012)
length(Avg_Temp.2013)

#####Finding outliers by Country wise#####
Avg_Temp.China[(Avg_Temp.China<gLower_inner_fence[1]|Avg_Temp.China>gUpper_inner_fence[1])]
Avg_Temp.India[(Avg_Temp.India<gLower_inner_fence[2]|Avg_Temp.India>gUpper_inner_fence[2])]
Avg_Temp.Japan[(Avg_Temp.Japan<gLower_inner_fence[3]|Avg_Temp.Japan>gUpper_inner_fence[3])]
Avg_Temp.Malaysia[(Avg_Temp.Malaysia<gLower_inner_fence[4]|Avg_Temp.Malaysia>gUpper_inner_fence[4])]
Avg_Temp.Pakistan[(Avg_Temp.Pakistan<gLower_inner_fence[5]|Avg_Temp.Pakistan>gUpper_inner_fence[5])]
Avg_Temp.Philippines[(Avg_Temp.Philippines<gLower_inner_fence[6]|Avg_Temp.Philippines>gUpper_inner_fence[6])]
Avg_Temp.Russia[(Avg_Temp.Russia<gLower_inner_fence[7]|Avg_Temp.Russia>gUpper_inner_fence[7])]
Avg_Temp.SaudiArabia[(Avg_Temp.SaudiArabia<gLower_inner_fence[8]|Avg_Temp.SaudiArabia>gUpper_inner_fence[8])]
Avg_Temp.Singapore[(Avg_Temp.Singapore<gLower_inner_fence[9]|Avg_Temp.Singapore>gUpper_inner_fence[9])]
Avg_Temp.Thailand[(Avg_Temp.Thailand<gLower_inner_fence[10]|Avg_Temp.Thailand>gUpper_inner_fence[10])]
Avg_Temp.UnitedArabEmirates[(Avg_Temp.UnitedArabEmirates<gLower_inner_fence[11]|Avg_Temp.UnitedArabEmirates>gUpper_inner_fence[11])]

#####Plotting boxplot for Average Temperature vs Country#####
boxplot(Avg_Temp~Country, xlab="Country", ylab="Average Temperature", main="Boxplot of Average Temperature vs Country", las=3, col = "skyblue")
boxplot(Avg_Temp~year, xlab="year", ylab="Average Temperature", main="Boxplot of Average Temperature vs Country", las=3, col = "skyblue")
#####Using spread VS Level Plot#####

#####Finding the log median and log spread values#####
Log_Median = log10(A$x[,3])
Log_Spread = log10(gspread)

#####Median Median line (Resistant line fitting)  Function#####

median_median_line <- function(x, y, data)
{
  if(!missing(data))
  {
    x <- eval(substitute(x), data) 
    y <- eval(substitute(y), data) 
  }
  
  stopifnot(length(x) == length(y))
  
  #Step 1
  one_third_length <- floor(length(x) / 3)
  groups <- rep(1:3, times = switch((length(x) %% 3) + 1,
                                    one_third_length,
                                    c(one_third_length, one_third_length + 1, one_third_length),
                                    c(one_third_length + 1, one_third_length, one_third_length + 1)
  ))
  
  #Step 2
  
  y <- y[order(x)]
  
  x <- sort(x)
  
  groups<-sort(groups)
  
  
  #Step 3
  median_x <- tapply(x, groups, median)                                 
  median_y <- tapply(y, groups, median)
  
  #Step 4
  slope <- (median_y[3] - median_y[1]) / (median_x[3] - median_x[1])
  intercept <- median_y[1] - slope * median_x[1]
  
  #Step 5
  middle_prediction <- intercept + slope * median_x[2]
  intercept <- intercept + (median_y[2] - middle_prediction) / 3
  c(intercept = unname(intercept), slope = unname(slope))
  
}

par(mfrow = c(1,2))

#####Plotiong Graph and fitting resistant line#####
res_line<-median_median_line(Log_Median, Log_Spread)
res_line
fitted<-Log_Median*res_line[2]+res_line[1]
plot(Log_Median, Log_Spread, main="Scatter plot and fitted resistant line")
lines(Log_Median, fitted, type="l")

#####Based in the resitant line slope, changing the raw data by using power if 1-b, b=slope #####

#####Tranforming the data using Power#####
nAvg_Temp = Avg_Temp^(1-res_line[2])
nAvg_Temp_Total = data.frame(nAvg_Temp,year)

#####Plotting boxplot for transformed data#####
#with outliers
boxplot(nAvg_Temp_Total$nAvg_Temp~nAvg_Temp_Total$year, las=3)
#excluding outliers
boxplot(nAvg_Temp_Total$nAvg_Temp~nAvg_Temp_Total$year, outline = FALSE, las=3)

#####Finding the fivenum summary and letter values for trasnformed data#####
nA<-aggregate(nAvg_Temp,list(group=year), fivenum)
nA$group
ngf = nA$x
ngspread<- (ngf[,4]-ngf[,2])
ngSTEP<- 1.5*(gf[,4]-ngf[,2])
ngLower_inner_fence<-ngf[,2]-ngSTEP
ngUpper_inner_fence<-ngf[,4]+ngSTEP
ngLower_outer_fence<-ngf[,2]-2*ngSTEP
ngUpper_outer_fence<-ngf[,4]+2*ngSTEP

#####Grouping transformed data based on the country #####
nAvg_Temp.China = subset(nAvg_Temp, Country=="China")
nAvg_Temp.China
nAvg_Temp.India = subset(nAvg_Temp, Country=="India")
nAvg_Temp.India
nAvg_Temp.Japan = subset(nAvg_Temp, Country=="Japan")
nAvg_Temp.Japan
nAvg_Temp.Malaysia = subset(nAvg_Temp, Country=="Malaysia")
nAvg_Temp.Malaysia
nAvg_Temp.Pakistan = subset(nAvg_Temp, Country=="Pakistan")
nAvg_Temp.Pakistan
nAvg_Temp.Philippines = subset(nAvg_Temp, Country=="Philippines")
nAvg_Temp.Philippines
nAvg_Temp.Russia = subset(nAvg_Temp, Country=="Russia")
nAvg_Temp.Russia
nAvg_Temp.SaudiArabia = subset(nAvg_Temp, Country=="Saudi Arabia")
nAvg_Temp.SaudiArabia
nAvg_Temp.Singapore = subset(nAvg_Temp, Country=="Singapore")
nAvg_Temp.Singapore
nAvg_Temp.Thailand = subset(nAvg_Temp, Country=="Thailand")
nAvg_Temp.Thailand
nAvg_Temp.UnitedArabEmirates = subset(nAvg_Temp, Country=="United Arab Emirates")
nAvg_Temp.UnitedArabEmirates

nAvg_Temp.2011 = subset(nAvg_Temp, year==2011)
nAvg_Temp.2011
nAvg_Temp.2012 = subset(nAvg_Temp, year=="2012")
nAvg_Temp.2012
nAvg_Temp.2013 = subset(nAvg_Temp, year=="2013")
nAvg_Temp.2013

#####Finding the log median and log spread values for spread VS level plot for Transformed data#####
nLog_Median = log10(nA$x[,3])
nLog_Spread = log10(ngspread)

#####Plotting spread vs level plot for transformed data#####
nres_line<-median_median_line(nLog_Median, nLog_Spread)
nres_line
nfitted<-nLog_Median*nres_line[2]+nres_line[1]
plot(nLog_Median, nLog_Spread, main="Scatter plot and fitted resistant line for transformed data")
lines(nLog_Median, nfitted, type="l")

#####Examining the raw and transformed data##### 
#####Considering the df raw and df re-expressed#####
raw_max = max(gspread)
raw_min = min(gspread)
trans_max = max(ngspread)
trans_min = min(ngspread)
raw_ratio = raw_max/raw_min
raw_ratio
trans_ratio = trans_max/trans_min
trans_ratio
trans_ratio-raw_ratio

#####Hinkley Method#####
####################################################
#####Spread Vs level plot didn't work here as the difference between the max to min ratio of spread is 0.4#####

##FOR Avg_Temp##
par(mfrow = c(3,2))

##plotting histograms

hist(Avg_Temp.India, freq = FALSE, main = "Histogram for raw data")
lines(density(Avg_Temp, bw=2), lwd=1)
plot(density(Avg_Temp,bw=2),lwd=1, axes=FALSE, xlab="", ylab="", main="")

##finding summary values

f_raw_Avg_Temp = summary(Avg_Temp.India)
f_raw_Avg_Temp

##finding hinkley values for raw data

hinkley_raw_Avg_Temp = (f_raw_Avg_Temp[4]-f_raw_Avg_Temp[3])/(f_raw_Avg_Temp[5]-f_raw_Avg_Temp[2])
hinkley_raw_Avg_Temp

##transforming raw data to roots

Avg_Temp_roots = sqrt(Avg_Temp.India)

##plotting histograms

hist(Avg_Temp_roots, freq = FALSE, main = "Histogram for Root transformed data")
lines(density(Avg_Temp_roots, bw=0.2), lwd=1)
plot(density(Avg_Temp_roots,bw=0.2),lwd=2, axes=FALSE, xlab="", ylab="", main="")

##finding summary values

f_roots_Avg_Temp = summary(Avg_Temp_roots)
f_roots_Avg_Temp

##finding hinkley values for root transformed data

hinkley_roots_Avg_Temp = (f_roots_Avg_Temp[4]-f_roots_Avg_Temp[3])/(f_roots_Avg_Temp[5]-f_roots_Avg_Temp[2])
hinkley_roots_Avg_Temp


##transforming raw data to logs

Avg_Temp_logs = log10(Avg_Temp.India)

##plotting histograms

hist(Avg_Temp_logs,freq = FALSE, main = "Histogram for log transformed data")
lines(density(Avg_Temp_logs, bw=0.2), lwd=2)
plot(density(Avg_Temp_logs,bw=0.2),lwd=2, axes=FALSE, xlab="", ylab="", main="")

##finding summary values

f_logs_Avg_Temp = summary(Avg_Temp_logs)
f_logs_Avg_Temp

##finding hinkley values for log transformed data

hinkley_logs_Avg_Temp = (f_logs_Avg_Temp[4]-f_logs_Avg_Temp[3])/(f_logs_Avg_Temp[5]-f_logs_Avg_Temp[2])
hinkley_logs_Avg_Temp

#######################################
##transforming raw data to another power

Avg_Temp_power = (Avg_Temp.India)^

f_power_Avg_Temp = summary(Avg_Temp_power)
f_power_Avg_Temp

hinkley_power_Avg_Temp = (f_power_Avg_Temp[4]-f_power_Avg_Temp[3])/(f_power_Avg_Temp[5]-f_power_Avg_Temp[2])
hinkley_power_Avg_Temp


#4.535
par(mfrow = c(1,2))
##plotting histograms

hist(Avg_Temp_power, freq = FALSE, main = "Histogram for transformed data")
lines(density(Avg_Temp_power, bw=40), lwd=2)
plot(density(Avg_Temp_power,bw=40),lwd=2, axes=FALSE, xlab="", ylab="", main="")

##finding summary values

f_power_Avg_Temp = summary(Avg_Temp_power)
f_power_Avg_Temp

##finding hinkley values for transformed area data

hinkley_power_Avg_Temp = (f_power_Avg_Temp[4]-f_power_Avg_Temp[3])/(f_power_Avg_Temp[5]-f_power_Avg_Temp[2])
hinkley_power_Avg_Temp

########################################################
##plotting symmetry plot for raw data




n_Avg_Temp = length(Avg_Temp.India)
n_Avg_Temp

u_Avg_Temp<-seq((n_Avg_Temp+1-(n_Avg_Temp)/2), (n_Avg_Temp+1-1), by=1)
u_Avg_Temp

ui_Avg_Temp<-sort(Avg_Temp.China)[rev(u_Avg_Temp)]
ui_Avg_Temp

ui_Avg_Temp<-sort(Avg_Temp.China)[rev(u_Avg_Temp)]-median(Avg_Temp.China)
ui_Avg_Temp

v_Avg_Temp<-seq(1,(n_Avg_Temp)/2,by=1)
v_Avg_Temp

vi_Avg_Temp<-median(Avg_Temp.China)-sort(Avg_Temp.China)[v_Avg_Temp]
vi_Avg_Temp

plot(vi_Avg_Temp,ui_Avg_Temp, main="Symmetry Plot of raw data")

su_Avg_Temp<-seq(0,25,1)
su_Avg_Temp

lines(su_Avg_Temp,su_Avg_Temp, type="l")

####to root####
roots = sqrt(Avg_Temp.China)
n_Avg_Temp = length(roots)
n_Avg_Temp

u_Avg_Temp<-seq((n_Avg_Temp+1-(n_Avg_Temp)/2), (n_Avg_Temp+1-1), by=1)
u_Avg_Temp

ui_Avg_Temp<-sort(roots)[rev(u_Avg_Temp)]
ui_Avg_Temp

ui_Avg_Temp<-sort(roots)[rev(u_Avg_Temp)]-median(roots)
ui_Avg_Temp

v_Avg_Temp<-seq(1,(n_Avg_Temp)/2,by=1)
v_Avg_Temp

vi_Avg_Temp<-median(roots)-sort(roots)[v_Avg_Temp]
vi_Avg_Temp

plot(vi_Avg_Temp,ui_Avg_Temp, main="Symmetry Plot of raw data")

su_Avg_Temp<-seq(0,25,1)
su_Avg_Temp

lines(su_Avg_Temp,su_Avg_Temp, type="l")
####to log####
logs = log10(Avg_Temp.China)
n_Avg_Temp = length(logs)
n_Avg_Temp

u_Avg_Temp<-seq((n_Avg_Temp+1-(n_Avg_Temp)/2), (n_Avg_Temp+1-1), by=1)
u_Avg_Temp

ui_Avg_Temp<-sort(logs)[rev(u_Avg_Temp)]
ui_Avg_Temp

ui_Avg_Temp<-sort(logs)[rev(u_Avg_Temp)]-median(logs)
ui_Avg_Temp

v_Avg_Temp<-seq(1,(n_Avg_Temp)/2,by=1)
v_Avg_Temp

vi_Avg_Temp<-median(logs)-sort(logs)[v_Avg_Temp]
vi_Avg_Temp

plot(vi_Avg_Temp,ui_Avg_Temp, main="Symmetry Plot of raw data")

su_Avg_Temp<-seq(0,25,1)
su_Avg_Temp

lines(su_Avg_Temp,su_Avg_Temp, type="l")

##########################################################

##plotting symmetry plot for transformed data

n_Avg_Temp_power = length(Avg_Temp_power)
n_Avg_Temp_power

u_Avg_Temp_power<-seq((n_Avg_Temp_power+1-(n_Avg_Temp_power+1)/2), (n_Avg_Temp_power+1-1), by=1)
u_Avg_Temp_power

ui_Avg_Temp_power<-sort(Avg_Temp_power)[rev(u_Avg_Temp_power)]
ui_Avg_Temp_power

ui_Avg_Temp_power<-sort(Avg_Temp_power)[rev(u_Avg_Temp_power)]-median(Avg_Temp_power)
ui_Avg_Temp_power

v_Avg_Temp_power<-seq(1,(n_Avg_Temp_power+1)/2,by=1)
v_Avg_Temp_power

vi_Avg_Temp_power<-median(Avg_Temp_power)-sort(Avg_Temp_power)[v_Avg_Temp_power]
vi_Avg_Temp_power

plot(vi_Avg_Temp_power,ui_Avg_Temp_power, main="Symmetry Plot of Transformed data")

su_Avg_Temp_power<-seq(0,200,1)
su_Avg_Temp_power

lines(su_Avg_Temp_power,su_Avg_Temp_power, type="l")

##########################################################


par(mfrow = c(1,1))
boxplot(Avg_Temp_power~Country, xlab="Country", ylab="IMDB Score", main="Boxplot of IMDB Score vs Country", las=3, col = "skyblue")

############plotting part1############
datafile = read.csv(file = "datafile_india.csv", header = T)
attach(datafile)
plot(datafile$Avg_Temp, datafile$Avg_Temp_Uncertain, xlab="Average Temperature", ylab="Average Temperature unicertainiity", main = "scatterplot for Average temperature uncertainity over the three years")

library(car)
scatterplot(datafile$Avg_Temp_Uncertain~datafile$Avg_Temp, xlab="Average Temperature", ylab="Average Temperature Uncertainity", main = "Scatter plot for Average Temperatures and Average Temperatures Unicertainity", smoother=FALSE, boxplots=FALSE)
lm(datafile$Avg_Temp_Uncertain~datafile$Avg_Temp)

FIT = -0.002048*( datafile$Avg_Temp - 16.778) + 0.30566
Residual <- datafile$Avg_Temp_Uncertain - FIT
plot(datafile$Avg_Temp, Residual, xlab="Average Temperature", ylab="Residual", main = "Scatter plot between Average Temperature and Residual")
abline(h=0, lwd=2)

#####Alternative Fit#####

plot(datafile$Avg_Temp, datafile$Avg_Temp_Uncertain, xlab="Average Temperature", ylab="Average Temperature Uncertain")
lines(lowess(datafile$Avg_Temp, datafile$Avg_Temp_Uncertain, f=0.2))


scatterplot(datafile$Avg_Temp_Uncertain~datafile$Avg_Temp, xlab="Average Temperature", ylab="Average Temperature Uncertain",boxplots=FALSE)

#####Resistant Line#####

plot(datafile$Avg_Temp, datafile$Avg_Temp_Uncertain)

log.Avg_Temp <- log10(datafile$Avg_Temp)
log.Avg_Temp_Uncertain <- log10(datafile$Avg_Temp_Uncertain)
plot(log.Avg_Temp, log.Avg_Temp_Uncertain)

log_data <- data.frame(log.Avg_Temp, log.Avg_Temp_Uncertain)
log_data
sorted.log_data <- log_data[order(log.Avg_Temp), ]
rounded_sorted.log_data = round(sorted.log_data, 2)

group_1 = (sorted.log_data$log.Avg_Temp[1:11])
group_2 = (sorted.log_data$log.Avg_Temp[12:21])
group_3 = (sorted.log_data$log.Avg_Temp[22:32])

length(group_1)
length(group_2)
length(group_3)
median(group_1)
median(group_2)
median(group_3)


plot(log.Avg_Temp, log.Avg_Temp_Uncertain, main="3 Groups and Summary Points")
abline(v=1.388)
abline(v=1.432)

rounded_sorted.log_data[6,]
rounded_sorted.log_data[16,]
rounded_sorted.log_data[17,]
rounded_sorted.log_data[27,]


s.points <- data.frame(x=c(1.3, 1.42, 1.46), y=c(-0.77, -0.74, -0.52))
points(s.points, cex=2, pch=19, col="red")
b0=(-0.52-(-0.77))/(1.46-1.3)
b0
##a0 =1/3 ([yL -b0(xL -xC)] + yC + [yR -b0(xR -xC)])
a0<-1/3*((-0.77-b0*(1.3-1.42))+(-0.74)+(-0.52-b0*(1.46-1.42)))
a0


plot(log.Avg_Temp, log.Avg_Temp_Uncertain, main="Resistant Line")
curve(1.5625 * (x - 1.42) - 0.635, add=TRUE, col="red")


#####mmline#####
mmline <- function(x, y, data)
{
  if(!missing(data))
  {
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)
  }
  stopifnot(length(x) == length(y))
  #Step 1
  one_third_length <- floor(length(x) / 3)
  groups <- rep(1:3, times = switch((length(x) %% 3) + 1,
                                    one_third_length,
                                    c(one_third_length, one_third_length + 1, one_third_length),
                                    c(one_third_length + 1, one_third_length, one_third_length + 1)
  ))
  #Step 2
  data<-data.frame(x,y)
  data<-data[order(data$x),]
  x<-data$x
  y<-data$y
  #Step 3
  groups<-sort(groups)
  median_x <- tapply(x, groups, median)
  median_y <- tapply(y, groups, median)
  #Step 4
  slope <- (median_y[3] - median_y[1]) / (median_x[3] - median_x[1])
  intercept <- 1/3*(median_y[1] - slope *(median_x[1]-median_x[2])
                    +median_y[2]+median_y[3] - slope *(median_x[3]-median_x[2]))
  d<-data.frame(median_x, median_y, slope, intercept)
  d$slope[2-3]
  d$intercept[2-3]
  return(d)
}

mfit<-mmline(log.Avg_Temp, log.Avg_Temp_Uncertain)
mfit

FIT<-mfit$intercept[1]+mfit$slope[1]*(log.Avg_Temp-mfit$median_x[2])
RESIDUAL <- log.Avg_Temp_Uncertain-FIT
data.frame(Movie=Movie_Name, log.IMDB_Score, log.Critic_Reviews, FIT, RESIDUAL)
plot(log.Avg_Temp, RESIDUAL)
abline(h=0, lwd=2)


############plotting part2############
plot(datafile$Avg_Temp, datafile$Avg_Temp_Uncertain)
datafile[unique(Avg_Temp)]

test = subset(datafile, !duplicated(Avg_Temp))
length(test$Avg_Temp)
plot(test$Avg_Temp, test$Avg_Temp_Uncertain)

## plotting a random line byb hand###
Fit_plot2 = 200*(test$IMDB_Score-8.2) + 516
curve(200 * (x - 8.2) + 516, add=TRUE, col="red")
Residual_plot2 = test$Critic_Reviews - Fit_plot2
Residual_plot2 = round(Residual_plot2,2)
plot(test$IMDB_Score, Residual_plot2)
abline(h=0, col="red")
test$Residual_plot2 = Residual_plot2

data2 = subset(test, test$IMDB_Score >6)
plot(data2$IMDB_Score, data2$Residual_plot2)
abline(h=0, col="red")

####Fitting a line with log values###
log_test_critic_reviews = log10(test$Critic_Reviews)
test$log_test_critic = log_test_critic_reviews
plot(test$IMDB_Score, log_test_critic_reviews)
Fit_log = 0.85*(test$IMDB_Score-2.2)
curve(0.85*(x-2.2) , add=TRUE, col="red")
residual_log = log_test_critic_reviews - Fit_log
test$Residual_log_plot3 = residual_log
plot(test$IMDB_Score, residual_log)
abline(h=0, col = "red")
data3 = subset(test, test$IMDB_Score < 5)
plot(data3$IMDB_Score, data3$Residual_log_plot3)
abline(h=0, col="red")


########################Straightning########################
datafile = read.csv(file = "datafile_strat.csv", header = T)

head(datafile)

plot(datafile$Avg_Temp, datafile$Avg_Temp_Uncertain)
lines(lowess(datafile$Avg_Temp, datafile$Avg_Temp_Uncertain, f=0.5))

length(datafile$Avg_Temp)
sort_data=datafile[order(datafile$Avg_Temp),]

length(sort_data$Avg_Temp)

group_straight_1=subset(sort_data[1:10,])
group_staright_2=subset(sort_data[11:21,])
group_straight_3=subset(sort_data[22:31,])

length(group_straight_1$Avg_Temp)
length(group_staright_2$Avg_Temp)
length(group_straight_3$Avg_Temp)

median(group_straight_1$Avg_Temp)
median(group_staright_2$Avg_Temp)
median(group_straight_3$Avg_Temp)


plot(sort_data$Avg_Temp, sort_data$Avg_Temp_Uncertain)

group_straight_1[5,]
group_straight_1[6,]

group_staright_2[6,]

group_straight_3[5,]
group_straight_3[6,]


summary.points <- data.frame(x=c(19.791, 25.061, 28.818), y=c(0.168, 0.2,0.309))
points(summary.points, cex=2, pch=19, col="red")
ml = (0.2-0.168)/(25.061-19.791)
mr = (0.309-0.2)/(28.818-25.061)
abline(lm(y~x, data=summary.points[1:2,]), col="blue")
abline(lm(y~x, data=summary.points[2:3,]), col="blue")
curve(exp(x), -0, 3, lwd=3, axes=FALSE, xlab="X", ylab="Y", main="Bulges to large x, small y")
arrows(1, 12, 2.5, 2, lwd=3); box()


straightening.work<-function(sp, px, py)
{
  sp$tx<-(sp$x^px-1)/px
  sp$ty<-(sp$y^py-1)/py
  sp$slope[1]<-diff(sp$ty[1:2])/diff(sp$tx[1:2])
  sp$slope[2]<-diff(sp$ty[2:3])/diff(sp$tx[2:3])
  sp$half.slope.ratio<-sp$slope[2]/sp$slope[1]
  sp$slope[3]<-NA
  sp$half.slope.ratio[2:3]<-NA
  row.names(sp)<-c("Left", "Center", "Right")
  sp
}
straightening.work(summary.points, 1, 1)
straightening.work(summary.points, 1.74, -4)
  new.x<-sort_data$Avg_Temp^(1.74)
  new.y<-sort_data$Avg_Temp_Uncertain^(-4)
  plot(new.x, new.y)
  length(new.y)
  mfit<-mmline(new.x, new.y)
  mfit
  FIT<-mfit$intercept+mfit$slope*(new.x-mfit$median_x[2])
  RESIDUAL<-new.y-FIT
  plot(new.x, RESIDUAL)
  abline(h=0)  
  
  
  #####Smoothing#####
  datafile = read.csv(file = "datafile.csv", header = T)
  
  smoothing_data1 = subset(datafile, datafile$Avg_Temp_Uncertain < 0.6 & datafile$Country == "India")
  length(smoothing_data1$date)
  
  smoothing_data = smoothing_data1[order(smoothing_data1$Avg_Temp), ]
  
  plot(smoothing_data$Avg_Temp, smoothing_data$Avg_Temp_Uncertain, xlab="Average Temperature", ylab = "Average temperature uncertain")
  smoothing_data[1:32,]
  smooth3<-c(NA, 0.267, 0.279, 0.279, 0.268, 0.168, 0.168, 0.306, 0.306, 0.216, 0.216, 0.373, 0.373, 0.224, 0.2, 0.19, 0.19, 0.242, 0.302, 0.242, 0.224, 0.205, 0.224, 0.227, 0.299, 0.299, 0.315, 0.299, 0.24, 0.221, 0.221, NA)
  cbind(smoothing_data, smooth3)
  smooth.3R<-smooth(smoothing_data$Avg_Temp_Uncertain, kind="3R")
  
  smooth.3R
  plot(smoothing_data$Avg_Temp, smooth.3R, type="l", col="red", lwd=2, xlab="Average Temperature", ylab="Average temperature uncertain", main="3R SMOOTH")
  # Splitting, "S, 3R, S, 3R"
  smooth.3RSS<-smooth(smoothing_data$Avg_Temp_Uncertain, kind="3RSS")
  plot(smoothing_data$Avg_Temp, smooth.3RSS, type="l", col="blue", lwd=2, xlab="Average Temperature", ylab="Average temperature uncertain", main="3RSS SMOOTH")
  
  plot(smoothing_data$Avg_Temp, smooth.3RSS, type="l", col="blue", lwd=2, xlab="Average Temperature", ylab="Average temperature uncertain", main="3R and 3RSS SMOOTHS")
  lines(smoothing_data$Avg_Temp, smooth.3R, col="red")
  legend("topleft", legend=c("3RSS","3R"), lty=1, col=c("blue", "red"))
  # Hanning on the 3RSS
  han<-function(x)
  { n<-length(x)
  y<-c(x[1], rep(0,n-2), x[n])
  z<-rep(0,n)
  { for ( i in 2: (n-1))
    y[i]<- (x[i-1]+x[i+1])/2}
  {for (j in 1:n)
    z[j]<-(x[j]+y[j])/2}
  z
  }
  smooth.3RSSH<-han(smooth.3RSS)
  plot(smoothing_data$Avg_Temp, smooth.3RSSH, type="l", col="red", lwd=2, xlab="Average Temperature", ylab="Average temperature uncertain", main="3RSSH SMOOTH")
  # Rough
  # Rough = DATA- SMOOTH
  # Reroughing: (on the note)
  Rough<-smoothing_data$Avg_Temp-smooth.3RSSH
  head(cbind(smoothing_data$Avg_Temp, smooth.3RSSH, Rough))
  as.vector(Rough)
  # we add the smooth of this rough back to the original data to get the our final smooth. This operation of reroughing is called 3RSSH, TWICE
  smooth_Avg_Temp = smoothing_data$Avg_Temp
  smooth.3RS3R.twice <- smooth(smooth_Avg_Temp, kind="3RS3R", twiceit=TRUE)
  plot(smoothing_data$Avg_Temp, smooth.3RS3R.twice, col="red", lwd=2, type="l", main="3RS3R, Twice Smooth", xlab="Movies")
  
  FinalRough<-smooth_IMDB_Score-smooth.3RS3R.twice
  par(mfrow = c(1,3))
  plot(as.numeric(smoothing_data$Movie_Name), FinalRough, pch=18, cex=1.3, xlab="Movies", ylab="ROUGH")
  plot(as.numeric(smoothing_data$Movie_Name), FinalRough, pch=18, cex=1, xlab="Movies", ylab="ROUGH")
  plot(as.numeric(smoothing_data$Movie_Name), FinalRough, pch=18, cex=1.5, xlab="Movies", ylab="ROUGH")
  abline(h=0,lwd=3, col="blue")
  
  
  
  
  