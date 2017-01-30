#import regression library
library(s20x)

#read data into data frame
dird= #directory containing "nfl_drives.csv" file
myread=function(csv) {
  fl=paste(dird, csv, sep="")
  read.table(fl, header=TRUE, sep=",")
}
drives.df=myread("nfl_drives.csv")

#define 32 distinct colors for data identification in plots
team_colors = c("dodgerblue4", "red2", "royalblue4", "deepskyblue2", 
                "darkorchid", "darkred", "steelblue3", "midnightblue",
                "chocolate4", "firebrick1", "orangered1", "lightgoldenrod",
                "red", "royalblue4", "red3", "purple4",
                "dodgerblue", "navyblue", "springgreen4", "forestgreen",
                "yellow", "blue", "cornflowerblue", "blue3",
                "darkorange2", "gray9", "firebrick", "cyan2",
                "firebrick2", "turquoise3", "navy", "darkgreen")


#quick summary of data
head(drives.df)
summary(drives.df)
with(drives.df, factor(Team))

#look at relationships among data
pairs(drives.df)


#check linearity of relationship between chosen variables
trendscatter(Avg_Pts~Avg_Start, f=2/3, data=drives.df)


#create linear model and plot it over data
drives.lm=with(drives.df, lm(Avg_Pts~Avg_Start))
with(drives.df, plot(Avg_Pts~Avg_Start, main="Average Drive Start vs. Average Points/Drive Scatter Plot", 
                     ylab="Average Points/Drive", xlab="Average Drive Start", pch=21, bg=team_colors, cex=1.5,
                     ylim=c(0, 1.2*max(Avg_Pts)), xlim=c(0.99*min(Avg_Start), 1.01*max(Avg_Start))))
abline(drives.lm, col="Black", lwd=2)


#set up 4-paned plotting window
windows()
layout(matrix(1:4, nr=2, nc=2, byrow=TRUE))
layout.show(4)

#top left - repeat of above plot
with(drives.df, plot(Avg_Pts~Avg_Start, main="Average Drive Start vs. Average Points/Drive Scatter Plot", 
                     ylab="Average Points/Drive", xlab="Average Drive Start", pch=21, bg=team_colors, cex=1.5,
                     ylim=c(0, 1.2*max(Avg_Pts)), xlim=c(0.99*min(Avg_Start), 1.01*max(Avg_Start))))
abline(drives.lm, col="Black", lwd=2)

#top right - RSS
with(drives.df, plot(Avg_Pts~Avg_Start, main="Average Drive Start vs. Average Points/Drive Scatter Plot & RSS", 
                     ylab="Average Points/Drive", xlab="Average Drive Start", pch=21, bg=team_colors, cex=1.5,
                     ylim=c(0, 1.2*max(Avg_Pts)), xlim=c(0.99*min(Avg_Start), 1.01*max(Avg_Start))))
yhat=fitted(drives.lm)
with(drives.df, {segments(Avg_Start, Avg_Pts, Avg_Start, yhat)})
abline(drives.lm, col="Black", lwd=2)

#bottom left - MSS
with(drives.df, plot(Avg_Pts~Avg_Start, main="Average Drive Start vs. Average Points/Drive Scatter Plot & MSS", 
                     ylab="Average Points/Drive", xlab="Average Drive Start", pch=21, bg=team_colors, cex=1.5,
                     ylim=c(0, 1.2*max(Avg_Pts)), xlim=c(0.99*min(Avg_Start), 1.01*max(Avg_Start))))
abline(drives.lm, col="Black", lwd=2)
with(drives.df, abline(h=mean(Avg_Pts), col="Black", lwd=2))
with(drives.df, segments(Avg_Start, mean(Avg_Pts), Avg_Start, yhat, col="Red"))

#bottom right - TSS
with(drives.df, plot(Avg_Pts~Avg_Start, main="Average Drive Start vs. Average Points/Drive Scatter Plot & TSS", 
                     ylab="Average Points/Drive", xlab="Average Drive Start", pch=21, bg=team_colors, cex=1.5,
                     ylim=c(0, 1.2*max(Avg_Pts)), xlim=c(0.99*min(Avg_Start), 1.01*max(Avg_Start))))
with(drives.df,abline(h=mean(Avg_Pts), col="Black", lwd=2))
with(drives.df, segments(Avg_Start, Avg_Pts, Avg_Start, mean(Avg_Pts), col="Green"))


#calculate RSS, MSS, and TSS
RSS=with(drives.df, sum((Avg_Pts-yhat)^2))
MSS=with(drives.df, sum((yhat-mean(Avg_Pts))^2))
TSS=with(drives.df, sum((Avg_Pts-mean(Avg_Pts))^2))

#output residual, model, and total sums of squares
RSS
MSS
TSS

#verify that RSS + MSS = TSS
RSS+MSS

#MSS/TSS = Multiple R Squared ~ model accuracy
MSS/TSS


#residuals & fitted values
Avg_Pts.res=residuals(drives.lm)
Avg_Pts.fit=fitted(drives.lm)

#residual vs. fitted values plot
plot(drives.lm, which=1)

#normality test with shaprio wilk option
normcheck(drives.lm, shapiro.wilk=TRUE)


#summarize model
summary(drives.lm)
anova(drives.lm)

#confidence intervals for coefficents
ciReg(drives.lm)

#Avg_Pts predictions
predict(drives.lm, data.frame(Avg_Start=c(20, 25, 30)))

#detect outliers
cooks20x(drives.lm)

