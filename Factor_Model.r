df = read.csv("Data2.csv", header = TRUE) #importing the data
sum(is.na(df))
df <- df[!chron::is.weekend(as.Date(df$Date, "%Y/%m/%d")), ]
df$Date = as.POSIXct(df$Date,format="%Y/%m/%d", tz = "") # converting the first column into date format
df2 = read.csv("Data3.csv", header = TRUE)
sum(is.na(df2))
df2$Date = as.POSIXct(df2$Date,format="%Y/%m/%d", tz = "") # converting the first column into date format
new_df <- merge(df,df2,by = "Date")
 
del <- which(new_df$SHEL == 'null')
new_df <- new_df[-del,] # remove 2022-1-31 to 2022-2-10 because new_df has null values
new_df$CVX<-as.numeric(new_df$CVX)
new_df$XOM<-as.numeric(new_df$XOM)
new_df$COP<-as.numeric(new_df$COP)
new_df$SHEL<-as.numeric(new_df$SHEL)
new_df$BP<-as.numeric(new_df$BP)
new_df$BTC.USD<-as.numeric(new_df$BTC.USD)
new_df$LTC.USD<-as.numeric(new_df$LTC.USD)
new_df$ETH.USD<-as.numeric(new_df$ETH.USD)
new_df$STETH.USD<-as.numeric(new_df$STETH.USD)
new_df$BNB.USD<-as.numeric(new_df$BNB.USD)
new_df_xts <- xts(new_df[,-1], order.by=new_df[,1])
df3 = read.csv("VGSH Historical Data.csv", header = TRUE)
sum(is.na(df3))
df3$Date = as.POSIXct(df3$Date,format="%Y/%m/%d", tz = "") # converting the first column into date format
df3 <- df3[-c(53:61),] # remove 2022-1-31 to 2022-2-10 because new_df has null values
df3$spread <- df3$VCLT - df3$VGLT
df3 <- df3[,-c(3:4)]
df3 <- xts(df3[,-1], order.by=df3[,1])

return = Return.calculate( new_df_xts , method = "log") # automatically calculate return
matrix <- round(cor(new_df_xts,method = c("pearson")),2)
corrplot(corr = matrix,method = 'circle',type = 'upper')

plot(df3$VGSH)
plot(df3$spread)
acf(df3$VGSH,lag = 100)
acf(df3$spread,lag = 100)

pacf(df3$VGSH,lag = 100)
pacf(df3$spread,lag = 100)

adf.test(df3$VGSH)
adf.test(df3$spread)

df3 <- diff(df3)
df3 <- df3[-1,]
adf.test(df3$VGSH)
adf.test(df3$spread)

arFit = ar(df3)
res = arFit$resid[2:245,] # residuals of the AR (5) modle (the interpretations of the residuals is that they are unexpected shocks)
lmfit = lm(new_df_xts[,1:5]~ res[,1]+res[,2]) #fit a regression Y is a set of Cryptocurrency stock returns and X1 and X2 - residuals from the AR modles
lmfit2 = lm(new_df_xts[,6:10]~ res[,1]+res[,2])  #fit a regression Y is a set of traditional assets stock returns
slmfit = summary(lmfit) #summary of the regression
slmfit2 = summary(lmfit2)
slmfit
slmfit2
rsq = rep(0,5) 

for (i in 1:5){rsq[i]= slmfit[[i]][[8]]} # I substruct the values of R2 from each of 5 models
Cryp_VGSH = lmfit$coef[2,] # extract all VGSH betas (b1)
Cryp_spread = lmfit$coef[3,] # extract all spread betas (b2)

par(mfrow=c(1,3)) # building three graphs in a row
barplot(rsq,horiz=T,names=names(Cryp_VGSH),main="R squared") #Creates a bar plot with vertical or horizontal bars.
barplot(Cryp_VGSH,hori=T,main="Cryp_VGSH") #Creates a bar plot with vertical or horizontal bars.
barplot(Cryp_spread,hori=T,main="Cryp_spread") #Creates a bar plot with vertical or horizontal bars.
rsq = rep(0,5) 

for (i in 1:5){rsq[i]= slmfit2[[i]][[8]]} # I substruct the values of R2 from each of 5 models
trad_VGSH = lmfit2$coef[2,] # extract all VGSH betas (b1)
trad_spread = lmfit2$coef[3,] # extract all spread betas (b2)

par(mfrow=c(1,3)) # building three graphs in a row
barplot(rsq,horiz=T,names=names(trad_VGSH),main="R squared") #Creates a bar plot with vertical or horizontal bars.
barplot(trad_VGSH,hori=T,main="trad_VGSH") #Creates a bar plot with vertical or horizontal bars.
barplot(trad_spread,hori=T,main="trad_spread") #Creates a bar plot with vertical or horizontal bars.

res.pca <- PCA(new_df_xts, graph = T, scale.unit = TRUE, ncp = 5)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
fa_none = factanal(new_df_xts,2,rotation="none") 
print(fa_none,cutoff=0.1) # By convention, any loading with an absolute value less than the parameter cutoff is not printed, and the default value of cutoff is 0.1
z=round(x=fa_none$loadings,digits=3)
a = vector(length = 10)
for (i in 1:10){
  a[i] = round(sum(z[i,1:2]^2),3)
}
cbind(z[,1:2],a)


fa_vari = factanal(new_df_xts,2,rotation="varimax") #factor model with rotation
print(fa_vari,cutoff=0.1,sort=T)

cortest.bartlett(new_df_xts)
KMO(new_df_xts)
