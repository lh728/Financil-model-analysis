close_prices = read.csv("Data.csv", header = TRUE) #importing the data
sum(is.na(close_prices))
close_prices$Date = as.POSIXct(close_prices$Date,format="%Y/%m/%d", tz = "") # converting the first column into date format
close_prices <- na.omit(close_prices)
del <- which(close_prices$SHEL == 'null')
close_prices <- close_prices[-del,]
close_prices$CVX<-as.numeric(close_prices$CVX)
close_prices$XOM<-as.numeric(close_prices$XOM)
close_prices$COP<-as.numeric(close_prices$COP)
close_prices$SHEL<-as.numeric(close_prices$SHEL)
close_prices_xts <- xts(close_prices[,-1], order.by=close_prices[,1]) # convert the object into xts object (time series object)
# close_prices_xts <- xts(clean_df[,-1], order.by=clean_df[,1]) # convert the object into xts object (time series object)
close_prices_zoo  <- as.zoo(close_prices_xts) # convert the time series object into zoo object

## calculate the return
return = Return.calculate( close_prices_xts , method = "log") # automatically calculate return
return2 <- return[-1,]
adf.test(return2$CVX)
adf.test(return2$XOM)
adf.test(return2$COP)
adf.test(return2$SHEL)
descriptive.stat.return = stat.desc(return)
# a function to create CSAD and Rm
exchange.herd = function(return) 
{
  n=ncol(return)
  Rm = rowMeans(return)
  temp_dif =abs(return-Rm)
  temp_sum = rowSums(temp_dif)
  CSAD = temp_sum / ncol(return)
  CSAD = cbind (CSAD, Rm)
  return (CSAD)
}
f = exchange.herd(return)
head (f)
CSAD.df = fortify.zoo(f) # converting f into a dataframe (to simplify further calculations)
CSAD.df$Rm2 = CSAD.df$Rm^2 # calculating Rm^2
CSAD.df = CSAD.df[-c(1),] # removing the first row with NAs
y = CSAD.df$CSAD  # reassign my columns as Y and Xs to look better in the regression model
x1 = abs (CSAD.df$Rm)
x2 = CSAD.df$Rm2
plot(CSAD.df$Rm,y)

linearMod <- lm(y~x1+x2)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

require (tvReg)
tvlm.fit = tvLM(y~x1+x2, bw = NULL  ) #bw=0.6697039
head (tvlm.fit$coefficients)
plot(tvlm.fit$coefficients[,1], type="l")
plot(tvlm.fit$coefficients[,2], type="l")
plot(tvlm.fit$coefficients[,3], x = close_prices$Date[-1], type="l")
abline(h=0,lwd=1,col="red") 
