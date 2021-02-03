##############################################################################################
##
## 1 year lag i.e. 4 quarters
## but using the other two competitors to predict
##
##############################################################################################

rm(list=ls()) 

#import both datasets
#import data from air.txt
air <- as.data.frame(read.csv("air.txt", head=T, sep = "",))
#Import extra data from air2.txt
air2 <- as.data.frame(read.csv("air2.txt", head=T, sep = "",))

#Create Dataframe With only relevant variable columns in it


air$BRT <- as.numeric(as.character(air$BRT))
air$FX <- as.numeric(as.character(air$FX))


# Work out value per quarter for air dataset

X_CA <-ts(aggregate(CA ~ Quarter + Year ,data=air, mean), frequency = 4)
ts_CA <- X_CA
ts_SC <-ts(aggregate(SC ~ Quarter + Year,data=air, mean), frequency = 4)
ts_EC <-ts(aggregate(EC ~ Quarter + Year,data=air, mean), frequency = 4)

ts_BRT <-ts(aggregate(BRT ~ Quarter + Year,data=air, mean), frequency = 4)
ts_FX <-ts(aggregate(FX ~ Quarter + Year,data=air, mean), frequency = 4)



# Work out value per quarter for air2 dataset

ts_GDP <-ts(aggregate(GDP ~ Quarter + Year, data=air2, mean), frequency = 4)
ts_RP <-ts(aggregate(RailwayPassenger ~ Quarter + Year,data=air2, mean), frequency = 4)
ts_RF <-ts(aggregate(RailwayFreight ~ Quarter + Year,data=air2, mean), frequency = 4)
ts_HP <-ts(aggregate(HighwayPassenger ~ Quarter + Year,data=air2, mean), frequency = 4)
ts_HF <-ts(aggregate(HighwayFreight ~ Quarter + Year,data=air2, mean), frequency = 4)
ts_PV <-ts(aggregate(PassengerVolume ~ Quarter + Year,data=air2, mean), frequency = 4)
ts_IR <-ts(aggregate(InterestRate ~ Quarter + Year,data=air2, mean), frequency = 4)
ts_LBR <-ts(aggregate(Labour ~ Quarter + Year,data=air2, mean), frequency = 4)
ts_IE <-ts(aggregate(ImportExport ~ Quarter + Year,data=air2, mean), frequency = 4)
ts_PCDI <-ts(aggregate(PCDI ~ Quarter + Year,data=air2, mean), frequency = 4)


#ts_GDP 
#ts_RP 
#ts_RF 
#ts_HP 
#ts_HF 
#ts_PV 
#ts_IR
#ts_LBR
#ts_IE
#ts_PCDI

ts.plot( ts_BRT[,3], gpars = list(col = c("brown")), ylab = "Price in Dollars")


ts.plot( ts_FX[,3], gpars = list(col = c("orange")), ylab = "Rate")


ts.plot(ts_CA[,3], ts_SC[,3], ts_EC[,3], gpars = list(col = c("blue", "red", "green")), ylab = "Profit")

ts.plot(ts_CA[,3])

##Detrend and remove seasonality from the data

#install.packages("Ecdat")
library(Ecdat)



#Detrend Air Profits if nec

#test if seasonality present 
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_CA[,3]))

#YES

decompose_CA = decompose(ts_CA[,3], "additive")
plot(decompose_CA)
ts_CA = ts_CA[,3] - decompose_CA$seasonal
plot(ts_CA,  lwd = 2, ylab = "Profit")




#test if seasonality present 
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_SC[,3]))

#YES

decompose_SC = decompose(ts_SC[,3], "additive")
plot(decompose_SC)
ts_SC = ts_SC[,3] - decompose_SC$seasonal
plot(ts_SC,  lwd = 2, ylab = "Profit")




#test if seasonality present 
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_EC[,3]))

#YES

decompose_EC = decompose(ts_EC[,3], "additive")
plot(decompose_EC)
ts_EC = ts_EC[,3] - decompose_EC$seasonal
plot(ts_EC,  lwd = 2, ylab = "Profit")






ts_CA <-ts(ts_CA, frequency = 4)
ts_SC <-ts(ts_SC, frequency = 4)
ts_EC <-ts(ts_EC, frequency = 4)




ts.plot(ts_CA, ts_SC, ts_EC, gpars = list(col = c("black", "blue" , "red")), ylab = "Profit")



#Detrend Oil Prices

#test if seasonality present 
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_BRT[,3]))

#NO

#decompose_BRT  = decompose(ts_BRT , "multiplicative")
#plot(decompose_BRT)
#ts_BRT  = ts_BRT  / decompose_BRT$seasonal
#plot(ts_BRT[,3],  lwd = 2, ylab = "Price")

#no_trend_ts_BRT <- ts_BRT[,3]



ts.plot( ts_BRT, gpars = list(col = c("brown")), ylab = "Price in Dollars")


ts.plot( ts_FX, gpars = list(col = c("orange")), ylab = "Rate")



#Detrend Currency Exchange Prices

#test if seasonality present 
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_FX[,3]))

#NO

decompose_FX  = decompose(ts_FX , "multiplicative")
plot(decompose_FX)
ts_FX  = ts_FX  / decompose_FX$trend
#plot(ts_FX,  lwd = 2, ylab = "Rate")


#no_trend_ts_FX <- ts_FX[,3]

#plot(ts_FX[,3],  lwd = 2, ylab = "Rate")
#ts_FX <- ts_FX[,3]
#plot(decompose_FX)


#Plot seasonally tsed data for airline profits only vs
#their original graphs






### Note: needed to detrend the FX variable




ts_FX <-ts(ts_FX[,3], frequency = 4)
ts_BRT <-ts(ts_BRT[,3], frequency = 4)


ts_FX[1] <- 1.0037072
ts_FX[2] <- 1.0037072
ts_FX[52] <- 1.0212734
ts_FX[53] <- 1.0212734




###############################################
#Check for seasonality and trend in air2 data
################################################

#ts_GDP 
#test if seasonality present 
print("Testing the non-seasonal series")
library(seastests)
summary(wo(ts_GDP[,3]))

#YES

decompose_GDP = decompose(ts_GDP[,3], "additive")
plot(decompose_GDP)
ts_GDP = ts_GDP[,3] - decompose_GDP$seasonal
plot(ts_GDP,  lwd = 2, ylab = "Profit")

#REMOVE TREND
decompose_GDP  = decompose(ts_GDP , "multiplicative")
plot(decompose_GDP)
ts_GDP  = ts_GDP  / decompose_GDP$trend


ts_GDP[1] <- 0.999
ts_GDP[2] <- 0.999
ts_GDP[52] <- 1.011
ts_GDP[53] <- 1.011


#ts_RP
#test if seasonality present 
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_RP[,3]))

#YES

decompose_RP = decompose(ts_RP[,3], "additive")
plot(decompose_RP)
ts_RP = ts_RP[,3] - decompose_RP$seasonal
plot(ts_RP,  lwd = 2, ylab = "Profit")

#REMOVE TREND
decompose_RP  = decompose(ts_RP , "multiplicative")
plot(decompose_RP)
ts_RP  = ts_RP  / decompose_RP$trend


ts_RP[1] <- 1.08
ts_RP[2] <- 1.08
ts_RP[52] <- 1.17
ts_RP[53] <- 1.17


#ts_RF 
#test if seasonality present 
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_RF[,3]))

#YES

decompose_RF = decompose(ts_RF[,3], "additive")
plot(decompose_RF)
ts_RF = ts_RF[,3] - decompose_RF$seasonal
plot(ts_RF,  lwd = 2, ylab = "Profit")

#REMOVE TREND
decompose_RF  = decompose(ts_RF , "multiplicative")
plot(decompose_RF)
ts_RF  = ts_RF  / decompose_RF$trend

#Need to change vals
ts_RF[1] <- 0.99
ts_RF[2] <- 0.99
ts_RF[52] <- 1.01
ts_RF[53] <- 1.01


#ts_HP 
#test if seasonality present 
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_HP[,3]))

#YES

decompose_HP = decompose(ts_HP[,3], "additive")
plot(decompose_HP)
ts_HP = ts_HP[,3] - decompose_HP$seasonal
plot(ts_HP,  lwd = 2, ylab = "Profit")

#REMOVE TREND
decompose_HP  = decompose(ts_HP , "multiplicative")
plot(decompose_HP)
ts_HP  = ts_HP  / decompose_HP$trend


ts_HP[1] <- .928
ts_HP[2] <- .928
ts_HP[52] <- 1.00
ts_HP[53] <- 1.00


#ts_HF 
#test if seasonality present 
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_HF[,3]))

#YES

decompose_HF = decompose(ts_HF[,3], "additive")
plot(decompose_HF)
ts_HF = ts_HF[,3] - decompose_HF$seasonal
plot(ts_HF,  lwd = 2, ylab = "Profit")

#REMOVE TREND
decompose_HF  = decompose(ts_HF , "multiplicative")
plot(decompose_HF)
ts_HF  = ts_HF  / decompose_HF$trend


ts_HF[1] <- .67
ts_HF[2] <- .67
ts_HF[52] <- 1.01
ts_HF[53] <- 1.01

#ts_PV 
#test if seasonality present 
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_PV[,3]))

#YES

decompose_PV = decompose(ts_PV[,3], "additive")
plot(decompose_PV)
ts_PV = ts_PV[,3] - decompose_PV$seasonal
plot(ts_PV,  lwd = 2, ylab = "Profit")

#REMOVE TREND
decompose_PV  = decompose(ts_PV , "multiplicative")
plot(decompose_PV)
ts_PV  = ts_PV  / decompose_PV$trend


ts_PV[1] <- .966
ts_PV[2] <- .966
ts_PV[52] <- .99
ts_PV[53] <- .99


#ts_IR
#test if seasonality present 
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_IR[,3]))

#NO

#decompose_IR = decompose(ts_IR[,3], "additive")
#plot(decompose_IR)
#ts_IR = ts_IR[,3] - decompose_IR$seasonal
#plot(ts_IR,  lwd = 2, ylab = "Profit")

#REMOVE TREND
decompose_IR  = decompose(ts_IR , "multiplicative")
plot(decompose_IR)
ts_IR  = ts_IR  / decompose_IR$trend


ts_IR[1] <- .99
ts_IR[2] <- .99
ts_IR[52] <- .99
ts_IR[53] <- .99

#ts_LBR
#test if seasonality present 
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_LBR[,3]))

#YES

decompose_LBR = decompose(ts_LBR[,3], "additive")
plot(decompose_LBR)
ts_LBR = ts_LBR[,3] - decompose_LBR$seasonal
plot(ts_LBR,  lwd = 2, ylab = "Profit")

#REMOVE TREND
decompose_LBR  = decompose(ts_LBR , "multiplicative")
plot(decompose_LBR)
ts_LBR  = ts_LBR  / decompose_LBR$trend


ts_LBR[1] <- 1.06
ts_LBR[2] <- 1.06
ts_LBR[52] <- 1.00
ts_LBR[53] <- 1.00


#ts_IE
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_IE[,3]))
#YES

decompose_IE = decompose(ts_IE[,3], "additive")
plot(decompose_IE)
ts_IE = ts_IE[,3] - decompose_IE$seasonal
plot(ts_IE,  lwd = 2, ylab = "Profit")


decompose_IE  = decompose(ts_IE , "multiplicative")
plot(decompose_IE)
ts_IE  = ts_IE  / decompose_IE$trend



ts_IE[1] <- .95
ts_IE[2] <- .95
ts_IE[52] <- 1.02
ts_IE[53] <- 1.02


#ts_PCDI
library(seastests)
print("Testing the non-seasonal series")
summary(wo(ts_PCDI[,3]))
#YES


decompose_PCDI = decompose(ts_PCDI[,3], "additive")
plot(decompose_PCDI)
ts_PCDI = ts_PCDI[,3] - decompose_PCDI$seasonal
plot(ts_PCDI,  lwd2,  = ylab = "Profit")


decompose_PCDI  = decompose(ts_PCDI , "multiplicative")
plot(decompose_PCDI)
ts_PCDI  = ts_PCDI  / decompose_PCDI$trend



ts_PCDI[1] <- .99
ts_PCDI[2] <- .99

ts_PCDI[52] <- 1.01
ts_PCDI[53] <- 1.01

##################################################################################################
##
## Classical Lasso
##
##################################################################################################



#make 3 response vectors Y
#2006 to 2014 or 9:
Y_CA <- as.vector(ts_CA[5:36])
Y_SC <- as.vector(ts_SC[5:36])
Y_EC <- as.vector(ts_EC[5:36])

#predictors from air dataset
X_BRT <- as.vector(ts_BRT)
X_FX <- as.vector(ts_FX)
X_resp_lag <- as.vector(ts_SC) #Do this for each airline so 3 times
X_SC <- as.vector(ts_SC)
X_EC <- as.vector(ts_EC)

#predictors from air2 dataset
X_GDP <- as.vector(ts_GDP) #F
X_RP <- as.vector(ts_RP)
X_RF <- as.vector(ts_RF)
X_HP <- as.vector(ts_HP)
X_HF <- as.vector(ts_HF)
X_PV <- as.vector(ts_PV)
X_IR <- as.vector(ts_IR) #L
X_LBR <- as.vector(ts_IR) #M
X_IE <- as.vector(ts_IE)
X_PCDI <- as.vector(ts_PCDI)

#make overall predictor variable X
#make XA, XB and XC etc together and then bind them 

#air dataset variables

XA <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  
  XA[i,] <- X_BRT[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


XB <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XB[i,] <- X_FX[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}




#make overall predictor variable X
#make XA and XB together and then bind them 
XC <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XC[i,] <- X_resp_lag[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}




XD <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XD[i,] <- X_SC[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}





XE <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XE[i,] <- X_EC[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


#############################
#air 2 dataset variables
################################
XF <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XF[i,] <- X_GDP[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


XG <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XG[i,] <- X_RP[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


XH <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XH[i,] <- X_RF[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}



XI <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XI[i,] <- X_HP[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


XJ <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XJ[i,] <- X_HF[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


XK <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XK[i,] <- X_PV[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


XL <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XL[i,] <- X_IR[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


XM <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XM[i,] <- X_LBR[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


#ts_GDP
XN <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XN[i,] <- X_IE[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


#ts_PCDI
XO <- matrix(0,nrow=32,ncol=4)

row1 <- 1
row2 <- 4

for (i in 1:32)
{
  XO[i,] <- X_PCDI[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}




pred_X <- cbind(XA,XB,XC,XD,XE, XF, XG,XH,XI,XJ,XK,XL,XM, XN, XO)


#pred_X <- cbind(XC,XD,XE)
#pred_X <- cbind(XA,XB)
#pred_X <- XC


#Check
dim(pred_X) # is 36*24



#Fit glmnet
#need to install package glmnetUtils
#glmnet(x, y, family=c("gaussian","binomial","poisson","multinomial","cox","mgaussian"), weights, 
#offset=NULL, alpha = 1, nlambda = 100, lambda.min.ratio = ifelse(nobs<nvars,0.01,0.0001), lambda=NULL, standardize = TRUE, intercept=TRUE, thresh = 1e-07, dfmax = nvars + 1, pmax = min(dfmax * 2+20, nvars), exclude, penalty.factor = rep(1, nvars), lower.limits=-Inf, upper.limits=Inf, maxit=100000, type.gaussian=ifelse(nvars<500,"covariance","naive"), type.logistic=c("Newton","modified.Newton"), standardize.response=FALSE, type.multinomial=c("ungrouped","grouped"))

#standardize: Logical ???ag for x variable standardization, prior to ???tting the model sequence. 
#The coef???cients are always returned on the original scale. Defaultisstandardize=TRUE. 
#Ifvariablesareinthesameunitsalready,youmightnotwishtostandardize. See details below for y standardization with family="gaussian". 

# Run for three responses Y_CA, Y_SC, Y_EC 
library(glmnetUtils)
library(glmnet)

#lambda_seq <- 10^seq(2, -2, by = -.1)
lambda_seq <- seq(2.5, 10, by = .1) #3.5, 8,10 best so far 
fit.glmnet <- cv.glmnet(pred_X, Y_SC, family="gaussian", intercept=TRUE, standardize.response=TRUE,alpha=1, lambda=lambda_seq)
lambda_min <-min(fit.glmnet$lambda) # or lambda.lse
coef.glmnet <- coef(fit.glmnet, s=lambda_min)
sum(coef.glmnet == 0)

#print(fit.glmnet)

# Plot variable coefficients vs. shrinkage parameter lambda.
plot(fit.glmnet, xvar="lambda")







##PREDICT/FORECAST 
#Now run on another profit dataset to use it a test dataset

#Test_fit <- predict(fit.glmnet, s = lambda_min, type="response", newx = Cov_X[28:36,])


#make 3 response vectors Y
#2014 to 20** or :
Y_CA <- as.vector(ts_CA[37:53])
Y_SC <- as.vector(ts_SC[37:53])
Y_EC <- as.vector(ts_EC[37:53])


#predictors
#X_BRT <- as.vector(ts_BRT)
#X_FX <- as.vector(ts_FX)
#X_resp_lag <- as.vector(ts_CA) #change this to new airline data


#make overall predictor variable X in the same way, it's different XC though
#make XA, XB and XC together and then bind them 
XA <- matrix(0,nrow=17,ncol=4)


row1 <- 33
row2 <- 36

for (i in 1:17)
{
  
  XA[i,] <- X_BRT[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


XB <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XB[i,] <- X_FX[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}




#make overall predictor variable X
#make XA and XB together and then bind them 
XC <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XC[i,] <- X_resp_lag[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}




XD <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XD[i,] <- X_SC[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}





#make overall predictor variable X
#make XA and XB together and then bind them 
XE <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XE[i,] <- X_EC[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


#############################
#air2 dataset variables
#############################
XF <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XF[i,] <- X_GDP[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


XG <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XG[i,] <- X_RP[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


XH <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XH[i,] <- X_RF[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


XI <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XI[i,] <- X_HP[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


XJ <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XJ[i,] <- X_HF[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}



XK <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XK[i,] <- X_PV[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}


XL <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XL[i,] <- X_IR[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}



XM <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XM[i,] <- X_LBR[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}



XN <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XN[i,] <- X_IE[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}



XO <- matrix(0,nrow=17,ncol=4)

row1 <- 33
row2 <- 36

for (i in 1:17)
{
  XO[i,] <- X_PCDI[row1:row2]
  
  row1 <- row1 + 1 ;
  row2 <- row2 + 1 ;
  
}



pred_X_new <- cbind(XA,XB,XC,XD,XE, XF, XG, XH, XI, XJ, XK, XL, XM, XN, XO)


#check
dim(pred_X_new) # is 17*24 correct




Test_fit <- predict(fit.glmnet, s=lambda_min, type="response", newx = pred_X_new)


ts_Test_fit <-ts(Test_fit)


#Plot of actual vs predicted
ts.plot( ts_Test_fit, ts_SC[37:53], gpars = list(col = c("magenta","black")), ylab = "Profit")

#Plot of actual shifted along 4 quarters vs predicted
ts.plot( ts(ts_Test_fit[1:12]), ts_SC[42:53], gpars = list(col = c("magenta","black")), ylab = "Profit")



ser1 = Test_fit[2:12] - Test_fit[1:11]
ser2 = ts_SC[43:53] - ts_SC[42:52]


count_correct <- sign( ser1 ) == sign( ser2 )
sum(count_correct == TRUE)/length(count_correct)
#54% correct direction prediction  for just the lagged resp



#Mean Squared Error (RMSE)
RMSE = sqrt(  1/length(ts_Test_fit[1:12]) * sum( (ts_SC[42:53] - ts_Test_fit[1:12])^2 )  )
# LASSO
# Ridge
#14.14 for just the lagged resp
RMSE

#Mean Absolute Error (MAE)
MAE = 1/length(ts_Test_fit[1:12]) * sum(abs(ts_Test_fit[1:12] - ts_SC[42:53])) 
# LASSO
# Ridge
# for just the lagged resp
MAE


r2 <- fit.glmnet$dev.ratio[which(fit.glmnet$lambda == lambda_min)]
#55% for LASSO
#30% for just the lagged resp

#Calculations for next day predcictions



#Mean Squared Error (RMSE)
RMSE = sqrt(  1/length(ts_Test_fit) * sum( (ts_SC[37:53] - ts_Test_fit)^2 )  )
RMSE

#Mean Absolute Error (MAE)
MAE = 1/length(ts_Test_fit) * sum(abs(ts_Test_fit - ts_SC[37:53])) 
MAE




