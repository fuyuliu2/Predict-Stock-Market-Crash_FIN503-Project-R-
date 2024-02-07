
# Part3 -------------------------------------------------------------------
Data <- read.csv("ProjectData_QF.csv")
head(Data)
FFER <- Data$Federal_Funds_Effective_Rate
TFO <- Data$Total_Federal_Outlays
HPI <- Data$House_Price_Index
UR <- Data$Unemployment_Rate
CI <- Data$Confidence_Indicators
CPI <- Data$CPI_in_NE
PPI <- Data$PPI_all_commodities
RSRI <- Data$Realtime_SahmRule_Recession_Indicators
Rtn <- Data$MthPrcRet
names(Data)

reg1 <- lm(Rtn~ FFER + TFO + HPI + UR + CI + CPI + PPI + RSRI, data = Data)
summary(reg1)
vif(reg1)
ncvTest(reg1)

# only UR and CI are significant variables.CPI and PPI have high multicollinearity

#1 apply AIC
library(leaps)
subsets <- regsubsets(Rtn~., data = as.data.frame(cbind(FFER, TFO, HPI, UR, CI, CPI, PPI, RSRI)), nbest=1)
summary(subsets)
# when using one variable, UR is the best. when using two variables, combination of UR and CI is the best.

#2 step AIC
library(MASS)
stepAIC(reg1) # try to find out the weakest variable
# AIC the lower the better. Final model has FFER, HPI, CI, UR as variables. This is consistent with the result of applying AIC, when there are four variables.

#3 apply cp
x1<- as.matrix(cbind(FFER, TFO, HPI, UR, CI, CPI, PPI, RSRI))
names_x1 <- c('FFER', 'TFO', 'HPI', 'UR', 'CI', 'CPI', 'PPI', 'RSRI')
leaps.fit <- leaps(y = Rtn, x = x1, names = names_x1, nbest = 1)
cbind(leaps.fit$which, Cp = leaps.fit$Cp)
# combination of 4 variables has smallest Cp, which is consistent with the result of stepAIC.

reg2 <- lm(Rtn~ FFER + HPI + UR + CI)
summary(reg2)

# Part 4 ------------------------------------------------------------------
library(lubridate)
head(Data)
months <- month(Data$MthCalDt)
Aug <- ifelse(months == 8,1,0)
Sep <- ifelse(months == 9,1,0)
Oct <- ifelse(months == 10,1,0)
Dum_reg <- lm(Rtn~ Aug + Sep + Oct + FFER + HPI + UR + CI)
summary(Dum_reg)
## 虚拟变量 Jan 的系数估计为-0.025205。它表示在一月（Jan=1）时，股票回报平均减少0.025205.
## Jan return is less, so for Coca Cola, January is not a good time to invest.








