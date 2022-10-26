library("MTS")

data <- read.csv("Data.csv")

#US_index <- cbind(Data1[,], Data2[, c(4, 5, 6, 7, 10, 11, 16, 17)])
#UK_index <- Data2[, c(3, 9, 14, 15, 19, 20)]
#SG_index <- Data2[, c(2, 8, 12, 13, 18)]
df.US <- cbind(data$SPX,data$UST.10.2.Spread,data$US.Federal.Funds.Effective.Rate..continuous.series.,data$M1.Index,data$US.Condition.of.All.Federal.Reserve.Banks.Total.Assets..,data$CDX.IG.CDSI.GEN.5Y.Corp,data$CBOE.Equity.Put.Call.Ratio,data$VIX,data$Brent,data$US.CPI.MoM,data$US.ISM.Manufacturing.PMI,data$US.Service.PMI)
df.UK <- cbind(data$FTSE.100,data$VIX,data$Brent,data$UK.2yr.swap.rate,data$UK.MoM.CPI,data$UK.Manufacturing.PMI,data$UK.Service.PMI)

