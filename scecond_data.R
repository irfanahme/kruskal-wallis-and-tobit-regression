
# import data in libraries 
df <- read.csv('TOC DATA R.csv', TRUE, ',')
View(df)
# find not missing value in data 
which(complete.cases(df))
# find which missing value is present and remove from the data 

na_va <- which(!complete.cases(df))
df_clean <- df[-na_va,]
View(df_clean)
head(df_clean)
# number of columns 
cols_num <- c("Number.of.Employees","Route.km.operated","Passenger.km..millions.")
#change data type of the columns 
df_clean[cols_num] <- sapply(df_clean[cols_num],as.numeric)

# DEA analysis using Dear library
library(deaR)
data <- read_data(df_clean, inputs = 6:7, outputs = 8)
View(data)
Result_pft <- model_basic(data, dmu_eval = 1:54, dmu_ref = 1:54, rts = "crs", orientation = "io")
rts(Result_pft)
efficiencies(Result_pft) 
rts(Result_pft)
summary(df_clean)
# check data type in data
sapply(df_clean, class)
library(ggplot2)
f <- function(x, var, bw = 15) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

p <- ggplot(df_clean, aes(x = Passenger.Journeys..millions., fill=Number.of.Stations.Managed))

# histogram, coloured by proportion in different programs
# with a normal distribution overlayed
p + stat_bin(binwidth=15) +
  stat_function(fun = f, size = 1,
                args = list(var = df_clean$Passenger.Train.km..millions.))
# find correlation between
library(dplyr)
group_by(df_clean, Ã.Â.Â.Train.Operating.Company) %>%
  summarise(
    count = n(),
    mean = mean(Passenger.Journeys..millions., na.rm = TRUE),
    sd = sd(Passenger.Journeys..millions., na.rm = TRUE),
    median = median(Passenger.Journeys..millions., na.rm = TRUE),
    IQR = IQR(Passenger.Train.km..millions., na.rm = TRUE)
  )
kruskal.test(Passenger.Journeys..millions. ~ Ã.Â.Â.Train.Operating.Company, data = df_clean)
# find correlation between
cor(df_clean[, c("Passenger.Train.km..millions.","Passenger.km..millions.","Passenger.Journeys..millions.")])
# plot matrix

library(GGally)
ggpairs(df_clean[, c("Passenger.Train.km..millions.","Passenger.km..millions.","Passenger.Journeys..millions.")])
# Tobit regression in give input columns 
library(VGAM)
summary(m <- vglm(Passenger.Journeys..millions. ~ Passenger.Train.km..millions. + Passenger.km..millions., tobit(Upper = 800), data = df_clean))
ctable <- coef(summary(m))
pvals <- 2 * pt(abs(ctable[, "z value"]), df.residual(m), lower.tail = FALSE)
cbind(ctable, pvals)
b <- coef(m)
se <- sqrt(diag(vcov(m)))

cbind(LL = b - qnorm(0.975) * se, UL = b + qnorm(0.975) * se)
df_clean$yhat <- fitted(m)[,1]
df_clean$rr <- resid(m, type = "response")
df_clean$rp <- resid(m, type = "pearson")[,1]
par(mfcol = c(2, 3))
with(df_clean, {
  plot(yhat, rr, main = "Fitted vs Residuals")
  qqnorm(rr)
  plot(yhat, rp, main = "Fitted vs Pearson Residuals")
  qqnorm(rp)
  plot(Passenger.Train.km..millions., rp, main = "Actual vs Pearson Residuals")
  plot(Passenger.Train.km..millions., yhat, main = "Actual vs Fitted")
})
(r <- with(df_clean, cor(yhat, Passenger.Train.km..millions.)))
# variance accounted for
r^2
