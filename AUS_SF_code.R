install.packages(c("boot", "ftsa"))
require(boot)
require(ftsa)
require(ie2misc)
require(xtable)
require(philentropy)
require(abind)
require(demography)

# read Australian life-table death counts

year = 1921:2020
age = 0:110
n_year = length(year)
n_age = length(age)

AUS_female_qx = matrix(read.table("AUS_female_lifetable.txt", 
                                  header = TRUE)[,4], n_age, n_year, byrow = FALSE)
AUS_male_qx = matrix(read.table("AUS_male_lifetable.txt", 
                                header = TRUE)[,4], n_age, n_year, byrow = FALSE)
colnames(AUS_female_qx) = colnames(AUS_male_qx) = year

###########################
# compute survival function
###########################

SF_female = SF_male = matrix(NA, n_age, n_year)
for(ij in 1:n_year)
{
    for(ik in 1:n_age)
    {
        SF_female[,ij] = cumprod(1 - AUS_female_qx[,ij])
        SF_male[,ij] = cumprod(1 - AUS_male_qx[,ij])
    }
}
rm(ij); rm(ik)
colnames(SF_female) = colnames(SF_male) = as.character(year)
rownames(SF_female) = rownames(SF_male) = as.character(age)

# take the logit transformation

SF_female_logit = logit(SF_female)
SF_male_logit = logit(SF_male)

colnames(SF_female_logit) = colnames(SF_male_logit) = as.character(year)
rownames(SF_female_logit) = rownames(SF_male_logit) = as.character(age)

###############
# Back testing
###############

training_SF_female <- SF_female[66:110,1:80]
testing_SF_female <- rbind(SF_female[66:110,81:100],rep(0, 20))
training_SF_male <- SF_male[66:110,1:80]
testing_SF_male <- rbind(SF_male[66:110,81:100],rep(0, 20))

# take the logit transformation

training_years = 1921:2000
testing_years = 2001:2020

training_SF_female_logit = logit(training_SF_female)
colnames(training_SF_female_logit) = training_years

training_SF_male_logit = logit(training_SF_male)
colnames(training_SF_male_logit) = training_years

# model the transformed training data FPCR (20-years-ahead to compare with testing data)

fh = 20
fore_training_SF_female_FPCR = forecast(ftsm(fts(age[66:(n_age - 1)], 
                                        training_SF_female_logit),order = 6), h = fh)
fore_training_SF_male_FPCR = forecast(ftsm(fts(age[66:(n_age - 1)], 
                                        training_SF_male_logit), order = 6), h = fh)

training_SF_female_FPCR_back_transform = rbind(inv.logit(fore_training_SF_female_FPCR$mean$y), rep(0,fh))
training_SF_male_FPCR_back_transform = rbind(inv.logit(fore_training_SF_male_FPCR$mean$y), rep(0,fh))
colnames(training_SF_female_FPCR_back_transform) = testing_years
colnames(training_SF_male_FPCR_back_transform) = testing_years

# model the transformed training data LC method (20-years-ahead to compare with testing data)

fh = 20
fore_training_SF_female_LC = forecast(ftsm(fts(age[66:(n_age - 1)], 
                                      training_SF_female_logit), order = 1), h = fh)
fore_training_SF_male_LC = forecast(ftsm(fts(age[66:(n_age - 1)], 
                                      training_SF_male_logit), order = 1), h = fh)

training_SF_female_LC_back_transform = rbind(inv.logit(fore_training_SF_female_LC$mean$y), rep(0,fh))
training_SF_male_LC_back_transform = rbind(inv.logit(fore_training_SF_male_LC$mean$y), rep(0,fh))
colnames(training_SF_female_LC_back_transform) = testing_years
colnames(training_SF_male_LC_back_transform) = testing_years

##############################
# Perform MAFE and MAPE test
##############################

Calc_FPCR_female <- abs(testing_SF_female-training_SF_female_FPCR_back_transform)
n <- ncol(Calc_FPCR_female)
MAE_FPCR_female <- numeric(n)
for(i in 1:n)
{
    divisor <- n+1-i
    MAE_FPCR_female[i] <- sum(Calc_FPCR_female[, i]) / (divisor*46)
}
cat("Mean Absolute Error (MAE):", MAE_FPCR_female, "\n")
Calc_LC_female <- abs(testing_SF_female - training_SF_female_LC_back_transform)
n <- ncol(Calc_LC_female)
MAE_LC_female <- numeric(n)
for(i in 1:n)
{
    divisor <- n+1-i
    MAE_LC_female[i] <- sum(Calc_LC_female[, i])/(divisor * 46)
}
cat("Mean Absolute Error (MAE):", MAE_LC_female, "\n")

CalcMAPE_FPCR_female <- abs((testing_SF_female - training_SF_female_FPCR_back_transform)/testing_SF_female)
n <- ncol(CalcMAPE_FPCR_female)
MAPE_FPCR_female <- numeric(n)
for(i in 1:n)
{
    divisor <- n+1-i
    MAPE_FPCR_female[i] <- sum(CalcMAPE_FPCR_female[, i], na.rm = TRUE) * 100/(divisor * 46)
}
cat("Mean Absolute Percent Error (MAPE):", MAPE_FPCR_female, "\n")
CalcMAPE_LC_female <- abs((testing_SF_female - training_SF_female_LC_back_transform)/testing_SF_female)
n <- ncol(CalcMAPE_LC_female)
MAPE_LC_female <- numeric(n)
for(i in 1:n)
{
    divisor <- n+1-i
    MAPE_LC_female[i] <- sum(CalcMAPE_LC_female[, i], na.rm = TRUE) * 100/(divisor * 46)
}
cat("Mean Absolute Percent Error (MAPE):", MAPE_LC_female, "\n")

####################################################
# Perform MAFE and MAPE test (repeat tests for Male)
####################################################

Calc_FPCR_male<-abs(testing_SF_male-training_SF_male_FPCR_back_transform)
n<-ncol(Calc_FPCR_male)
MAE_FPCR_male <- numeric(n)
for (i in 1:n) {
  divisor <- n+1-i
  MAE_FPCR_male[i] <- sum(Calc_FPCR_male[, i]) / (divisor*46)
}
cat("Mean Absolute Error (MAE):", MAE_FPCR_male, "\n")
Calc_LC_male<-abs(testing_SF_male-training_SF_male_LC_back_transform)
n<-ncol(Calc_LC_male)
MAE_LC_male <- numeric(n)
for (i in 1:n) {
  divisor <- n+1-i
  MAE_LC_male[i] <- sum(Calc_LC_male[, i]) / (divisor*46)
}
cat("Mean Absolute Error (MAE):", MAE_LC_male, "\n")

CalcMAPE_FPCR_male<-abs((testing_SF_male-training_SF_male_FPCR_back_transform)/testing_SF_male)
n<-ncol(CalcMAPE_FPCR_male)
MAPE_FPCR_male <- numeric(n)
for (i in 1:n) {
  divisor <- n+1-i
  MAPE_FPCR_male[i] <- sum(CalcMAPE_FPCR_male[, i],na.rm=TRUE) *100 / (divisor*46)
}
cat("Mean Absolute Percent Error (MAPE):", MAPE_FPCR_male, "\n")
CalcMAPE_LC_male<-abs((testing_SF_male-training_SF_male_LC_back_transform)/testing_SF_male)
n<-ncol(CalcMAPE_LC_male)
MAPE_LC_male <- numeric(n)
for (i in 1:n) {
  divisor <- n+1-i
  MAPE_LC_male[i] <- sum(CalcMAPE_LC_male[, i],na.rm=TRUE) *100 / (divisor*46)
}
cat("Mean Absolute Percent Error (MAPE):", MAPE_LC_male, "\n")

#Making Latex table for MAFE and MAPE test

MATable<-data.frame(MAFE = c(mean(MAE_FPCR_female),mean(MAE_LC_female),mean(MAE_FPCR_male),mean(MAE_LC_male)),
                    MAPE = c(mean(MAPE_FPCR_female),mean(MAPE_LC_female),mean(MAPE_FPCR_male),mean(MAPE_LC_male)))
colnames(MATable) = c('MAFE','MAPE')
rownames(MATable) = c('Female FPCR method','Female LC method','Male FPCR method','Male LC method')
xtable(MATable, caption = 'Point forecast accuracy test result summary',digits = 4)

#######################################################################
# model the FPCR transformed data (46-years-ahead for annuity pricing)
#######################################################################

fhap = 46

fore_SF_female_FPCR = forecast(ftsm(fts(age[65:(n_age - 1)], SF_female_logit[65:110,]), 
                               order = 6), h = fhap)

fore_SF_male_FPCR = forecast(ftsm(fts(age[65:(n_age - 1)], SF_male_logit[65:110,]), 
                               order = 6), h = fhap)

# Construct point forecast and prediction intervals
set.seed(1)

SF_female_forecast <- rbind(inv.logit(forecast(ftsm(fts(age[65:(n_age - 1)], SF_female_logit[65:110,]), 
                                               order = 6), h = fhap)$mean$y), rep(0, fhap))

SF_female_forecast_lower<-rbind(inv.logit(forecast(ftsm(fts(age[65:(n_age - 1)], SF_female_logit[65:110,]), 
                                                   order = 6), h = 46,method = 'ets',level = 95, 
                                                   pimethod = 'nonparametric',B = 1000)$lower$y), 
                                                   rep(0, fhap))

SF_female_forecast_upper<-rbind(inv.logit(forecast(ftsm(fts(age[65:(n_age - 1)], SF_female_logit[65:110,]), 
                                                    order = 6), h = 46,method = 'ets',level = 95, 
                                                    pimethod = 'nonparametric',B = 1000)$upper$y), 
                                                    rep(0, fhap))

SF_male_forecast <- rbind(inv.logit(forecast(ftsm(fts(age[65:(n_age - 1)], SF_male_logit[65:110,]), 
                                             order = 6), h = fhap)$mean$y), rep(0, fhap))

SF_male_forecast_lower<-rbind(inv.logit(forecast(ftsm(fts(age[65:(n_age - 1)], SF_male_logit[65:110,]), 
                                                  order = 6), h = 46,method = 'ets',level = 95, 
                                                  pimethod = 'nonparametric',B = 1000)$lower$y), 
                                                  rep(0, fhap))
 
SF_male_forecast_upper<-rbind(inv.logit(forecast(ftsm(fts(age[65:(n_age - 1)], SF_male_logit[65:110,]), 
                                                  order = 6), h = 46,method = 'ets',level = 95, 
                                                  pimethod = 'nonparametric',B = 1000)$upper$y), 
                                                  rep(0, fhap))

colnames(SF_female_forecast) = colnames(SF_male_forecast) =2021:2066
colnames(SF_female_forecast_lower) = colnames(SF_male_forecast_lower) =2021:2066
colnames(SF_female_forecast_upper) = colnames(SF_male_forecast_upper) =2021:2066