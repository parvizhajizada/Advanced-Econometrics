# Loading the packages
library(moments)
library(car)
library(FENmlm)
library(stargazer)
library(ggplot2)
library(plm)
library(lfe)
library(bit64)
library(readxl)
library(gdata)
library(reshape2)
library(plyr)
library(dplyr)
library(gravity)
library(lmtest) 
library(tseries)
library(pqR)
library(ff)
library(data.table)
library(haven)
library(spatstat)
library(fastDummies)
library(MLmetrics)
library(kdensity)
library(overlap)
library(sm)
library(gtable)
library(ExPanDaR)
library(knitr)
library(tidyverse)
library(RColorBrewer)
library(corrplot)
library(caret)

## Introduction
# The aim of this project is to explain city-pair air traffic demand. 
# It is found that frequency of flights and fare are the most significant variables 
# and usage of fixed effects is crucial. OLS, FE, PPML models are used. 
# R2, MAPE, MAE, MedianAE, MedianAPE, RMSE are used as measures of prediction accuracy. 
# The datasets used covers airports from 174 countries over the time period of 2002-2017.


## Load data set
market <- read.csv("market.csv")[-1]
write.csv(market, "market.csv")
market <- read.csv("market.csv")


## Change class of variables 
# Convert into factors (year converted into factor intentionally)
facts <- c("year", "origin", "destination", "iso3_o", "iso3_d", "domestic", 
           "intercontinental", "dist200", "rta", "cu", "fta")
market[,facts] <- lapply(market[,facts], as.factor)

# COnvert into integers
nums <- c("passengers", "revenue_usd", "capacity", "frequency",
          "distance", "POP_o", "POP_d", "GDPCAP_o", "GDPCAP_d")
market[,nums] <- lapply(market[,nums], as.integer)


# EDA
glimpse(market)
head(market)
summary(market)
any(is.na(market))


# year against passengers
market %>%
  group_by(year) %>%
  summarise(yearly_passengers = sum(passengers)) %>%
  ggplot(aes(x = year, y = yearly_passengers)) +
  geom_col()

 
# it can be seen that number of passengers increased over years 


# year against revenue 
market %>%
  group_by(year) %>%
  summarise(yearly_revenue = sum(as.numeric(revenue_usd))) %>%
  ggplot(aes(x = year, y = yearly_revenue)) +
  geom_col()


# revenue does not have such a clear trend. Revenues droped during 2009 relative
# to 2008 probably because of financial crisis and then rises to very high levels
# during 2012-2014 because of high oil prices and later it drops alongside falling
# oil prices. Revenue to passenger ratio (i.e. average revenue) against year 
# shows the same trend also. 

# year against average revenue
market %>%
  group_by(year) %>%
  summarise(yearly_passengers = sum(passengers),
            yearly_revenue = sum(as.numeric(revenue_usd)),
            avg_revenue = yearly_revenue / yearly_passengers) %>%
  ggplot(aes(x = year, y = avg_revenue)) +
  geom_col()


# we can see that during 2009-2010 and 2015-2017 periods avg_revenue are 
# pretty low because of financial crisis and oil price 'crisis' respectively.


# passengers vs frequency 
market %>%
  ggplot(aes(x = log(frequency), y = log(passengers))) +
  geom_point() + 
  geom_smooth()
# as expected: rise in frequency of flights causes number of passengers to rise 

# passengers vs distance 
market %>%
  ggplot(aes(x = distance, y = passengers)) +
  geom_point() + 
  geom_smooth(method = "lm")
# unexpected: graph indicates almost no relationship between distance and passengers 

market %>%
  ggplot(aes(x = log(distance), y = log(passengers))) +
  geom_point() + 
  geom_smooth()

# still unexpected (even worse): positive relationship between log(distance) and
# log(passengers). It is against literature of gravity where the relationship found
# to be negative. I cannot explain this counterintuitive finding but I found it  
# important to include here


# avg_fare_per_km against passengers
market %>%
  ggplot(aes(x = log(avg_fare_per_km), y = log(passengers))) +
  geom_point() + 
  geom_smooth(method = "lm")
# negative relationship as expected

# gdp_o against passengers
market %>%
  ggplot(aes(x = log(GDPCAP_o), y = log(passengers))) +
  geom_point() + 
  geom_smooth(method = "lm")
# positive relationship as expected. As the citizens of origin city gets richer,
# they travel more and more


# make a data frame of dataset and group by year and dist200
# to see changes in number of passengers per year while distinguishing 
# between long and short distance travel. It is done becuase in case of
# short distance there are other means of transportations (i.e car, bus, train)
# that are substitutes to air travel. Basically more competition for short distance.
# we also need to account for distance itself because it is natural that avg_revenue 
# increase as the distance increases. So we introduce avg_revenue_per_km which is 
# equal to avg_revenue / distance. 

# group by year and dist200  
market %>%
  group_by(year, dist200) %>%
  summarise(avg_revenue_per_km = sum((passengers / revenue_usd) / distance)) %>%  
  ggplot(aes(x = year, y = avg_revenue, fill = dist200)) +
  geom_col(position = "dodge")
# dist200 == 1 means distance < 200. Just as we would expect, avg_revenue_per_km 
# is lower for short distance travel. In other words, those travels are carried 
# under low margins. The difference of avg_revenue_per_km between short and long 
# distance is also partially because of fixed costs of operating a flight.


# number of passengers grouped by year and whether the flight is domestic 
market %>%
  group_by(year, domestic) %>%
  summarise(yearly_passengers = sum(passengers)) %>%  
  ggplot(aes(x = year, y = yearly_passengers, fill = domestic)) +
  geom_col(position = "dodge")
# Turns out not most of the flights are domestic. domestic == 0 when the flight is 
# non domestic


# number of passengers grouped by year and whether the flight is intercontinental 
market %>%
  group_by(year, intercontinental) %>%
  summarise(yearly_passengers = sum(passengers)) %>%  
  ggplot(aes(x = year, y = yearly_passengers, fill = intercontinental)) +
  geom_col(position = "dodge")
# Looks like number of intercontinental and non intercontinental flights are similar 
# over year. There is only a little difference; a bit more non intercontinental flights.
# intercontinental == 0 when the flight is not intercontinental


# Correlations
correlations <- cor(market[nums])

corrplot(correlations, type="upper", order="hclust", 
         col=brewer.pal(n=8, name="RdBu"))



##############################################################################
################################ Aviation Model ##############################

## OLS (ordinary least squares)
OLS <- lm(data = market, log(passengers) ~ 
            log(avg_fare_per_km) + log(frequency))



## FE (ordinary least squares with fixed effects)
FE <- felm(data = market, log(passengers) ~ 
             log(avg_fare_per_km) + log(frequency)
           |
             year + origin + destination)


## PPML (poisson pseudo maximum likelihood with fixed effects)
PPML <- femlm(passengers ~ 
                log(avg_fare_per_km) + log(frequency)
              |
                year + origin + destination, market,
              family = "poisson")


## NBPML (negative binomial pseudo maximum likelihood with fixed effects)
NBPML <- femlm(passengers ~ 
                log(avg_fare_per_km) + log(frequency)
              |
                year + origin + destination, market,
              family = "negbin")

## GAUSSIAN (gaussian with fixed effects)
GAUSSIAN <- femlm(passengers ~ 
                log(avg_fare_per_km) + log(frequency)
              |
                year + origin + destination, market,
              family = "gaussian")


## Comparison of PPML, NBPML and GAUSSIAN
res2table(PPML, NBPML, GAUSSIAN,                   
          se = "twoway", 
          subtitles = c("PPML", "NBPML", "GAUSSIAN"))


## Error Measures
# MAPE (FE)
MAPE_OLS <- MAPE(y_pred = exp(OLS$fitted.values), y_true= market$passengers)
MAPE_FE <- MAPE(y_pred = exp(FE$fitted.values), y_true= market$passengers)
MAPE_PPML <- MAPE(y_pred = PPML$fitted.values, y_true= market$passengers)


# MedianAPE (PPML)
MedianAPE(y_pred = exp(OLS$fitted.values), y_true= market$passengers)
MedianAPE(y_pred = exp(FE$fitted.values), y_true= market$passengers)
MedianAPE(y_pred = PPML$fitted.values, y_true= market$passengers)


# MAE (PPML)
MAE(y_pred = exp(OLS$fitted.values), y_true= market$passengers)
MAE(y_pred = exp(FE$fitted.values), y_true= market$passengers)
MAE(y_pred = PPML$fitted.values, y_true= market$passengers)


# MedianAE (FE)
MedianAE(y_pred = exp(OLS$fitted.values), y_true= market$passengers)
MedianAE(y_pred = exp(FE$fitted.values), y_true= market$passengers)
MedianAE(y_pred = PPML$fitted.values, y_true= market$passengers)



# RMSE (PPML)
RMSE(y_pred = exp(OLS$fitted.values), y_true= market$passengers)
RMSE(y_pred = exp(FE$fitted.values), y_true= market$passengers)
RMSE(y_pred = PPML$fitted.values, y_true= market$passengers)


# Data Visualization
# Residual plot
w <- data.frame(x = log(PPML_1000$fitted.values), 
                y = log(market_1000$passengers) - log(PPML_1000$fitted.values))
names(w) <- c("PredictedlnPAX", "Residuals")

ggplot(data=w,aes(x = PredictedlnPAX, y = Residuals)) + 
  geom_point()


# Kernel  
q = data.frame(market_1000$passengers, exp(OLS_1000$fitted.values), 
               exp(FE_1000$fitted.values), PPML_1000$fitted.values) 
names(q) <- c("Observed","OLS", "FE", "PPML")
q.m <- melt(log(q))
names(q.m) <- c("Model", "lnPAX")
ggplot(q.m) +
  geom_density(aes(x = lnPAX, colour = Model)) +
  xlim(4,14) 


# Predicted vs Observed plot
r = data.frame(market_1000$passengers, exp(OLS_1000$fitted.values), 
               exp(FE_1000$fitted.values), PPML_1000$fitted.values)
names(r) <- c("Observed","OLS", "FE", "PPML")
r.m <- log(r) %>% melt(id.vars = "Observed", measure.vars = c("OLS", "FE", "PPML"))
names(r.m) <- c("Observed", "Model", "Predicted")
ggplot(r.m, aes(Observed, Predicted, colour = factor(Model))) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, color="black") + 
  facet_wrap(~ Model, ncol = 1, scales = "fixed") +
  guides(colour = "none") +
  theme()


# Summary Statistics 
summary_statistics <- (summary((market_1000)[,c("avg_fare_per_km", "frequency", "passengers")]))
write.csv(summary_statistics, "summary_statistics.csv")

# Spot Analysis
new.df <- market_1000 %>% filter(origin == "Atlanta")
predicted <- predict(PPML_1000, new.df)
example <- cbind(new.df[,c("year", "destination", "passengers")], predicted, new.df[,c("capacity")])
names(example) <- c("Year", "Destination", "Observed", "Predicted", "Capacity")
example <- (example %>% arrange(Year, Destination))[c(230:322),]
example$Predicted <- example$Predicted %>% as.integer()
View(example)
R2_Score(example$Predicted, example$Observed) 
write.csv(example, "example.csv")
###################################################################################


###########################################################################
#################### Geo-Economic Model ###################################
market1 <- read.csv("market.csv")[,-1]
market1$year <- market1$year %>% as.character()
market1$iso3_o <- market1$iso3_o %>% as.character()
market1$iso3_d <- market1$iso3_d %>% as.character()

RTA <- read_dta("rta_20181107.dta")[,c(1:6)] %>% 
  filter(year >= 2002) 
names(RTA) <- c("iso3_o", "iso3_d", "year", "rta", "cu", "fta")
RTA$year <- RTA$year %>% as.character()
RTA$iso3_o <- RTA$iso3_o %>% as.character()
RTA$iso3_d <- RTA$iso3_d %>% as.character()

GDPCAP <- read_xlsx("GDP per capita (current US$).xlsx") %>% 
  melt(id.vars="iso3") %>%
  na.omit()
POP <- read_xlsx("Population, total.xlsx") %>% 
  melt(id.vars="iso3") %>% 
  na.omit()

WBD <- join_all(list(GDPCAP, POP), by=c("iso3", "variable"))
names(WBD) <- c("iso3", "year", "GDPCAP", "POP") 
WBD <- WBD %>% filter(GDPCAP != "..",POP != "..") 
WBD$year <- WBD$year %>% as.character()
WBD$iso3 <- WBD$iso3 %>% as.character()

WBD_o <- WBD 
names(WBD_o) <-c("iso3_o", "year", "GDPCAP_o", "POP_o")

WBD_d <- WBD 
names(WBD_d) <-c("iso3_d", "year", "GDPCAP_d", "POP_d")

market1 <- merge(market1, WBD_o)
market1 <- left_join(market1, WBD_d)
market1 <- left_join(market1, RTA)

i <- c("year", "origin", "destination", "iso3_o", "iso3_d", "domestic", "intercontinental",
       "dist200", "rta", "cu", "fta")
j <- c("passengers", "revenue_usd", "capacity", "frequency",
       "distance", "POP_o", "POP_d", "GDPCAP_o", "GDPCAP_d")

market1[,i] <- lapply(market1[,i], as.factor)
market1[,j] <- lapply(market1[,j], as.integer)

market1 <- market1[complete.cases(market1),]
write.csv(market1, "market1.csv")
market1 <- read.csv("market1.csv")[,-1]

# Subset data
market_1000_1 <- market1 %>% filter(passengers >= 1000)


# OLS - full data set
OLS_ <- lm(data = market1, log(passengers) ~ 
             log(GDPCAP_o) + log(GDPCAP_d) +
             log(POP_o) + log(POP_d) +
             log(distance) + 
             rta + cu +  
             domestic)



# Fixed effects model - full data set
FE_ <- felm(data = market1, log(passengers) ~ 
              log(GDPCAP_o) + log(GDPCAP_d) +
              log(POP_o) + log(POP_d) +
              log(distance) + 
              rta + cu +  
              domestic 
            |
              year + origin + destination)


# PPML model - full data set
PPML_ <- femlm(passengers ~ 
                 log(GDPCAP_o) + log(GDPCAP_d) +
                 log(POP_o) + log(POP_d) +
                 log(distance) + 
                 rta + cu + 
                 domestic 
               |
                 year + origin + destination, market1,
               family = "poisson")


# OLS - market_1000
OLS_1000_ <- lm(data = market_1000_1,  log(passengers) ~ 
                  log(GDPCAP_o) + log(GDPCAP_d) +
                  log(POP_o) + log(POP_d) +
                  log(distance) +
                  rta + cu + 
                  domestic)


# Fixed effect model - market_1000
FE_1000_ <- felm(data = market_1000_1, log(passengers) ~ 
                   log(GDPCAP_o) + log(GDPCAP_d) +
                   log(POP_o) + log(POP_d) +
                   log(distance) +
                   rta + cu +
                   domestic 
                 |
                   year + origin + destination)


# PPML model - market_1000
PPML_1000_ <- femlm(passengers ~ 
                      log(GDPCAP_o) + log(GDPCAP_d) +
                      log(POP_o) + log(POP_d) +
                      log(distance) +
                      rta + cu +
                      domestic 
                    |
                      year + origin + destination, market_1000_1,
                    family = "poisson")




# Table of results of Fixed Effects model
OLS_and_FE_results_ <- stargazer(OLS_, FE_, OLS_1000_, FE_1000_,
                                 column.labels = c("OLS", "FE", "OLS1000", "FE1000"),
                                 type="html", 
                                 out=c("OLS_and_FE_results_.html", "OLS_and_FE_results_.txt",
                                       "OLS_and_FE_results_.tex"))


PPML_results_ <- res2table(PPML_, PPML_1000_, 
                           se = "twoway", 
                           subtitles = c("Poisson_", "Poisson_1000_"))


## Error Measures
# MAPE (FE)
MAPE(y_pred = exp(OLS_$fitted.values), y_true= market1$passengers)
MAPE(y_pred = exp(FE_$fitted.values), y_true= market1$passengers)
MAPE(y_pred = PPML_$fitted.values, y_true= market1$passengers)
MAPE(y_pred = exp(OLS_1000_$fitted.values), y_true= market_1000_1$passengers)
MAPE(y_pred = exp(FE_1000_$fitted.values), y_true= market_1000_1$passengers)
MAPE(y_pred = PPML_1000_$fitted.values, y_true= market_1000_1$passengers)

# MedianAPE (PPML)
MedianAPE(y_pred = exp(OLS_$fitted.values), y_true= market1$passengers)
MedianAPE(y_pred = exp(FE_$fitted.values), y_true= market1$passengers)
MedianAPE(y_pred = PPML_$fitted.values, y_true= market1$passengers)
MedianAPE(y_pred = exp(OLS_1000_$fitted.values), y_true= market_1000_1$passengers)
MedianAPE(y_pred = exp(FE_1000_$fitted.values), y_true= market_1000_1$passengers)
MedianAPE(y_pred = PPML_1000_$fitted.values, y_true= market_1000_1$passengers)

# MAE (PPML)
MAE(y_pred = exp(OLS_$fitted.values), y_true= market1$passengers)
MAE(y_pred = exp(FE_$fitted.values), y_true= market1$passengers)
MAE(y_pred = PPML_$fitted.values, y_true= market1$passengers)
MAE(y_pred = exp(OLS_1000_$fitted.values), y_true= market_1000_1$passengers)
MAE(y_pred = exp(FE_1000_$fitted.values), y_true= market_1000_1$passengers)
MAE(y_pred = PPML_1000_$fitted.values, y_true= market_1000_1$passengers)

# MedianAE (FE)
MedianAE(y_pred = exp(OLS_$fitted.values), y_true= market1$passengers)
MedianAE(y_pred = exp(FE_$fitted.values), y_true= market1$passengers)
MedianAE(y_pred = PPML_$fitted.values, y_true= market1$passengers)
MedianAE(y_pred = exp(OLS_1000_$fitted.values), y_true= market_1000_1$passengers)
MedianAE(y_pred = exp(FE_1000_$fitted.values), y_true= market_1000_1$passengers)
MedianAE(y_pred = PPML_1000_$fitted.values, y_true= market_1000_1$passengers)

# MSE (PPML)
MSE(y_pred = exp(OLS_$fitted.values), y_true= market1$passengers)
MSE(y_pred = exp(FE_$fitted.values), y_true= market1$passengers)
MSE(y_pred = PPML_$fitted.values, y_true= market1$passengers)
MSE(y_pred = exp(OLS_1000_$fitted.values), y_true= market_1000_1$passengers)
MSE(y_pred = exp(FE_1000_$fitted.values), y_true= market_1000_$passengers)
MSE(y_pred = PPML_1000_$fitted.values, y_true= market_1000_1$passengers)

# RMSE (PPML)
RMSE(y_pred = exp(OLS_$fitted.values), y_true= market1$passengers)
RMSE(y_pred = exp(FE_$fitted.values), y_true= market1$passengers)
RMSE(y_pred = PPML_$fitted.values, y_true= market1$passengers)
RMSE(y_pred = exp(OLS_1000_$fitted.values), y_true= market_1000_1$passengers)
RMSE(y_pred = exp(FE_1000_$fitted.values), y_true= market_1000_1$passengers)
RMSE(y_pred = PPML_1000_$fitted.values, y_true= market_1000_1$passengers)


# Data Visualization
# Residual plot
e <- data.frame(x = log(PPML_1000_$fitted.values), 
                y = log(market_1000_1$passengers) - log(PPML_1000_$fitted.values))
names(e) <- c("PredictedlnPAX", "Residuals")

ggplot(data=e,aes(x = PredictedlnPAX, y = Residuals)) + 
  geom_point()


# Kernel  
t = data.frame(market_1000_1$passengers, exp(OLS_1000_$fitted.values), 
               exp(FE_1000_$fitted.values), PPML_1000_$fitted.values) 
names(t) <- c("Observed","OLS", "FE", "PPML")
t.m <- melt(log(t))
names(t.m) <- c("Model", "lnPAX")
ggplot(t.m) +
  geom_density(aes(x = lnPAX, colour = Model)) +
  xlim(4,14) 


# Predicted vs Observed plot
y = data.frame(market_1000_1$passengers, exp(OLS_1000_$fitted.values), 
               exp(FE_1000_$fitted.values), PPML_1000_$fitted.values)
names(y) <- c("Observed","OLS", "FE", "PPML")
y.m <- log(y) %>% melt(id.vars = "Observed", measure.vars = c("OLS", "FE", "PPML"))
names(y.m) <- c("Observed", "Model", "Predicted")
ggplot(y.m, aes(Observed, Predicted, colour = factor(Model))) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, color="black") + 
  facet_wrap(~ Model, ncol = 1, scales = "fixed") +
  guides(colour = "none") +
  theme()


# Summary Statistics 
summary_statistics_ <- (summary((market_1000_1)[,c("GDPCAP_o", "GDPCAP_d",
                                                   "POP_o", "POP_d",
                                                   "distance", "passengers")]))
write.csv(summary_statistics, "summary_statistics_.csv")

# Spot Analysis
new.df <- market_1000 %>% filter(origin == "Atlanta")
predicted <- predict(PPML_1000, new.df)
example <- cbind(new.df[,c("year", "destination", "passengers")], predicted, new.df[,c("capacity")])
names(example) <- c("Year", "Destination", "Observed", "Predicted", "Capacity")
example <- (example %>% arrange(Year, Destination))[c(230:322),]
example$Predicted <- example$Predicted %>% as.integer()
View(example)
R2_Score(example$Predicted, example$Observed) 
write.csv(example, "example.csv")
################################################################################

