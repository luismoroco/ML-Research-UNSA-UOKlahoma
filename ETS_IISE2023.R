# Call the required libraries first

library(readxl)
library(GGally)
library(fpp3)
library(ggplot2)
library(tsibble)
library(corrplot)

#Import data to R 
Data <- read_excel("/Users/talayeh/Dropbox/Mac/Documents/UNSA/GrantCode/IISE forecasting/Arequipa.xlsx")

#select deaths
Data <- Data %>% select(c(deaths, date))

#create tsibble
NNdata <- Data %>%
  mutate(Daily = ymd(date)) %>%
  select(-date) %>%
  as_tsibble(index = Daily)

dcmp <- NNdata %>% 
  model(
    stl = STL(deaths ~ season(period = 1), robust = TRUE)) %>%
  components()

autoplot(dcmp)

#identify outliers
outliers <- dcmp %>%
  filter(
    remainder < quantile(remainder, .25) - 3*IQR(remainder) |
      remainder > quantile(remainder, .75) + 3*IQR(remainder))

#optional step: fill outliers with missing data imputation 
#miss <- NNdata %>%
#  anti_join(outliers) %>%
#  fill_gaps()
#fill <- miss %>%
#  model(ARIMA(deaths)) %>%
#  interpolate(miss)
#fill %>%
#  right_join(outliers %>% select(-deaths))

# Save cleaned data in NNdata
#NNdata <- fill

#Plot death data
autoplot(NNdata,deaths)

# Perform box-cox transformation
 lambda <- NNdata %>% features(deaths, features = guerrero) %>% pull(lambda_guerrero) 
# 0.4796161
 NNdata <- NNdata %>% box_cox(deaths, lambda)

#train and test split
train <- NNdata %>% filter(ymd(Daily) <= "2022-01-15")
test <- NNdata %>% filter(ymd(Daily) >= "2022-01-15")

#Create models
fittest <- train %>%
  model(
    'Mean' = MEAN(deaths),
    'Random Walk' = NAIVE(deaths),
    'Drift' = RW(deaths ~ drift()),
    'Automatic ETS' = ETS(deaths),
    #'Vanilla ETS' = ETS(deaths ~ error("A")+ trend("N") + season("N")),
    'Additive trend ETS' = ETS(deaths ~ error("A")+ trend("A")+ season("N")),
    'Damped trend ETS' = ETS(deaths ~ error("A")+ trend("Ad")+season("N")),
    'Holt Winters Additive' = ETS(deaths ~ error("A")+ trend("A")+season("A")),
    #'Holt_Winters Multiplicative' = ETS(deaths ~ error("M")+ trend("A")+season("M")),
    'ARIMA' = ARIMA(deaths),
    # You need to tune these parameters p, n_nodes, and n_networks for your data
    'Autoregressive NN'= NNETAR(deaths, p=2, n_nodes =5, n_networks = 5)
  )  %>%
  forecast(h = 21)

#Plot models for the year 2022
fittest %>%
  autoplot( NNdata %>% filter_index("2022-01-01" ~ .), size = 2, level = NULL) + 
  labs(x = "", y = "", title = "", font_size = 55)

#Print accuracy measures
accuracy(fittest,test)

#plot models against entire data set
autoplot(fittest,NNdata)

