# STAT 610 Class Project
#--------------------------------------------------------------------------------------------------

# Optional: Load prior workspace
load("STAT 610 Class Project.RData")

# Install packages (disable after installing the first time)
# install.packages('tidyverse')
# install.packages('car')
# install.packages('cowplot')
# install.packages('corrplot')
library(tidyverse)
library(car)
library(cowplot)
library(corrplot)
#--------------------------------------------------------------------------------------------------
### Import Compass Maritime Data

# Assumes data file lives in same folder as R sript
ships <- read.csv("Compass Maritime Data_Add Info.csv",
                   header=TRUE, 
                   sep = ",",
                   stringsAsFactors=FALSE)

# The data file for this week contains attributes for ships.
# Value (and comparability) is determined by:
  # Ship type (e.g., capesize vs. Panamax)
  # Age (typical max of 25 years)
  # DWT (deadweight tons)
  # Index (capesize Index)
  # Condition
# Our goal is to figure out which attributes are most predictive of price, and 
# identify recommended price and negotiating strategy for the Bet Performer.


#--------------------------------------------------------------------------------------------------
### Data Cleaning

# Relabel headers for convenience
col_headings <- c('ID','SalesDate','Name','SalePrice','YearBuilt','AgeAtSale','DWT','Index',
                  'Speed','Draught','Tons','USD_Units','Units_USD','Length','Width')
names(ships) <- col_headings

# Remove ID (since it's included in data frame)
ships$ID <- NULL


# Fix dates
dates <- ships$SalesDate
betterDates <- as.Date(dates, origin = "1899-12-30")
ships$SalesDate <- betterDates

attach(ships)

# create Bet Performer record
BetPerformer <- data.frame(SalesDate=NA,
                           Name="Bet Performer",
                           SalePrice=NA,
                           YearBuilt=1997,
                           AgeAtSale=11,
                           DWT=172,
                           Index=12479,
                           Speed=9.6,
                           Draught=.54,
                           Tons=87120,
                           USD_Units=NA,
                           Units_USD=NA,
                           Length=272, 
                           Width=42.7)
# use final capesize index (12,479) (interesting note: the market crashed HARD after peak in 2008)
  # speed, draught, gross tons, length, width are averages of ships within 2,000 DWT of Bet Performer (170,174.9):
    # Ingenious, Cape Kassos, Nightflight, Formosabulk Brave, Formosabulk Clement, Sumihou, Cape Sun,
    # Gran Trader, Fertilia, Gran Trader, Jin Tai, Zorbas II, TMT TBN
  # I don't understand Units/Dollar / Dollar/Unit so leaving blank

#--------------------------------------------------------------------------------------------------
### Draw scatterplots of value vs. each attribute to visually ascertain relationships
Date.gg <- ggplot() + geom_point(data=ships, aes(x=SalePrice,y=SalesDate)) + coord_flip()
Age.gg <- ggplot() + geom_point(data=ships,aes(x=SalePrice,y=AgeAtSale)) + coord_flip()
DWT.gg <- ggplot() + geom_point(data=ships,aes(x=SalePrice,y=DWT)) + coord_flip()
Index.gg <- ggplot() + geom_point(data=ships,aes(x=SalePrice,y=Index)) + coord_flip()
Speed.gg <- ggplot() + geom_point(data=ships,aes(x=SalePrice,y=Speed)) + coord_flip()
Draught.gg <- ggplot() + geom_point(data=ships,aes(x=SalePrice,y=Draught)) + coord_flip()
Tons.gg <- ggplot() + geom_point(data=ships,aes(x=SalePrice,y=Tons)) + coord_flip()
USD_Units.gg <- ggplot() + geom_point(data=ships,aes(x=SalePrice,y=USD_Units)) + coord_flip()
Units_USD.gg <- ggplot() + geom_point(data=ships,aes(x=SalePrice,y=Units_USD)) + coord_flip()
Length.gg <- ggplot() + geom_point(data=ships,aes(x=SalePrice,y=Length)) + coord_flip()
Width.gg <- ggplot() + geom_point(data=ships,aes(x=SalePrice,y=Width)) + coord_flip()


plot_grid(Date.gg, Age.gg, USD_Units.gg, Units_USD.gg)
plot_grid(Index.gg, Length.gg, Width.gg, Draught.gg, Tons.gg, DWT.gg, Speed.gg)

# Age, DWT, Index, Speed look reasonably linear
# All except Age look like they'll have relatively high error
### look at plots to identify other variables with potential linear relationship

# probably not that useful, but looks pretty!
scatterplotMatrix( formula = ~ SalePrice + SalesDate + AgeAtSale + DWT + USD_Units + Units_USD,
                   data = ships, diagonal="histogram")
scatterplotMatrix( formula = ~ SalePrice + Index + Length + Width + Draught + Tons + DWT + Speed,
                   data = ships, diagonal="histogram")
# Note that there are smoothing lines and histograms on the diagonal now.

#--------------------------------------------------------------------------------------------------
### Identify relationships between attributes

# Correlations
ListCor <- ships %>%
  mutate(Date.cor = cor(SalePrice,as.numeric(SalesDate))) %>%
  mutate(Age.cor = cor(SalePrice,AgeAtSale)) %>%
  mutate(DWT.cor = cor(SalePrice,DWT)) %>%
  mutate(Index.cor = cor(SalePrice,Index)) %>%
  mutate(Speed.cor = cor(SalePrice,Speed)) %>%
  mutate(Draught.cor = cor(SalePrice,Draught)) %>%
  mutate(Tons.cor = cor(SalePrice,Tons)) %>%
  mutate(USD_Units.cor = cor(SalePrice,USD_Units, use="na.or.complete")) %>%
  mutate(Units_USD.cor = cor(SalePrice,Units_USD, use="na.or.complete")) %>%
  mutate(Length.cor = cor(SalePrice,Length)) %>%
  mutate(Width.cor = cor(SalePrice,Width)) %>%
  select(ends_with(".cor")) %>%
  sample_n(1) %>%
  remove_rownames()

ListCor
abs(ListCor)
### SalePrice * Age has correlation closest to 1, meaning it is has best correlation with value.  
  # The correlation is in a negative direction, meaning that as age increases, value decreases.
### DWT and Draught are second most closely correlated with SalePrice


### run correlation matrix of all combinations of independent variables to avoid collinear variables
# correlation matrix
cors <- ships %>%
  select(AgeAtSale,DWT,Index,Speed,Draught,Tons,USD_Units,Units_USD,Length,Width) %>%
  cor(use="na.or.complete")

corrplot(cors, order="FPC")
corrplot.mixed(cors, upper="circle",
               lower = "number", number.cex=.7,
               order="FPC")
### Don't combine size attributes in mixed regression model (length,width,dwt,tons) - strong correlations
### Don't combine Age & Draught
### Caution with Age & DWT

#--------------------------------------------------------------------------------------------------
### Linear regressions
  # lm(Y ~ X) --> explain Y based on X

## Ordered by Adjusted R2 ##

# Age is the best single predictor
Age.lm <- lm(SalePrice ~ AgeAtSale, data=ships) 
  summary(Age.lm)
    # Multiple R-squared:  0.6201,	Adjusted R-squared:  0.6119 
    # F-statistic:  75.1 on 1 and 46 DF,  p-value: 3.147e-11
  confint(Age.lm)
    # (Intercept) 117.865401 148.393663
    # AgeAtSale    -5.195926  -3.237118

  
DWT.lm <- lm(SalePrice ~ DWT, data=ships) 
  summary(DWT.lm)
    # Multiple R-squared:  0.2652,	Adjusted R-squared:  0.2493 
    # F-statistic:  16.6 on 1 and 46 DF,  p-value: 0.0001802
  confint(DWT.lm)
    # (Intercept) -162.2878712 -6.042327
    # DWT            0.4999898  1.477182

  
Draught.lm <- lm(SalePrice ~ Draught, data=ships) 
  summary(Draught.lm)
    # Multiple R-squared:  0.2216,	Adjusted R-squared:  o.2047 
    # F-statistic: 13.1 on 1 and 46 DF,  p-value: 0.000733
  confint(Draught.lm)
    # (Intercept) 87.796029 132.994303
    # Draught     -7.515636  -2.143424

  
Tons.lm <- lm(SalePrice ~ Tons, data=ships)  
  summary(Tons.lm)
    # Multiple R-squared:  0.1629,	Adjusted R-squared:  0.1447 
    # F-statistic: 8.952 on 1 and 46 DF,  p-value: 0.004444
  confint(Tons.lm)
    # (Intercept) -7.324178e+01 45.197731430
    # Tons         3.497992e-04  0.001788057

  
Length.lm <- lm(SalePrice ~ Length, data=ships) 
  summary(Length.lm)
    # Multiple R-squared:  0.1348,	Adjusted R-squared:  0.116 
    # F-statistic: 7.169 on 1 and 46 DF,  p-value: 0.01025
  confint(Length.lm)
    # (Intercept) -146.2457440 42.4154502
    # Length         0.1134517  0.8006491
  
  
Date.lm <- lm(SalePrice ~ SalesDate, data=ships)
  summary(Date.lm)
    # Multiple R-squared:  0.1219,	Adjusted R-squared:  0.1028 
    # F-statistic: 6.385 on 1 and 46 DF,  p-value: 0.0151
  confint(Date.lm)
  # (Intercept) -1.864788e+03 -146.3825067
  # SalesDate    1.599162e-02    0.1412414
 
   
Index.lm <- lm(SalePrice ~ Index, data=ships) 
  summary(Index.lm)
    # Multiple R-squared:  0.1207,	Adjusted R-squared:  0.1016 
    # F-statistic: 6.315 on 1 and 46 DF,  p-value: 0.01554
  confint(Index.lm)
    # (Intercept) 6.168523736 66.693047976
    # Index       0.001011655  0.008545346

  
Speed.lm <- lm(SalePrice ~ Speed, data=ships) 
  summary(Speed.lm)
    # Multiple R-squared:  0.09496,	Adjusted R-squared:  0.07529 
    # F-statistic: 4.827 on 1 and 46 DF,  p-value: 0.0331
  confint(Speed.lm)
    # (Intercept) -61.7813377 67.79124
    # Speed         0.6402099 14.64204
  
  
Width.lm <- lm(SalePrice ~ Width, data=ships) 
  summary(Width.lm)
    # Multiple R-squared:  0.08944,	Adjusted R-squared:  0.06964 
    # F-statistic: 4.518 on 1 and 46 DF,  p-value: 0.03894
  confint(Width.lm)
    # (Intercept) -130.1378921 67.894745
    # Width          0.1277675  4.691859

#--------------------------------------------------------------------------------------------------
### Multiple Regression ###

# ALL variables
All.lm <- lm(SalePrice ~ SalesDate + AgeAtSale + DWT + Index + 
               Speed + Draught + Tons, 
             data=ships)
  summary(All.lm)    
    # Multiple R-squared:  0.9237,	Adjusted R-squared:  0.9103 
    # F-statistic: 69.15 on 7 and 40 DF,  p-value: < 2.2e-16
  confint(All.lm)
    # (Intercept) -6.721213e+03  1.125266e+04
    # SalesDate   -2.888504e-01  1.741020e-01
    # AgeAtSale   -5.140808e+00 -3.790798e+00
    # DWT          6.754116e-02  5.997624e-01
    # Index       -3.271942e-03  2.461735e-02
    # Speed       -1.881730e+00  2.916476e+00
    # Draught     -1.268743e+00  9.584867e-01
    # Tons        -5.274355e-04  2.064365e-04
  
  
# based on all *provided* variables
Provided.lm <- lm(SalePrice ~ SalesDate + AgeAtSale + DWT + Index, data=ships) 
  summary(Provided.lm)
    # Multiple R-squared:  0.9209,	Adjusted R-squared:  0.9136 
    # F-statistic:   125.2 on 4 and 43 DF,  p-value: < 2.2e-16
  confint(Provided.lm)
    # (Intercept) -6.445074e+03  1.113841e+04
    # SalesDate   -2.857917e-01  1.671670e-01
    # AgeAtSale   -5.070188e+00 -4.005928e+00
    # DWT          5.895550e-02  4.325575e-01
    # Index       -2.865922e-03  2.438906e-02
  

AgeDWTIndex.lm <- lm(SalePrice ~ AgeAtSale + DWT + Index, data=ships) 
  summary(AgeDWTIndex.lm)
    # Multiple R-squared:  0.9204,	Adjusted R-squared:  0.915
    # F-statistic:   169.7 on 3 and 44 DF,  p-value: < 2.2e-16
  confint(AgeDWTIndex.lm)
    # (Intercept) 11.207351109 77.243748857
    # AgeAtSale   -5.070760585 -4.016847259
    # DWT          0.057514750  0.426794496
    # Index        0.006001613  0.008412235

  
# based on standard variables (Age, DWT) 
  ### Index is typically used for a different analysis type
AgeDWT.lm <- lm(SalePrice ~ AgeAtSale + DWT, data=ships) 
  summary(AgeDWT.lm)
    # Multiple R-squared:  0.6578,	Adjusted R-squared:  0.6426
    # F-statistic:   43.26 on 2 and 45 DF,  p-value: < 3.311e-11

# based on 3 best predictive variables
Multiple4.lm <- lm(SalePrice ~ AgeAtSale + DWT + Draught, data=ships) 
summary(Multiple4.lm)
  # Multiple R-squared:  0.6609,	Adjusted R-squared:  0.6378 
  # F-statistic:   28.59 on 3 and 44 DF,  p-value: < 2.055e-10

# based on Age and Index
Multiple5.lm <- lm(SalePrice ~ AgeAtSale + Index, data=ships) 
summary(Multiple5.lm)
  # Multiple R-squared:  0.9078,	Adjusted R-squared:  0.9037
  # F-statistic:   221.5 on 2 and 45 DF,  p-value: < 2.2e-16

# based on Age, Index, Speed
Multiple6.lm <- lm(SalePrice ~ AgeAtSale + Index + Speed, data=ships) 
summary(Multiple6.lm)
  # Multiple R-squared:  0.908,	Adjusted R-squared:  0.9017 
  # F-statistic: 144.7 on 3 and 44 DF,  p-value: < 2.2e-16

## Plot data and lines
df <- broom::augment(Age.lm)
ggplot(df, aes(x = .fitted, y = .resid)) + geom_point()

plot(ships$AgeAtSale, ships$SalePrice)
lines(ships$AgeAtSale, predict(Age.lm), col="red",lty=1,lwd=3)
lines(ships$AgeAtSale, resid(Age.lm), col="pink",lty=1,lwd=1)

plot(ships$DWT, ships$SalePrice)
lines(ships$DWT, predict(DWT.lm), col="blue",lty=1,lwd=3)

plot(ships$Index, ships$SalePrice)
lines(ships$Index, predict(Index.lm), col="green",lty=1,lwd=3)





#--------------------------------------------------------------------------------------------------
source("predictor.R")

AgeVal <- predictor(Age.lm, as.numeric(BetPerformer$AgeAtSale))
AgeVal # $86.74779

# Dates are weird
# DateVal <- predictor(Date.lm, as.numeric(BetPerformer$SalesDate))
# DateVal

DWTVal <- predictor(DWT.lm, as.numeric(BetPerformer$DWT))
DWTVal # $85.87172

IndexVal <- predictor(Index.lm, as.numeric(BetPerformer$Index))
IndexVal # $96.06169

###using best multiple regression (Multiple2 or Multiple5 ???)
Multiple2Val <- predictor(Multiple2.lm, as.numeric(c(BetPerformer$AgeAtSale,
                                                 BetPerformer$DWT,
                                                 BetPerformer$Index)))
Multiple2Val # $125.8295

Multiple5Val <- predictor(Multiple5.lm, as.numeric(c(BetPerformer$AgeAtSale,
                                                 BetPerformer$Index)))
Multiple5Val # $124.8718

### Compare Models
anova(Multiple2.lm, Multiple5.lm)
### DWT contributes significantly to Multiple2.lm

AIC(model1, model2, model3...)  # smaller is better
BIC(model1, model2, model3...)  # smaller is better
 
### ---> notice that including the index INCREASES PRICE (because current index is very high!)
  
## What is error / Confidence Interval of prediction?  Since price also depends on condition (which
## we have no information about), we can suggest that the error is in fact caused by condition
## effecting sales price, and they should ask the lower end of the CI for a ship in poor condition
## and the upper for a ship in pristine condition

###Use ANOVA to determine whether models are different?

#--------------------------------------------------------------------------------------------------

### Remaining To-Dos:

### Calculate predicted values from each model, calculate CI for each model
  ## Bet Performer, an 11-year-old (built in 1997), 172,000 deadweight ton (DWT) capesize bulk carrier
    # “capesize” ships, which carried up to 200,000 DWT or more

### Do we want to try a non-linear model?
    # https://stackoverflow.com/questions/31124440/specifying-in-r-points-to-predict-using-lm-and-predict-with-interactions-and?rq=1
    # https://www.r-bloggers.com/first-steps-with-non-linear-regression-in-r/
  ## Plot data and lines
  ## Predicted Values?

### Do we want to attempt to find & include additional information? (Who? Due?)
  # Attempt to do "income approach" analysis by plotting points from exhibit 3?

### 5-page paper (Who? Due?)
### Presentation (slides) (Who? Due?)

#--------------------------------------------------------------------------------------------------
### Standard EOF
# Detach ships from search path!
detach()

