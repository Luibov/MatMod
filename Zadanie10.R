tbl = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1")
tbl
class(tbl)
tbl = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1", skip =1)
tbl
tbl = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1", skip = 1, comment=c("["))
tbl
tbl = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
tbl = tbl[-1,]
tbl
glimpse(tbl)
tbl = select(tbl, -(roll))
tbl = tbl %>% mutate_if(is.character, factor)
names(tbl) =  str_replace_all(names(tbl), "[!]","_emph_")
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(tbl)
sapply(tbl,is.numeric)
tbl_numeric = tbl[,sapply(tbl,is.numeric) ]
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ]
cor_td = cor(tbl_numeric)
cor_td
cor_td = cor(drop_na(tbl_numeric))
cor_td
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux)
cor_td
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
cor_td = cor(drop_na(tbl_numeric)) 
tbl_numeric = na.omit(tbl_numeric) 
cor_td 
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux) 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude 
cor_td 
tbl_numeric[, c(20,21,22,86,87,88,89,90,91,92,93,94)] = NULL 
tbl_numeric 
cor_td = cor(tbl_numeric$co2_flux, tbl_numeric) 
cor_td 
class(cor_td) 
cor_td = as.data.frame(cor_td) 
cor_td 
cor_td$co2_time_lag = NULL 
cor.names = names(cor_td[, abs(cor_td) > 0.4]) 
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep="")) 
formula 
teaching_tbl = sample_n(tbl, floor(length(tbl$date)*.7)) 
testing_tbl = sample_n(tbl, floor(length(tbl$date)*.3)) 
row_numbers = 1:length(tbl$date) 
teach = sample(row_numbers, floor(length(tbl$date)*.7)) 
test = row_numbers[-teach] 
teaching_tbl_unq = tbl[teach,] 
testing_tbl_unq = tbl[test,]
sdata = read.csv("https://www.dropbox.com/s/lx9celfieswn4vq/NatalRisk.csv?dl=1")
head(sdata)
train <- sdata[sdata$ORIGRANDGROUP<=5,]
test <- sdata[sdata$ORIGRANDGROUP>5,]
complications <- c("ULD_MECO","ULD_PRECIP","ULD_BREECH")
riskfactors <- c("URF_DIAB", "URF_CHYPER", "URF_PHYPER",
                 "URF_ECLAM")
y <- "atRisk"
x <- c("PWGT",
       "UPREVIS",
       "CIG_REC",
       "GESTREC3",
       "DPLURAL",
       complications,
       riskfactors)
fmla <- paste(y, paste(x, collapse="+"), sep="~")
model <- glm(fmla, data=train, family=binomial(link="logit"))
train$pred <- predict(model, newdata=train, type="response")
test$pred <- predict(model, newdata=test, type="response")
library(ggplot2)
ggplot(train, aes(x=pred, color=atRisk, linetype=atRisk)) + geom_density()
library(ROCR)
library(grid) # Load grid library (you’ll need this  for the nplot function below).
predObj <- prediction(train$pred, train$atRisk) # Create ROCR prediction object
precObj <- performance(predObj, measure="prec") # Create ROCR object to calculate precision as a function of threshold
recObj <- performance(predObj, measure="rec")# Create ROCR object to calculate recall as a function of threshold.
precision <- (precObj@y.values)[[1]] # ROCR objects are what R calls S4 objects; the slots (or fields) of an S4 object are stored as lists within the object. You extract the slots from an S4 object using @ notation. 
prec.x <- (precObj@x.values)[[1]] # The x values (thresholds) are the same in both predObj and recObj, so you only need to extract them once.
recall <- (recObj@y.values)[[1]]
rocFrame <- data.frame(threshold=prec.x, precision=precision, recall = recall) # Build data frame with thresholds, precision, and recall
nplot <- function(plist) { #Function to plot multiple plots on one page (stacked).
  n <- length(plist)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(n,1)))
  vplayout=function(x,y) {viewport(layout.pos.row=x, layout.pos.col=y)}
  for(i in 1:n) {
    print(plist[[i]], vp=vplayout(i,1))
  }
}
pnull <- mean(as.numeric(train$atRisk)) # Calculate rate of at-risk births in the training set.
p1 <- ggplot(rocFrame, aes(x=threshold)) + # Plot enrichment rate as a function of threshold.
  geom_line(aes(y=precision/pnull)) +
  coord_cartesian(xlim = c(0,0.05), ylim=c(0,10) )
p2 <- ggplot(rocFrame, aes(x=threshold)) + # Plot recall as a function of threshold
  geom_line(aes(y=recall)) +
  coord_cartesian(xlim = c(0,0.05) )
nplot(list(p1, p2)) #Show both plots simultaneously.
ctab.test <- table(pred=test$pred>0.02, atRisk=test$atRisk)
ctab.test
precision <- ctab.test[2,2]/sum(ctab.test[2,])
precision
recall <- ctab.test[2,2]/sum(ctab.test[,2])
recall
enrich <- precision/mean(as.numeric(test$atRisk))
enrich
coefficients(model)
summary(model)
pred <- predict(model, newdata=train, type="response")
llcomponents <- function(y, py) { y*log(py) + (1-y)*log(1-py)} # Function to return the log likelihoods for each data point. Argument y is the true outcome
# (as a numeric variable, 0/1); argument py is the predicted probability.
edev <- sign(as.numeric(train$atRisk) - pred) * sqrt(-2*llcomponents(as.numeric(train$atRisk), pred)) # Calculate deviance result
summary(edev)
loglikelihood <- function(y, py) {
  sum(y * log(py) + (1-y)*log(1 - py))
}
# Function to calculate the log likelihood of a dataset. Variable y is the outcome in numeric form (1 for positive examples, 0 for negative). Variable py is
# the predicted probability that y==1.
pnull <- mean(as.numeric(train$atRisk))
pnull
null.dev <- -2*loglikelihood(as.numeric(train$atRisk), pnull)
null.dev
model$null.deviance
pred <- predict(model, newdata=train, type="response") #Predict probabilities for training data.
resid.dev <- 2*loglikelihood(as.numeric(train$atRisk), pred) # Calculate deviance of model for training data
resid.dev
model$deviance # For training data, model deviance is stored in the slot model$deviance
testy <- as.numeric(test$atRisk) # Calculate null deviance  and residual deviance for test data.
testpred <- predict(model, newdata=test,
                    type="response")
pnull.test <- mean(testy)
null.dev.test <- -2*loglikelihood(testy, pnull.test)
resid.dev.test <- -2*loglikelihood(testy, testpred)
pnull.test
null.dev.test
resid.dev.test
df.null <- dim(train)[[1]] - 1 # Null model has (number of data points - 1) degrees of freedom.
df.model <- dim(train)[[1]] - length(model$coefficients) # Fitted model has (number of data points - number of coefficients) degrees of freedom.
df.null
df.model
delDev <- null.dev - resid.dev
deldf <- df.null - df.model # Compute difference in deviances and difference in degrees of freedom.
p <- pchisq(delDev, deldf, lower.tail=F) # Estimate probability of seeing the observed difference in deviances under null model (the p-value)
# using chi-squared distribution.
delDev
deldf
p
pr2 <- 1-(resid.dev/null.dev)
pr2.test <- 1-(resid.dev.test/null.dev.test)
print(pr2.test)
