#Data Mining the Inside Airbnb San Francisco May 2015 Dataset"
##Predictive Modeling of Daily Rental Pricing in Airbnb's San Francisco Market Using Web User Interface Inputs
##Schinria Islam
##December 2, 2015
---

For this project, I chose to look at the "Inside Airbnb San Francisco" dataset, which provides data compiled from the Airbnb website for listings available for San Francisco, CA for the month of May 2015.

Inside Airbnb is an independent, non-commercial set of tools and data that allows users to explore how Airbnb is really being used in cities around the world.

By analyzing publicly available information about a city's Airbnb's listings, Inside Airbnb provides filters and key metrics so data scientists can see how Airbnb is being used to compete with the residential housing market.

This dataset contains 7,029 listings (rows) that were posted on Airbnb's San Francisco page. The raw dataset contains 92 columns that may be treated as predictor variables. 

A few things to note: This data was web scraped by Murray Cox, the creater of the Inside Airbnb project, who I consulted with directly in the interpretation of some variables in the dataset. The project was not commercially funded, and resources used to collect and analyze the data were self-funded, so very special thank you to Murray Cox for building this dataset, among others, for public use. More information on the dataset can be found at: http://insideairbnb.com/

Secondly, the datset consists of listing information that was extracted from questions within the user interface of the Airbnb website for listings and hosts. Thus, the variables are limited to those fields which correspond directly to the Airbnb electronic form filled out by users posting listings. The *host_id* and *calculated_host_listings_count* variables identify unique hosts and how many listings those hosts have in the dataset.

Lastly, a host may list multiple rooms in the same apartment, or multiple apartments or homes they have available. This is to say that each listing in the dataset does not necessarily correspond with unique hosts, so this is a limitation of the dataset's interpretive power on a unique-user basis.

```{r}
airbnbRaw <- read.csv("/Users/Schinria/Documents/SF_Airbnb_May_2015.csv")
dim(airbnbRaw) #determine the number of rows and columns
str(airbnbRaw) #overview of the data types
```

The dependent variable in this analysis is *price* which represents the daily purchase price of accommodations listed on the Airbnb website under the search for Francisco, CA. 

For each listing in our data set, I was interested in predicting *price* with respect to the following 17 inputs:

* id - (INTEGER) unique website listing identifier 

* neighbourhood_cleansed - (NOMINAL) name of listing's neighborhood in San Francisco, as cleaned/reorganized by data curators: "Bayview", "Bernal Heights", "Castro/Upper Market", "Chinatown", "Crocker Amazon", "Diamond Heights", "Downtown/Civic Center", "Excelsior", "Financial District", "Glen Park", "Golden Gate Park", "Haight Ashbury", "Inner Richmond", "Inner Sunset", "Lakeshore", "Marina", "Mission", "Nob Hill", "Noe Valley", "North Beach", "Ocean View", "Outer Mission", "Outer Richmond", "Outer Sunset", "Pacific Heights", "Parkside", "Potrero Hill", "Presidio", "Presidio Heights", "Russian Hill", "Seacliff", "South of Market", "Treasure Island/YBI", "Twin Peaks", "Visitacion Valley", "West of Twin Peaks", "Western Addition"

* zipcode - (INTEGER) zipcode of the listed residence

* property_type - (NOMINAL) type of listed residence: "Apartment", "Bed & Breakfast", "Boat", "Bungalow", "Cabin", "Camper/RV", "Castle", "Condominium", "Dorm", "House", "Island", "Loft", "Other", "Plane", "Tent", "Townhouse", "Treehouse", "Villa", "Yurt"

* room_type - (NOMINAL) type of room available in listed residence: "Entire home/apt", "Private room", "Shared room"

* accommodates - (NOMINAL) number of guests able to accomodated in listed residence

* bathrooms - (NUMERIC) number of bathrooms in listed residence

* bedrooms - (NUMERIC) number of bedrooms in listed residence

* beds - (NUMERIC) number of beds in listed residence

* bed_type - (NOMINAL) type of bed available in listed residence: "Airbed", "Couch", "Futon", "Pull-out Sofa", "Real Bed"

* cleaning_fee - (NUMERIC) the total fee charged per renter by the host of the listing; this fee is added to the price in the final reservation, so it is separate from the price variable

* minimum_nights - (NUMERIC) the minimum number of nights the renter must book for as required by the listing host
 
* availability_365 - (NUMERIC)  the number of days the residence is available to renters by the host within the 365 day span after the listing was posted

* number_of_reviews - (NUMERIC) the number of customer reviews that listed residence has received in the past

* review_scores_rating - (NUMERIC) the overall score given to the listing by past renters DOUBLE CHECK!

* review_scores_value - (NUMERIC) the review rating given on the dimension of "Value" of the listing residence by past renters

* calculated_host_listings_count - (NUMERIC) the number of total listings per the host of each unique listing within this dataset from May 2015

* price - (NUMERIC) the rental price of the listed property in dollars ($)

I first subsetted my dataset to the 18 variables of interest (predictor variablse and outcome variable listed above) to condense the dataset to the predictor values and outcome of interest.

Next I double checked that all integer values in the subsetted data were read into R as numeric values. I also cleaned the *cleaning_fee* variable in the dataset because its values were listed in the raw date file with "$" that were interpreted as factors rather than numeric values.

View(sub_with_missing)

```{r}
vars <- c("id","neighbourhood_cleansed","zipcode","property_type","room_type","accommodates","bathrooms","bedrooms","beds","bed_type","cleaning_fee","minimum_nights","availability_365","number_of_reviews","review_scores_rating","review_scores_value","calculated_host_listings_count","price") #all of the variables that will go into our model

sub_with_missing <- airbnbRaw[,vars] #subsetting the dataset into variables before removing missing variables

class(sub_with_missing$cleaning_fee) #check class type of cleaning fee variable because in raw file was written as "$---.00" for example

sub_with_missing$cleaning_fee<-as.numeric(sub('$','',as.character(sub_with_missing$cleaning_fee),fixed=TRUE)) #change cleaning fee from factor to numeric data type

sub_with_missing$cleaning_fee[is.na(sub_with_missing$cleaning_fee)] <- 0 #replace all missing values of cleaning fees with "0" to represent no cleaning fee charged by host

airbnb <- na.omit(sub_with_missing) #removing all rows in the subsetted dataset with missing values

airbnb$price <- as.numeric(airbnb$price)

ok <- complete.cases(airbnb) #double check that all remaining rows in our dataset have no missing values
sum(!ok) # how many rows in the dataset are not ok and have missing values

str(airbnb) #overview of the subsetted data types

summary(airbnb) #descriptive statistics overview
summary(airbnb$price) #descriptive statistics for price 
table(airbnb$price) #counts of listings per unique price
```

Next I focus my attention to the dependent variable in the dataset that we'd like to predict: price. I run a few functions to get a sense of how prices of listings in the Airbnb SF dataset look. 

```{r}
attach(airbnb)
plot(price, cleaning_fee, main="Daily Price of Airbnb Residence Listings in SF, May 2015", xlab="Price ($)", ylab="Cleaning Fee", pch=21)

#some exploratory analysis
pairs(~price+cleaning_fee+accommodates,data=airbnb)
```


The histogram and boxplot below shows us that the prices for rental listings are skewed to the right. 

```{r}
stopifnot(require("dplyr"))
stopifnot(require("lattice"))
library(dplyr)
library(lattice)
hist(airbnb$price,col="light blue",xlab="Price ($)",main="Daily Price of Airbnb Residence Listings in SF, May 2015", breaks=20)

bwplot(airbnb$price, xlab = "Daily Price of Airbnb Residence Listings in SF, May 2015")
```

The purpose of this project is to predict the daily charged price for listings in San Francisco's Airbnb network. Now that the dataset has been cleaned up, I can set up the training and testing data, which I will do using the caTools package.

```{r}
stopifnot(require(caTools))
set.seed(123456)
sample <- sample.split(airbnb, SplitRatio = .75)
training <- subset(airbnb, sample == TRUE) #75% of the dataset
testing <- subset(airbnb, sample == FALSE) #25% of the dataset
dim(training)
dim(testing)

training <- subset(training, training$property_type!="Island" & training$property_type!="Castle" & training$property_type!="Plane")#removing training rows with property types Island, Castle and Plane
testing <- subset(testing, testing$property_type!="Island" & testing$property_type!="Castle" & testing$property_type!="Plane")#removing testing rows with property types Island, Castle and Plane

table(training$property_type)
table(testing$property_type)
```

Above you will see that I removed rows of listings that had "Island", "Castle", or "Plane" listed as the property type. This was done because only one of each was contained in the airbnb subsetted data, and removing these 3 rows allows us to create equal levels in our training and testing data sets and proceed with forming and testing our predictions.

Next, I will estimate the model on the training data
```{r}
#simple linear model
ols <- lm(price ~ . + I(accommodates^2), data=training)
summary(ols)
```

First I ran a multiple linear regression on our training set. We see in the output that there are a number of statistically significant relationships between price and inputs in our dataset, net of all other factors; however having statistically insignificant relationships doesn't necessarily mean our variables won't be worthwhile predictors of of price. I included a squared-*accommodates* variable in the model because a non-linear relationship that appeared during my exploratory data analysis. However, our simple OLS model (with all inputs included) has an adjusted R-sq of 0.18, which means there are a number of important predictive factors not in this dataset. 

Next I'll use the step function to rank variations on the model by the AIC.

```{r}
ols_AIC <- step(ols, trace = FALSE)

setdiff(names(coef(ols)), names(coef(ols_AIC)))
```

The output above tells us that the step function dropped the predictors *id*, *zipcode*, *minimum_nights*, *availability_365*, *number_of_reviews*, and *review_scores_value*.

```{r}
summary(ols_AIC) #linear regression with dropped variables after step function
```

Next I find the average sum-of-squared error after using our model to predict in the testing data.

```{r}
yhat_AIC <- predict(ols_AIC, newdata = testing) #predicitng the new model in the testing data set
(SSE_AIC <- mean( (testing$price - yhat_AIC) ^ 2) ) #average sum of squared error for the new model
best_model <- ols_AIC
best_error <- SSE_AIC

#sum of squared residuals
bm <- model.matrix(best_model)
ssr <- function (beta){
  eta <- bm %*% beta
  p <- training$price
  return (sum(eta-p)^2)
}

opt <- optim(rep(0, ncol(bm)), fn = ssr, method = "BFGS")
cbind(coef(best_model),opt$par)
```

Next I'll do a Principal Components Regression to replace correlated predictors with an orthogonal approximation of the space they span.

```{r}
stopifnot(require(pls))
library(pls)
PCR <- pcr(price ~ . + I(accommodates^2), data=training, validation = "LOO")
y_hat_pcr <- predict(PCR, newdata = testing)

mean_sqer <- function(y_hat_pcr) {
  mean(testing$price - y_hat_pcr)^2
}

a <- apply(y_hat_pcr,3, mean_sqer)

#identifies the best PCA model
a[which.min(a)]
```

Our output tells us that a minimum of 58 components are needed to make our PCR prediction.

Next I use the gam function to find smooth nonlinear models for our continuous outcome (price).

```{r}
stopifnot(require("gam"))

gam_train <- gam(price ~ neighbourhood_cleansed + property_type + room_type + s(accommodates) + s(bathrooms) + s(bedrooms) + s(beds) + bed_type + s(cleaning_fee) + s(review_scores_rating) + (calculated_host_listings_count) + s(accommodates^2), data = training)

par(mfrow=c(2,2), las=1, mar=c(5,4,1,1)+.1)
summary(gam_train)

#Predicting the gam model in the training data
y_hat_gam <- predict(gam_train)
mean( (training$price - y_hat_gam) ^ 2 )

#Predicting the gam model in the testing data
y_hat_gam_training <- predict(gam_train, newdata = testing)
mean( (testing$price - y_hat_gam_training) ^ 2 )

#plotting the prediction
par(mfrow=c(1,2), mar = c(5,4,1,1) + .1, las = 1, pch = 20)
#In training data
plot(training$price, y_hat_gam, col = "red")
#In testing data
plot(testing$price, y_hat_gam_training, col = "blue")
```

#Predicting Prices in the 1st, 2nd, 3rd, and 4th Quartiles

Next, I'd like to make binary predictions using the 1st, 2nd, 3rd, and 4th quartiles of our *price* distribution. The summary earlier (and below) told us that when it comes to our distribution of prices in the dataset, the 1st quartile ends at $76.00, median/2nd quartile ends at $141.00, the 3rd quartile ends at $275.00, and the maximum/4th quartile ends at $444.00. These quartiles will now be used to make categorical predictions using a variety of techniques.

```{r}
summary(airbnb$price) #descriptive statistics for price
```

```{r}
#quartiles for binary predictions
airbnb$firstQ <- as.factor(airbnb$price<=76.0)
airbnb$secondQ <- as.factor(airbnb$price>76.0 & airbnb$price<=141.0)
airbnb$thirdQ <- as.factor(airbnb$price>141.0 & airbnb$price<=275.00)
airbnb$fourthQ <- as.factor(airbnb$price>275.00)
```

```{r}
set.seed(123456)
sample2 <- sample.split(airbnb, SplitRatio = .75)
training2 <- subset(airbnb, sample2 == TRUE) #75% of the dataset
testing2 <- subset(airbnb, sample2 == FALSE) #25% of the dataset
dim(training2)
dim(testing2)

training2 <- subset(training2, training2$property_type!="Island" & training2$property_type!="Castle" & training2$property_type!="Plane")#removing training rows with property types Island, Castle and Plane
testing2 <- subset(testing2, testing2$property_type!="Island" & testing2$property_type!="Castle" & testing2$property_type!="Plane")#removing testing rows with property types Island, Castle and Plane

table(training2$property_type)
table(testing2$property_type)
```

In the next section I will run a series of predictive models to predict whether or not prices will fall within the 1st, 2nd, 3rd or 4th quartiles ranges. I will also share with method made the best prediction in each bracket and share the percentage of the models' accuracies. In these models, I will use the variables that were retained after using the step() function in our OLS model earlier.

##4th Quartile Predictions

First I will run a logistic regression to predict whether or not prices fall in the 4th (highest) quartile of prices.

```{r}
#logistic regression

logit_4 <- glm(fourthQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, family = binomial(link = "logit"))

summary(logit_4)

y_hat_logit_4 <- predict(logit_4, newdata = testing2, type = "response")

table(testing2$fourthQ, as.integer(y_hat_logit_4 > 0.5))

correct_logit_4 <- mean((testing2$fourthQ == TRUE) == (y_hat_logit_4 > 0.5))

summary(correct_logit_4) #correct predictions using logistic regression in 4th quartile
```

Next I will use a single-tree approach because they are simple and useful for interpretation. 
```{r}
stopifnot(require("tree"))
library(tree)
set.seed(123456)
highprice_tree_4 <- tree(fourthQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2)

summary(highprice_tree_4)

new_highprice_tree_4 <- cv.tree(highprice_tree_4, FUN = prune.misclass)
new_highprice_tree_4$dev 

best_tree_model_4 <- prune.tree(highprice_tree_4, best = 4)
summary(best_tree_model_4)

predictions_tree_4 <- predict(best_tree_model_4, newdata = testing2, type = "class")
table(testing2$fourthQ, predictions_tree_4)

correct_tree_4 <- mean(testing2$fourthQ == predictions_tree_4)

summary(correct_tree_4) #correct predictions using tree method in 4th quartile
```

Next I'll use bagging, random forests, boosting, and BART methods. Each of these approaches produce multiple trees which are then combined to yield a single consensus prediction We will see that combining a large number of trees can often result in dramatic improvements in prediction accuracy, at the expense of some loss in interpretation. 

```{r}
#bagging
stopifnot(require(randomForest))
library(randomForest)
set.seed(123456)
bagged_4 <- randomForest(fourthQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, importance = TRUE)
bagged_4

pb_4 <- plot(bagged_4)
varImpPlot(bagged_4)

predictions_bagged_4 <- predict(bagged_4, newdata = testing2, type = "response")

table(testing2$fourthQ, predictions_bagged_4)

correct_bagged_4 <- mean(testing2$fourthQ == predictions_bagged_4)

summary(correct_bagged_4) #correct predictions using tree method in 4th quartile
```

Next I will use Random Forests to predict whether or not prices fall in the 4th quartile of prices.
```{r}
#random forests
rf_4 <- randomForest(fourthQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, importance = TRUE)
summary(rf_4)

predictions_rf_4 <- predict(rf_4, newdata = testing2, type = "response")

table(testing2$fourthQ, predictions_rf_4)

correct_rf_4 <- mean(testing2$fourthQ == predictions_rf_4)

summary(correct_rf_4) #correct predictions using random forests in 4th quartile
```

Next I will use Boosting methods.
```{r}
#boosting
stopifnot(require(gbm))

resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}

par(resetPar()) 

boosted_4 <- randomForest(fourthQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, interaction.depth = 4, shrinkage = 0.001, n.cores = parallel::detectCores())

summary(boosted_4)

predictions_boosted_4 <- predict(boosted_4, newdata = testing2, type = "response", n.trees = 100)

table(testing2$fourthQ, predictions_boosted_4)

correct_boosted_4 <- mean(testing2$fourthQ == predictions_boosted_4)

summary(correct_boosted_4) #correct predictions using boosting in 4th quartile
```

Lastly I will use Bayesian Additive Regression Trees, or the BART method.
```{r}
#bartmachine
stopifnot(require(bartMachine))
set_bart_machine_num_cores(parallel::detectCores())

bart_4 <- bartMachine(X = training2[,1:17], y = training2$fourthQ, mem_cache_for_speed = FALSE)

bart_4

predictions_bart_4 <- predict(bart_4, new_data = testing2[,1:17], type = "class")

table(testing2$fourthQ, predictions_bart_4)

correct_bart_4 <- mean(testing2$fourthQ == predictions_bart_4)

summary(correct_bart_4) #correct predictions using bart in 4th quartile
```

```{r}
#Proportions of correct predictions for Fourth Quartile prices
round(c(logit_4th = correct_logit_4, tree_4th = correct_tree_4, bart_4th = correct_bart_4, RF_4th = correct_rf_4, boost_4th = correct_boosted_4, bagged_4th = correct_bagged_4), 4)
```

##3rd Quartile Predictions

Next, I will run a logistic regression to predict whether or not prices fall in the 3rd (second highest) quartile of prices.

```{r}
#logistic regression

logit_3 <- glm(thirdQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, family = binomial(link = "logit"))

summary(logit_3)

y_hat_logit_3 <- predict(logit_3, newdata = testing2, type = "response")

table(testing2$thirdQ, as.integer(y_hat_logit_3 > 0.5))

correct_logit_3 <- mean((testing2$thirdQ == TRUE) == (y_hat_logit_3 > 0.5))

summary(correct_logit_3) #correct predictions using logistic regression in 3rd quartile
```

Next I will use a single-tree approach because they are simple and useful for interpretation. 
```{r}
stopifnot(require("tree"))
library(tree)
set.seed(123456)
highprice_tree_3 <- tree(thirdQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2)

summary(highprice_tree_3)

new_highprice_tree_3 <- cv.tree(highprice_tree_3, FUN = prune.misclass)
new_highprice_tree_3$dev 

best_tree_model_3 <- prune.tree(highprice_tree_3, best = 3)
summary(best_tree_model_3)

predictions_tree_3 <- predict(best_tree_model_3, newdata = testing2, type = "class")
table(testing2$thirdQ, predictions_tree_3)

correct_tree_3 <- mean(testing2$thirdQ == predictions_tree_3)

summary(correct_tree_3) #correct predictions using tree method in 3rd quartile
```

Next I'll again use bagging, random forests, boosting, and BART methods. Each of these approaches produce multiple trees which are then combined to yield a single consensus prediction We will see that combining a large number of trees can often result in dramatic improvements in prediction accuracy, at the expense of some loss in interpretation. 

```{r}
#bagging
stopifnot(require(randomForest))
library(randomForest)
set.seed(123456)
bagged_3 <- randomForest(thirdQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, importance = TRUE)
bagged_3

pb_3 <- plot(bagged_3)
varImpPlot(bagged_3)

predictions_bagged_3 <- predict(bagged_3, newdata = testing2, type = "response")

table(testing2$thirdQ, predictions_bagged_3)

correct_bagged_3 <- mean(testing2$thirdQ == predictions_bagged_3)

summary(correct_bagged_3) #correct predictions using tree method in 3rd quartile
```

Next I will use Random Forests to predict whether or not prices fall in the 3rd quartile of prices.
```{r}
#random forests
rf_3 <- randomForest(thirdQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, importance = TRUE)
summary(rf_3)

predictions_rf_3 <- predict(rf_3, newdata = testing2, type = "response")

table(testing2$thirdQ, predictions_rf_3)

correct_rf_3 <- mean(testing2$thirdQ == predictions_rf_3)

summary(correct_rf_3) #correct predictions using random forests in 3rd quartile
```

Next I will use Boosting methods.
```{r}
#boosting
stopifnot(require(gbm))

resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}

par(resetPar()) 

boosted_3 <- randomForest(thirdQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, interaction.depth = 4, shrinkage = 0.001, n.cores = parallel::detectCores())

summary(boosted_3)

predictions_boosted_3 <- predict(boosted_3, newdata = testing2, type = "response", n.trees = 100)

table(testing2$thirdQ, predictions_boosted_3)

correct_boosted_3 <- mean(testing2$thirdQ == predictions_boosted_3)

summary(correct_boosted_3) #correct predictions using boosting in 3rd quartile
```

Lastly I will use Bayesian Additive Regression Trees, or the BART method.
```{r}
#bartmachine
stopifnot(require(bartMachine))
set_bart_machine_num_cores(parallel::detectCores())

bart_3 <- bartMachine(X = training2[,1:17], y = training2$thirdQ, mem_cache_for_speed = FALSE)

bart_3

predictions_bart_3 <- predict(bart_3, new_data = testing2[,1:17], type = "class")

table(testing2$thirdQ, predictions_bart_3)

correct_bart_3 <- mean(testing2$thirdQ == predictions_bart_3)

summary(correct_bart_3) #correct predictions using bart in 3rd quartile
```

```{r}
#Proportions of correct predictions for Third Quartile prices
round(c(logit_3rd = correct_logit_3, tree_3rd = correct_tree_3, bart_3rd = correct_bart_3, RF_3rd = correct_rf_3, boost_3rd = correct_boosted_3, bagged_3rd = correct_bagged_3), 4)
```

##2nd Quartile Predictions

Next, I will run a logistic regression to predict whether or not prices fall in the 2nd (second lowest) quartile of prices.

```{r}
#logistic regression

logit_2 <- glm(secondQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, family = binomial(link = "logit"))

summary(logit_2)

y_hat_logit_2 <- predict(logit_2, newdata = testing2, type = "response")

table(testing2$secondQ, as.integer(y_hat_logit_2 > 0.5))

correct_logit_2 <- mean((testing2$secondQ == TRUE) == (y_hat_logit_2 > 0.5))

summary(correct_logit_2) #correct predictions using logistic regression in 2nd quartile
```

Next I will use a single-tree approach because they are simple and useful for interpretation. 
```{r}
stopifnot(require("tree"))
library(tree)
set.seed(123456)
highprice_tree_2 <- tree(secondQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2)

summary(highprice_tree_2)

new_highprice_tree_2 <- cv.tree(highprice_tree_2, FUN = prune.misclass)
new_highprice_tree_2$dev 

best_tree_model_2 <- prune.tree(highprice_tree_2, best = 4)
summary(best_tree_model_2)

predictions_tree_2 <- predict(best_tree_model_2, newdata = testing2, type = "class")
table(testing2$secondQ, predictions_tree_2)

correct_tree_2 <- mean(testing2$secondQ == predictions_tree_2)

summary(correct_tree_2) #correct predictions using tree method in 2nd quartile
```

Next I'll again use bagging, random forests, boosting, and BART methods. Each of these approaches produce multiple trees which are then combined to yield a single consensus prediction We will see that combining a large number of trees can often result in dramatic improvements in prediction accuracy, at the expense of some loss in interpretation. 

```{r}
#bagging
stopifnot(require(randomForest))
library(randomForest)
set.seed(123456)
bagged_2 <- randomForest(secondQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, importance = TRUE)
bagged_2

pb_2 <- plot(bagged_2)
varImpPlot(bagged_2)

predictions_bagged_2 <- predict(bagged_2, newdata = testing2, type = "response")

table(testing2$secondQ, predictions_bagged_2)

correct_bagged_2 <- mean(testing2$secondQ == predictions_bagged_2)

summary(correct_bagged_2) #correct predictions using tree method in 2nd quartile
```

Next I will use Random Forests to predict whether or not prices fall in the 3rd quartile of prices.
```{r}
#random forests
rf_2 <- randomForest(secondQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, importance = TRUE)
summary(rf_2)

predictions_rf_2 <- predict(rf_2, newdata = testing2, type = "response")

table(testing2$secondQ, predictions_rf_2)

correct_rf_2 <- mean(testing2$secondQ == predictions_rf_2)

summary(correct_rf_2) #correct predictions using random forests in 2nd quartile
```

Next I will use Boosting methods.
```{r}
#boosting
stopifnot(require(gbm))

resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}

par(resetPar()) 

boosted_2 <- randomForest(secondQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, interaction.depth = 4, shrinkage = 0.001, n.cores = parallel::detectCores())

summary(boosted_2)

predictions_boosted_2 <- predict(boosted_2, newdata = testing2, type = "response", n.trees = 100)

table(testing2$secondQ, predictions_boosted_2)

correct_boosted_2 <- mean(testing2$secondQ == predictions_boosted_2)

summary(correct_boosted_2) #correct predictions using boosting in 2nd quartile
```

Lastly I will use Bayesian Additive Regression Trees, or the BART method.
```{r}
#bartmachine
stopifnot(require(bartMachine))
set_bart_machine_num_cores(parallel::detectCores())

bart_2 <- bartMachine(X = training2[,1:17], y = training2$secondQ, mem_cache_for_speed = FALSE)

bart_2

predictions_bart_2 <- predict(bart_2, new_data = testing2[,1:17], type = "class")

table(testing2$secondQ, predictions_bart_2)

correct_bart_2 <- mean(testing2$secondQ == predictions_bart_2)

summary(correct_bart_2) #correct predictions using bart in 2nd quartile
```

```{r}
#Proportions of correct predictions for Second Quartile prices
round(c(logit_2nd = correct_logit_2, tree_2nd = correct_tree_2, bart_2nd = correct_bart_2, RF_2nd = correct_rf_2, boost_2nd = correct_boosted_2, bagged_2nd = correct_bagged_2), 4)
```

##1st Quartile Predictions

Lastly, I will run a logistic regression to predict whether or not prices fall in the 1st (lowest) quartile of prices.

```{r}
#logistic regression

logit_1 <- glm(firstQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, family = binomial(link = "logit"))

summary(logit_1)

y_hat_logit_1 <- predict(logit_1, newdata = testing2, type = "response")

table(testing2$firstQ, as.integer(y_hat_logit_1 > 0.5))

correct_logit_1 <- mean((testing2$firstQ == TRUE) == (y_hat_logit_1 > 0.5))

summary(correct_logit_1)#correct predictions using logistic regression in 1st quartile
```

Next I will use a single-tree approach because they are simple and useful for interpretation. 
```{r}
stopifnot(require("tree"))
library(tree)
set.seed(123456)
highprice_tree_1 <- tree(firstQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2)

summary(highprice_tree_1)

new_highprice_tree_1 <- cv.tree(highprice_tree_1, FUN = prune.misclass)
new_highprice_tree_1$dev 

best_tree_model_1 <- prune.tree(highprice_tree_1, best = 4)
summary(best_tree_model_1)

predictions_tree_1 <- predict(best_tree_model_1, newdata = testing2, type = "class")
table(testing2$firstQ, predictions_tree_1)

correct_tree_1 <- mean(testing2$firstQ == predictions_tree_1)

summary(correct_tree_1) #correct predictions using tree method in 1st quartile
```

Next I'll use bagging, random forests, boosting, and BART methods. Each of these approaches produce multiple trees which are then combined to yield a single consensus prediction We will see that combining a large number of trees can often result in dramatic improvements in prediction accuracy, at the expense of some loss in interpretation. 

```{r}
#bagging
stopifnot(require(randomForest))
library(randomForest)
set.seed(123456)
bagged_1 <- randomForest(firstQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, importance = TRUE)
bagged_1

pb_1 <- plot(bagged_1)
varImpPlot(bagged_1)

predictions_bagged_1 <- predict(bagged_1, newdata = testing2, type = "response")

table(testing2$firstQ, predictions_bagged_1)

correct_bagged_1 <- mean(testing2$firstQ == predictions_bagged_1)

summary(correct_bagged_1) #correct predictions using tree method
```

Next I will use Random Forests to predict whether or not prices fall in the 1st quartile of prices.
```{r}
#random forests
rf_1 <- randomForest(firstQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, importance = TRUE)
summary(rf_1)

predictions_rf_1 <- predict(rf_1, newdata = testing2, type = "response")

table(testing2$firstQ, predictions_rf_1)

correct_rf_1 <- mean(testing2$firstQ == predictions_rf_1)

summary(correct_rf_1) #correct predictions using random forests in 1st quartile
```

Next I will use Boosting methods.
```{r}
#boosting
stopifnot(require(gbm))

resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}

par(resetPar()) 

boosted_1 <- randomForest(firstQ ~ property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + cleaning_fee + review_scores_rating + calculated_host_listings_count + I(accommodates^2), data = training2, interaction.depth = 4, shrinkage = 0.001, n.cores = parallel::detectCores())

summary(boosted_1)

predictions_boosted_1 <- predict(boosted_1, newdata = testing2, type = "response", n.trees = 100)

table(testing2$firstQ, predictions_boosted_1)

correct_boosted_1 <- mean(testing2$firstQ == predictions_boosted_1)

summary(correct_boosted_1) #correct predictions using boosting in 1st quartile
```

Lastly I will use Bayesian Additive Regression Trees, or the BART method.
```{r}
#bartmachine
stopifnot(require(bartMachine))
set_bart_machine_num_cores(parallel::detectCores())

bart_1 <- bartMachine(X = training2[,1:17], y = training2$firstQ, mem_cache_for_speed = FALSE)

bart_1

predictions_bart_1 <- predict(bart_1, new_data = testing2[,1:17], type = "class")

table(testing2$firstQ, predictions_bart_1)

correct_bart_1 <- mean(testing2$firstQ == predictions_bart_1)

summary(correct_bart_1) #correct predictions using bart in 1st quartile
```

```{r}
#Proportions of correct predictions for 1st Quartile prices
round(c(logit_1st = correct_logit_1, tree_1st = correct_tree_1, bart_1st = correct_bart_1, RF_1st = correct_rf_1, boost_1st = correct_boosted_1, bagged_1st = correct_bagged_1), 4)
```

##Conclusion of Price Quartile Predictive Modeling 

```{r}
#Proportions of correct predictions for Fourth Quartile prices
round(c(logit_4th = correct_logit_4, tree_4th = correct_tree_4, bart_4th = correct_bart_4, RF_4th = correct_rf_4, boost_4th = correct_boosted_4, bagged_4th = correct_bagged_4), 4)

#Proportions of correct predictions for Third Quartile prices
round(c(logit_3rd = correct_logit_3, tree_3rd = correct_tree_3, bart_3rd = correct_bart_3, RF_3rd = correct_rf_3, boost_3rd = correct_boosted_3, bagged_3rd = correct_bagged_3), 4)

#Proportions of correct predictions for Second Quartile prices
round(c(logit_2nd = correct_logit_2, tree_2nd = correct_tree_2, bart_2nd = correct_bart_2, RF_2nd = correct_rf_2, boost_2nd = correct_boosted_2, bagged_2nd = correct_bagged_2), 4)

#Proportions of correct predictions for First Quartile prices
round(c(logit_1st = correct_logit_1, tree_1st = correct_tree_1, bart_1st = correct_bart_1, RF_1st = correct_rf_1, boost_1st = correct_boosted_1, bagged_1st = correct_bagged_1), 4)
```

The output above tells us how each of the models perform when predicting Airbnb daily rental prices in San Francisco that fall either in the first, second, third, or fourth quartiles of all prices listed. 

The models that predict at the highest accuracy in each quartile may be used--for example, to see which inputs are most impactful in predicting whether listings will fall in the cheapest category (1st quartile), or reversely, to see which inputs are most impactful in predicting whether listings will fall in the most expensive category (4th quartile).
