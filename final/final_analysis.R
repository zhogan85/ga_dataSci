# Load training data
train_data10 <- read.csv("train10.csv", header=TRUE)
train_data11 <- read.csv("train11.csv", header=TRUE)

# Load 2012 season stats as test data
test_data <- read.csv("test.csv", header=TRUE)

# TODO # change Long to numeric
train_fit <- lm(yards_2011 ~ att + att.game + yards + avg + yards.game + TD + long + fumbles, data=train_data11)

# Try backwards elimination, doesn't affect the output very much
# We could try to continue to eliminate features, but the remaining
# features all seem important. For instance, next in line to be 
# eliminated based on lowest magnitude t-values would be attempts, which
# should definitely have some predictive power when it comes to predicting
# total yards. 
fit2 <- update(fit, .~. -avg)
summary(fit2)

# We will stick with our first fit
# We will first apply our model to the 2011 season data, and see how 
# accurate it is



# To compare the predict
train_data11["error"] <- train_data11["yards_2012"] - train_data11["predict_yds"]


# The following will apply our linear regression model from our training
# set to our test set, creating a new column with yardage predictions
test_data["predict_yds"] = predict(train_fit, test_data)
write.table(test_data,file="Predictions2012.csv",sep=",",row.names=F)
