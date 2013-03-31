# load required library
library(class)
library(ggplot2)

#################################################
# PREPROCESSING
#################################################

data <- iris                # create copy of iris dataframe

N <- nrow(data)          # total number of records (150)
K <- 10                  # Set n
K.size <- N/K            # Fold size
K.count <- 1

set.seed(1)        # initialize random seed for consistency
                    # NOTE -- run for various seeds --> need for CV!

rand.index <- sample(1:N, N)   # Randomize data
data <- data[rand.index, ]

labels <- data$Species      # store labels
data$Species <- NULL        # remove labels from feature set (note: could
                            # alternatively use neg indices on column index in knn call)


#################################################
# APPLY MODEL
#################################################
err.rates <- data.frame()  
for (a in 1:K) {
     test.start <- (((a-1) * K.size) + 1)    # Start parameter for indexing folds
     test.end <- (a*K.size)                  # End parameter for indexing folds

     test.index <- test.start:test.end       # One of n folds

     train.data <- data[-test.index, ]       # Training/test split
     test.data <- data[test.index, ]

     train.labels <- as.factor(as.matrix(labels)[-test.index, ])      # extract training set labels
     test.labels <- as.factor(as.matrix(labels)[test.index, ])        # extract test set labels
     
     #err.rates <- data.frame()       # initialize results object

     max.k <- 20
     for (k in 1:max.k)              # perform fit for various values of k
     {
         knn.fit <- knn(train = train.data,          # training set
                         test = test.data,           # test set
                         cl = train.labels,          # true labels
                         k = k                       # number of NN to poll
                    )

         
         print(table(test.labels, knn.fit))          # print confusion matrix

         this.err <- c(K.count, k, sum(test.labels != knn.fit) / length(test.labels))    # store gzn err
         err.rates <- rbind(err.rates, this.err)     # append err to total results
     }
     K.count <- K.count + 1
}



#################################################
# OUTPUT RESULTS
#################################################

#results <- data.frame(1:max.k, err.rates)   # create results summary data frame
names(err.rates) <- c('K', 'k', 'err.rate')        # label columns of err df

head(err.rates)
tail(err.rates)

err.rates[11:20, ]


head(err.rates)
tail(err.rates)


#err.rates$Kk <- paste(err.rates$K, err.rates$k, sep="-")
#results <- data.frame(1:nrow(err.rates), err.rates$err.rate)     # create results summary data frame
#names(results) <- c('Kk.count', 'err.rate') 

#head(results)
#tail(results)

#################################################
# VISUALIZATION - ggplot2
#################################################

# create title for results plot
title <- 'K Fold knn results'

# create results plot
results.plot <- ggplot(err.rates, aes(x=k, y=err.rate)) + geom_point() + geom_line() + facet_wrap(~K, nrow=2, ncol=5)
results.plot <- results.plot + ggtitle(title)

# draw results plot (note need for print stmt inside script to draw ggplot)
print(results.plot)
