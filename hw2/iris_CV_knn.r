# load required library
library(class)
library(ggplot2)

#################################################
# PREPROCESSING
#################################################

data <- iris                # create copy of iris dataframe

N <- nrow(data)          # total number of records (150)
n <- 6                  # Set n
n.size <- N/n            # Fold size
n.count <- 1

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
for (a in 1:n) {
     test.start <- (((a-1) * n.size) + 1)    # Start parameter for indexing folds
     test.end <- (a*n.size)                  # End parameter for indexing folds

     test.index <- test.start:test.end       # One of n folds

     train.data <- data[-test.index, ]       # Training/test split
     test.data <- data[test.index, ]

     train.labels <- as.factor(as.matrix(labels)[-test.index, ])      # extract training set labels
     test.labels <- as.factor(as.matrix(labels)[test.index, ])        # extract test set labels
     
     max.k <- 10
     for (k in 1:max.k)              # perform fit for various values of k
     {
         knn.fit <- knn(train = train.data,          # training set
                         test = test.data,           # test set
                         cl = train.labels,          # true labels
                         k = k                       # number of NN to poll
                    )

         
         print(table(test.labels, knn.fit))          # print confusion matrix

         this.err <- c(n.count, k, sum(test.labels != knn.fit) / length(test.labels))    # store gzn err
         err.rates <- rbind(err.rates, this.err)     # append err to total results
     }
     n.count <- n.count + 1
}



#################################################
# OUTPUT RESULTS
#################################################

names(err.rates) <- c('n', 'k', 'err.rate')        # label columns of err df

err.rates$nk <- paste(err.rates$n, err.rates$k, sep="-")
results <- data.frame(1:nrow(err.rates), err.rates$err.rate)     # create results summary data frame
names(results) <- c('nk.count', 'err.rate') 


#################################################
# VISUALIZATION - ggplot2
#################################################

# create title for results plot
title <- 'n Fold knn results'

# create results plot
results.plot <- ggplot(err.rates, aes(x=k, y=err.rate)) + geom_point() + geom_line() + facet_wrap(~n, nrow=3  )
results.plot <- results.plot + ggtitle(title)

# draw results plot (note need for print stmt inside script to draw ggplot)
print(results.plot)
