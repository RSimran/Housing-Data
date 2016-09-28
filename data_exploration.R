setwd("C:/Users/Simran/Desktop/KAGGLE/Housing")

library(data.table)
library(testthat)
library(gridExtra)
library(corrplot)
library(GGally)

train <- fread('./input/train.csv',colClasses=c('MiscFeature' = "character", 'PoolQC' = 'character', 'Alley' = 'character'))

####Do not peek or explore this data set####
test <- fread('./input/test.csv' ,colClasses=c('MiscFeature' = "character", 'PoolQC' = 'character', 'Alley' = 'character'))
###########

str(train)

cat_var <- names(train)[which(sapply(train, is.character))]
numeric_var <- names(train)[which(sapply(train, is.numeric))]


head(train)
colSums(sapply(train, is.na))
colSums(sapply(train[,.SD, .SDcols = cat_var], is.na))
colSums(sapply(train[,.SD, .SDcols = numeric_var], is.na))


summary(train[,.SD, .SDcols =numeric_var])

cat('Train has', dim(train)[1], 'rows and', dim(train)[2], 'columns.')
cat('Test has', dim(test)[1], 'rows and', dim(test)[2], ' columns.')

# The percentage of data missing in train.
sum(is.na(train)) / (nrow(train) *ncol(train))

# The percentage of data missing in test.
sum(is.na(test)) / (nrow(test) * ncol(test))

# The number of missing values for each column.
colSums(sapply(train, is.na))

# The number of missing values for categorical columns.

colSums(sapply(train[, .SD, .SDcols = cat_var], is.na))

# The numer of missing values for numberic columns
colSums(sapply(train[, .SD, .SDcols = numeric_var], is.na))

# Check for duplicated rows.

cat("The number of duplicated rows are", nrow(train) - nrow(unique(train)))

####Convert character to factors 

train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]

train_cat <- train[,.SD, .SDcols = cat_var]

plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x=element_text(size=8))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

train_cont <- train[,.SD,.SDcols = numeric_var]

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(colnames(data_in)[i]) + theme_light() + ggtitle(paste0( "Skewness", skewness(data_in[[i]], na.rm = TRUE)))
  return(p)
   
}




correlations <- cor(na.omit(train_cont[,-1, with = FALSE]))
corrplot(correlations, method="square")
correlations
correlations <- matrix_threshold(correlations, threshold = 0.2)

row_indic <- apply(correlations, function(x) sum(x != 0) > 1)



highcorr <- c(names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] > 0.5)], names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] < -0.5)])
 
data_corr <- train[,highcorr, with = FALSE]

plotCorr <- function(data_in, i){
  data <- data.frame(x = data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data, aes(x = x, y = SalePrice)) + geom_point(shape = 1) + geom_smooth(method = lm) + xlab(colnames(data_in)[i]) + theme_light()
  return(p)
}


doPlots(data_corr, fun = plotCorr, ii = 1:6)

library(scales)
ggplot(train, aes(x=SalePrice)) + geom_histogram(col = 'white') + theme_light() +scale_x_continuous(labels = comma)
summary(train[,.(SalePrice)])
#Normalize distribution
ggplot(train, aes(x=log(SalePrice+1))) + geom_histogram(col = 'white') + theme_light()


test[ , SalePrice := NA]
merge <- rbind(train, test)











