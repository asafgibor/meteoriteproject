library(MASS)

## Load data. Make sure working directory is in right place.
space = read.csv("meteorites.csv")

##
## LDA.
##

## Fit LDA model using just the numerical variables. By default, prior probabilities are 
## set to the sample proportions for the different classes.
space_lda_1 <- lda(class ~ mass_g + year + lat + long, data = space)

## Here is LDA with equal prior probabilities for each class.
space_lda_2 <- lda(class ~ mass_g + year + lat + long, prior = rep(1 / 8, 8), 
  data = space)

## View basic output. 
space_lda_1
space_lda_2

## Use the fitted model to make predictions.
pred_lda_1 <- predict(space_lda_1, newdata = space[, c(5, 7, 8, 9)])$class
pred_lda_2 <- predict(space_lda_2, newdata = space[, c(5, 7, 8, 9)])$class

## Confusion matrix (ouch!).
table(pred_lda_1, space[, 4])
table(pred_lda_2, space[, 4])

## Error rates are very high.
mean(pred_lda_1 != space[, 4])
mean(pred_lda_2 != space[, 4])

##
## QDA.
##

space_qda_1 <- qda(class ~ mass_g + year + lat + long, data = space)
space_qda_2 <- qda(class ~ mass_g + year + lat + long, prior = rep(1 / 8, 8), 
  data = space)

space_qda_1
space_qda_2

pred_qda_1 <- predict(space_qda_1, newdata = space[, c(5, 7, 8, 9)])$class
pred_qda_2 <- predict(space_qda_2, newdata = space[, c(5, 7, 8, 9)])$class

## Confusion matrix (ouch again!).
table(pred_qda_1, space[, 4])
table(pred_qda_2, space[, 4])

## Error rates are still very high.
mean(pred_qda_1 != space[, 4])
mean(pred_qda_2 != space[, 4])

