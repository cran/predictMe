## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=TRUE----------------------------------------------------------------
library(predictMe)

## ----eval=TRUE----------------------------------------------------------------
# Simulate data set with continuous outcome (use all default values)
dfContinuous <- quickSim()
# Use multiple linear regression as algorithm to predict the outcome.
lmRes <- lm(y~x1+x2,data=dfContinuous)
# Extract measured outcome and the predicted outcome (fitted values)
# from the regression output, put both in a data.frame.
lmDf <- data.frame(measOutcome=lmRes$model$y,
                   fitted=lmRes$fitted.values)
# Generate 5 equal bins (transformed outcome 0-100, bin width 20,
# yields 5 bins).
x100c <- binContinuous(x=lmDf, measColumn = 1, binWidth = 20)

## ----eval=TRUE----------------------------------------------------------------
# Show some lines of the data:
head(x100c[["xTrans"]])

## ----eval=TRUE----------------------------------------------------------------
# Demand the visualized performance, using makeTablePlot
outLs <- makeTablePlot(x100c[["xTrans"]][,1:2], measColumn = 1, plot = TRUE)
# Display names of the resulting list
cbind(names(outLs))

## ----eval=TRUE----------------------------------------------------------------
# Display total count table
outLs$totalCountTable

## ----eval=TRUE----------------------------------------------------------------
# Display row sum table
outLs$rowSumTable

## ----eval=TRUE----------------------------------------------------------------
# Display column sum table
outLs$colSumTable

## ----eval=TRUE----------------------------------------------------------------
# Generate 20 equal bins.
x100c5 <- binContinuous(x=lmDf, measColumn = 1, binWidth = 5)
# Demand the visualized performance, using makeTablePlot. Setting plotCellRes
# (Res = results) to FALSE means to not print the results into the cells.
outLs5 <- makeTablePlot(x100c5[["xTrans"]][,1:2], measColumn = 1, plot = TRUE, 
                        plotCellRes = FALSE)

## ----eval=TRUE----------------------------------------------------------------
# Demand the visualized differences, using makeDiffPlot
outDiffLs <- makeDiffPlot(x100c[["xTrans"]][,5:6], idCol = 2)

## ----eval=TRUE----------------------------------------------------------------
# Use the function makeDiffPlotColor
dpc <- makeDiffPlotColor(x100c[["xTrans"]][,5:7], idCol = 2, colorCol = 3)

## ----eval=TRUE----------------------------------------------------------------
# Use makeDiffPlotColor output and add a 'facet'
dpcFacet <- dpc$diffPlotColor + ggplot2::facet_wrap(~absBinDiff)

## ----eval=TRUE----------------------------------------------------------------
# Simulate data set with binary outcome
dfBinary <- quickSim(type="binary")
# Use logistic regression as algorithm to predict the response variable
# (estimated probability of outcome being present).
glmRes <- glm(y~x1+x2,data=dfBinary,family="binomial")
# Extract measured outcome and the predicted probability (fitted values)
# from the logistic regression output, put both in a data.frame.
glmDf <- data.frame(measOutcome=glmRes$model$y,
                    fitted=glmRes$fitted.values)
# Apply function binBinary, set binWidth to 20.
x100b <- binBinary(x=glmDf, measColumn = 1, binWidth = 20)

## ----eval=TRUE----------------------------------------------------------------
# Use part of the output of function binBinary, in particular: Display
# one row per bin (binWidth = 20 = 5 bins)
idx1RowPerBin <- match((1:5), x100b[["xTrans"]]$measOutcome)
# Display only the first 4 columns
x100b[["xTrans"]][idx1RowPerBin,1:4]

## ----eval=TRUE----------------------------------------------------------------
# Summary of column fittedPerc for the first bin
idxFirstBin <- x100b[["xTrans"]]$measOutcome==1
summary(x100b[["xTrans"]][idxFirstBin,"fittedPerc"])

## ----eval=TRUE----------------------------------------------------------------
# Demand the visualized performance, using makeTablePlot
outLs <- makeTablePlot(x100b[["xTrans"]][,1:2], measColumn = 1, plot = TRUE)

## ----eval=TRUE----------------------------------------------------------------
# Demand the visualized differences, using makeDiffPlot
outDiffLs <- makeDiffPlot(x100b[["xTrans"]][,5:6], idCol = 2)

## ----eval=TRUE----------------------------------------------------------------
# How many levels?
nlevels(x100b[["xTrans"]][,"absDiffBins"])

## ----eval=TRUE----------------------------------------------------------------
# Apply function binBinary, set binWidth to 4.
x100b4 <- binBinary(x=glmDf, measColumn = 1, binWidth = 4)
# How many levels?
nlevels(x100b4[["xTrans"]][,"absDiffBins"])

## ----eval=TRUE----------------------------------------------------------------
# dpb: difference plot binary
dpb <- makeDiffPlotColor(x100b4[["xTrans"]][,5:7], idCol = 2, colorCol = 3)
# Use makeDiffPlotColor output and add a 'facet'
dpbFacet <- dpb$diffPlotColor + ggplot2::facet_wrap(~absDiffBins)

