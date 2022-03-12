## Bankruptcy prediction

Prediction of Polish and Slovak companies bankruptcy, 

# First dataset:
The dataset was taken from the Kaggle website. This data is taken from:

Zieba, M., Tomczak, S. K., & Tomczak, J. M. (2016). Ensemble Boosted Trees with Synthetic Features Generation in Application to Bankruptcy Prediction. Expert Systems with Applications.
Bankrupt firms are from the years: 2000 - 2012 And operating companies from 2007 - 2013.

The dataset consists of 5 files, in each of them there are values of 64 financial ratios and a binary variable class informing whether, respectively for each file after 5, 4, 3, 2 years and a year a given company declared bankruptcy. For each year there are between 5000 and 10 000 companies.


# Second dataset:
The second set of data is for businesses in Slovakia, the data is divided into 4 years and economic sectors such as agriculture, construction, industry, trade.

The data was taken from https://data.mendeley.com/datasets/j89csb932y/2.

These are 63 financial indicators, information on bankruptcies for more than 10,000 enterprises each year.

The files weighed a lot, exceeding the Github limits, so only a sample of them was made available, about 1/10, you can find sample files in data folder.

# Methods used:
- Discriminant analysis
- Decision tree
- Random forest
- Naive Bayes
- MLP networks
- Gradient Boosting Machine

# R scripts:
- Functions:Libraries.R : definition of functions used and importing libraries
- ImportData.R : in this script all the data needed is imported
- Visualization.R : graphical representation of statistics, missing data, quantitative ratio of classes
- ComparePredictionPol.R : Compare prediction of bankruptcy for polish data
- ComparePredictionSk.R  : Compare prediction of bankruptcy for slovak data

# RMarkdown documents:
- dataOverview.Rmd : input data analysis
- BankruptcyPredictionDoc.Rmd : comparision of methods accuracy


