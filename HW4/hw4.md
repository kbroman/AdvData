---
layout: page
title: "Homework 4: Model building for prediction"
description: "Homework assignment 4 for Advanced Data Analysis course, on building a model to predict diabetes using NHANES data"
---

_due 30 April 2020_

The goal of this project is to build a model for predicting diabetes.
We will consider data from the [NHANES
survey](https://wwwn.cdc.gov/nchs/nhanes/).

I've extracted data for a set of 16 predictor variables for the first
three waves of the survey (1999-2003). We will define diabetes to be
having been diagnosed or having fasting glucose >= 126 mg/dL.

In the zip file [`hw4_data.zip`](hw4_data.zip), there is a
data file plus a data dictionary, both as CSV files.

Note that some of the variables have some strange categories, for
example age started smoking and household income.



1. Build a model to predict diabetes from the other variables, for
   example using logistic regression, a nearest neighbor classifier,
   or other means. Discuss your choice.


2. Discuss your handling of missing data.


3. Assess the performance of your predictions. What are the
   specificity and sensitivity, or the true and false positive rates?
   Use separate training and test sets, or k-fold cross validation.


4. Assess the relative importance of the variables. Which variables
   are most important for predicting diabetes?


### Related work

- Yu et al. (2010) Application of support vector machine modeling for
  prediction of common diseases: the case of diabetes and pre-diabetes.
  [doi:10.1186/1472-6947-10-16](https://doi.org/10.1186/1472-6947-10-16)

- Semerdjian and Frank (2017) An ensemble classifier for predicting
  the onset of Type II diabetes.
  [arXiv:1708.07480](https://arxiv.org/abs/1708.07480)
