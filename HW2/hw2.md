---
layout: page
title: "Homework 2: Pest management trial"
description: Homework assignment 2 for Advanced Data Analysis course, on reproducing results from a clinical trial analysis, and data visualization.
---

_due 12 March 2020_

A randomized clinical trial in Baltimore and Boston sought to assess
whether integrated pest management (IPM) could reduce asthma symptoms
in exposed children. Households were randomized to two groups: control (education
about management of mice) and treatment (education plus professional
pest management). Subjects were followed for one year.

Read the published study, [Matsui et al.
(2017)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5632564/). Also
poke through the supplements: [study
protocol](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5632564/bin/NIHMS904640-supplement-Supplement_1.pdf)
and
[eMethods/tables/figure](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5632564/bin/NIHMS904640-supplement-Supplement_2.pdf).
Further, maybe take a look at the summary videos at the [study
website](https://rdpeng.github.io/MAAIT/).

Download the data:
 - [published dataset](https://rdpeng.github.io/MAAIT/maait.csv)
 - codebook as [csv file](https://rdpeng.github.io/MAAIT/codebook.csv)
   or [xlsx file](https://rdpeng.github.io/MAAIT/codebook.xlsx)

a. Reproduce the first row in [Table
2](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5632564/table/T2/) of
[Matsui et al. (2017)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5632564/), on
the primary outcome, "Maximal symptom days/2 wk".

The analysis does not seem to be not described in much detail. The
most detail I could find was on page 72 of the [study protocol
supplement](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5632564/table/T2/)
(labeled page 56).

You can use the [gee package](https://cran.r-project.org/package=gee).
The outcome variable is `sxsmaxday`, and I believe you'll omit `VisitNum`
0 and 1, consider only complete cases, and then fit a log-linear model
something like this:

```r
out <- gee(sxsmaxday ~ group, id=ID, family=poisson(link=log), corstr="exchangeable")
```

b. Instead of using GEE, combine the outcomes for visits 2-4 in a
subject by taking the sum, and then fit a log-linear model with
`glm()`, something like this:

```r
out <- glm(sxsmaxday_sum ~ group, family=poisson(link=log))
```

With this approach, what is the confidence interval for the treatment
effect?

c. Create a data visualization of the `sxsmaxday` outcome, to try to
reveal both the longitudinal nature of the data and the treatment
effect. Discuss your design choices.

d. Turn the first eight rows of [Table
2](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5632564/table/T2/) of
[Matsui et al.
(2017)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5632564/) into a
graph (or graphs), showing the treatment effects on the primary and
secondary outcomes. Discuss your design choices.

e. What do you conclude about the effect of integrated pest management
on asthma symptoms?



### Resources

- [Lesson on generalized estimating
  equations](https://online.stat.psu.edu/stat504/node/179/)
- [Using the gee
  package](https://cran.r-project.org/web/packages/HSAUR2/vignettes/Ch_analysing_longitudinal_dataII.pdf)
  vignette from package for [Handbook of statistical analyses using
  R](https://smile.amazon.com/gp/product/1482204584?ie=UTF8&tag=7210-20)
- [Gelman et al. (2002) Let's practice what we preach: Turning tables
  into graphs. Am Stat 56:121-130](http://www.stat.columbia.edu/~gelman/research/published/dodhia.pdf)

---

This assignment is derived from [a data science
homework](https://jhu-advdatasci.github.io/2019/homeworks/711-hw5-assignment.html)
designed by [Stephanie Hicks](http://stephaniehicks.com/) and [Roger
Peng](http://www.biostat.jhsph.edu/~rpeng/) at Johns Hopkins.
