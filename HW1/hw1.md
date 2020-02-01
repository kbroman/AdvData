---
layout: page
title: "Homework 1: Spider mites"
description: Homework assignment 1 for Advanced Data Analysis course, on fitting and evaluating a dose-response relationship with a generalized linear model.
---

_due 20 Feb 2020_


The western predatory mite _Metaseiulus occidentalis_ has been used as
a biological control agent of spider mites in orchards and vineyards.
Since resistance to some insecticides was found to develop naturally
in _M. occidentalis_, and the species was documented to be an
acarine predator of economic importance, it was selected as a
candidate for genetic improvement.

The data for this assignment involve an
insecticide-resistant strain of
_M. occidentalis_ developed by crossing colonies with field-developed
resistances with a laborary-selected resistant colony.
Our data come from an experiment aimed at determining the genetic
basis for susceptibility to the insecticide permethrin.

The experiment was carried out in a lab at the Department of
Entomology, University of California, Berkeley.
Mites were expesed in 11-14 groups of ten to a given dose
of permethrin for a fixed interval of time, and the number of mites
dead in each group of ten at the end of the interval was recorded.
This was done for each of seven doses, expressed in grams of active
ingredient per 100 liters.

a. Consider the data in [`hw1_mitesA.txt`](data/hw1_mitesA.txt).
At each of seven doses we have the number of dead out of ten, for
between 11 and 14 groups of ten. Use this data to examine the
hypothesis that at any given dose the mites are dying independently
with a constant probability. Consider using computer simulations.

b. Make the binomial assumption for mites at each dose and pool the
data across groups. Then fit a dose-response curve of a standard type
(e.g., probit, logit, complementary log-log, etc.), justifying your
choice. That is, fit a generalized linear model (with `glm` in R, or
with the python module `statsmodels`).

c. Estimate the LD50 (the dose at which the probability of death is
50%) and its standard error (SE).

d. How might you need to modify your analyses in (b) and (c) in light
of your conclusions in (a)?

e. Repeat (a)-(d) for data on a second strain of mites, in
[`hw1_mitesB.txt`](data/hw1_mitesB.txt).

f. Assess whether the does-response curves for the two strains are
parallel.

---

This assignment is a slightly modified version of one designed by [Terry
Speed](https://www.wehi.edu.au/people/terry-speed) for an applied
statistics course at the University of California, Berkeley.
