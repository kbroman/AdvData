---
layout: page
title: "Homework 3: Data wrangling and diagnostics"
description: "Homework assignment 3 for Advanced Data Analysis course, on rearranging and combining data files and exploring the data to identify problems."
---

_due 9 April 2020_

Wrangling data files and exploring data to identify potential problems
are critical and time-consuming initial steps in any data analysis.
Data scientists spend considerable time on this effort. In this
homework assignment, you will wrangle some messy data files and
seek to identify potential problems.


In the zip file [`hw3_rawdata.zip`](hw3_rawdata.zip) there is a
`README.txt` file plus a set of five Excel files with data for a set of
intercross mice. The mice are the same as those in [Broman et al.
(2015)
doi:10.1534/g3.115.019778](https://doi.org/10.1534/g3.115.019778);
these are the clinical phenotypes.


1. Combine the phenotype data into a single dataset with mice as rows
   and measurements as columns. See the `README.txt` file for details on
   which columns to focus on.

   Note that the mouse identifiers appear in
   several forms. Also note that, to merge the data from
   `final_rbm_data.xls`, you need to use the sample/mouse key in
   `RBM_Tube_Number_Key.xls`.


2. Explore the data for possible problems: typos, errors, outliers?
   If a particular trait appears in multiple files, check that the values
   correspond.


3. Write a report describing:

   - your approach for merging the files, and the challenges involved
   - your data diagnostics methods
   - a summary of the problems you found. (Don't show _everything_ you
     did, but just some representative graphs.)
