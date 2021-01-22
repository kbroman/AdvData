## Lessons

- Follow up artifacts; they might be the most interesting results

- The simplest things can be the most important

- Consider taking logs, especially for ratios or when values span
  several orders of magnitude.

- Including data summaries in an analysis report can help to catch
  errors

- File organization, documentation, version control: investments that
  pay off in the long term

- Computer simulations have a lot of great uses

- The EM algorithm is really useful; use the log likelihood as a diagnostic

- Don't just cram your data into the standard approach

- Cramming your data into the standard approach might work fine

- If it seems too good to be true, it probably is

- Always ask: does this make sense?

- Omitting data is usually bad; crudely ignoring correlations is often
  perfectly fine

- Extracting the _full_ information from the data may not be worth
  the effort

- Time to solve a computational problem includes the time to formulate a solution,
  to write the program, to run the program, and to make sense of the
  results. Program run time is almost always the least of these.

- Use computer simulations to check that your software/method is
  giving reasonable results

- When cleaning data, think about what might have gone wrong and how
  it might be revealed. Also just make a ton of plots.

- If data are supposed to match, check that they do. If the rows in
  two data files are supposed to be aligned, check that they do.



## Principles

- Start with an understanding of the problems and data

- Think about a model for the data-generating process

- Modify your desires to match the defaults; focus your compulsive
  behavior on things that matter.

- In data visualization, show as much data as possible.

- It can be useful to ask, "Could this just be noise?"


## Further lessons

- The most important thing is that you get the right answer

- If you find a bug, first create a test that reproduces it, then fix it

- Capture the full process of data cleaning (what you did, what you
  saw, how you interpreted it, why you made the decisions you made),
  because you'll want that information later

- With the bootstrap, don't think about "re-sampling"; rather, think
  "simulate from an estimate of the population".

- How to tell if the bootstrap works? Simulate!

- Permutation tests, when appropriate, are the most natural of
  significance test, and they can make it easy to control for multiple
  testing.
