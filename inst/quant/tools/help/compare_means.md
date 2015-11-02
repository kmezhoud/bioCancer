> Compare the means of two or more variables or groups in the data

The compare means t-test is used to compare the mean of a variable for one sample in our data to the mean of the same variable in one, or more, other samples. The null hypothesis for the difference between the samples (or groups) is set to zero.

We can perform either a one-tailed test (i.e., `less than` or `greater than`) or a two-tailed test (see the 'Alternative hypothesis' dropdown). We use one-tailed tests to evaluate if the available data provide evidence that the difference in sample means between groups is less than (or greater than ) zero.

### Example: Mutation Frequency through Diseases (Studies)

The mutations of `DNA_damage_Response` gene list are collected from 4 studies and saved in `epiGenomcis` datasets in column `FreqMut`.
Suppose we want to test if some genes are more exposed to mutation in selected studies compared other genes. To test this hypothesis we first select `Genes` and select `FreqMut` as the numerical variable to compare across genes.
![summary](figures_quant/compare_means_summary.png)

The first two blocks of output show basic information about the test (e.g.,. selected variables and confidence levels) and summary statistics (e.g., mean, standard deviation, etc. per group). The final block of output shows the following:

* `Null hyp.` is the null hypothesis and `Alt. hyp.` the alternative hypothesis
* `diff` is the difference between the sample means for two groups. If the null hypothesis is true we expect this difference to be small (i.e., close to zero)

* `p.value` is the probability of being wrong if we choose to reject the null hypothesis (i.e., the type I error)

If we check `Show additional statistics` the following output is added:

* `se` is the standard error (i.e., the standard deviation of the sampling distribution of `diff`)
* `t.value` is the _t_ statistic associated with `diff` that we can compare to a t-distribution (i.e., `diff` / `se`)
* `df` is the degrees of freedom associated with the statistical test. Note that the Welch approximation is used for the degrees of freedom
* `5% 100%` show the 95% confidence interval around the difference in sample means. These numbers provide a range within which the true population difference is likely to fall


Because the p-values are smaller than the conventional level of significance (i.e. < 0.05) for each pair-wise comparison we can reject the null hypothesis based on the available sample of mutation. The data suggest that some genes are more mutated than others in selected studies (`*`).

In addition to the numerical output provided in the Summary tab we can also evaluate the hypothesis visually (see Plot tab). The settings in the side-panel are the same as before. The tab displays a box plot of genes mutation frequency with confidence interval (black) and standard error (blue) bars. Consistent with the results shown in the Summary tab there is clear difference between the gene mutation across genes. We can also choose to plot the data as a bar chart or scatter plot or as a set of density curves.

![plot](figures_quant/compare_means_plot.png)


### Testing

There are three approaches we can use to evaluate the null hypothesis. We will choose a significance level of 0.05.<sup>1</sup> Of course, each approach will lead to the same conclusion.

#### p.value


When the p.values is **smaller** than the significance level (0.05) we reject the null hypothesis for each evaluated pair. Note also the '***' that are used as an indicator for significance.


#### confidence interval

Because zero is **not** contained in any of the confidence intervals we reject the null hypothesis for each evaluated combination of ranks. Because our alternative hypothesis is `Less than` the confidence interval is actually an upper bound for the difference in mutation in the genes at a 95% confidence level.

### Multiple comparison adjustment

The more comparisons we evaluate the more likely we are to find a "significant" result just by chance even if the null hypothesis is true. If we conduct 100 tests and set our **significance level** at 0.05 (or 5%) we can expect to find 5 p.values smaller than or equal to 0.05 even if the are no associations in the population.

Bonferroni adjustment ensures the p.values are scaled appropriately given the number of tests conducted. <a href="https://xkcd.com/882/" target="blank">This XKCD cartoon</a> expresses the need for this type of adjustments very clearly.
