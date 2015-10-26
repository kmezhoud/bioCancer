> Compare the means of two or more variables or groups in the data

The compare means t-test is used to compare the mean of a variable for one sample in our data to the mean of the same variable in one, or more, other samples. This is important since we seldom have access to data for an entire population. The hypothesized value in the population is specified in the 'Comparsion value' box.

We can perform either a one-tailed test (i.e., less than or greater than) or two-tailed test (see 'Alternative hypothesis'). We use one-tailed tests to evaluate if the available data provide evidence that a variable or effect is larger (or smaller) in one sample than another.

### Example: Mutation Frequency through Diseases (Studies)

The mutations of `DNA_damage_Response` gene list are collected from 4 studies and saved in `epiGenomcis` datastes in column `FreqMut`.

Suppose we want to test if some genes are more exposed to mutation in selected studies compared other genes. To test this hypothesis we first select `Genes` and select `FreqMut` as the numerical variable to compare across genes. bioCancer will do a pair-wise comparison of salaries across the three levels. We are interested in a one-sided hypothesis.

![Summary](figures_quant/compare_means_summary.png)

Because the p-values are smaller than the conventional level of significance (i.e. < 0.05) for each pair-wise comparison we can reject the null hypothesis based on the available sample of mutation. The data suggest that some genes are more mutated than others in selected studies (`*`).

In addition to the numerical output provided in the Summary tab we can also evaluate the hypothesis visually (see Plot tab). The settings in the side-panel are the same as before. The tab displays a box plot of genes mutation frequency with confidence interval (black) and standard error (blue) bars. Consistent with the results shown in the Summary tab there is clear difference between the gene mutation across genes. We can also choose to plot the data as a bar chart or as a set of density curves.

![Plot](figures_quant/compare_means_plot.png)
