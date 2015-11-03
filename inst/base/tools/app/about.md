# bioCancer - Interactive Multi-Assay Cancer Genomics Data Visualisation

[![Travis-CI Build Status](https://travis-ci.org/kmezhoud/bioCancer.svg?branch=master)](https://travis-ci.org/kmezhoud/bioCancer) 
 [![Coverage Status](https://img.shields.io/coveralls/kmezhoud/bioCancer.svg)](https://coveralls.io/r/kmezhoud/bioCancer?branch=master)


bioCancer is a platform-independent browser-based interface for Cancer Genomics Data analytics in [R](http://www.r-project.org/), based on the [Shiny](http://www.rstudio.com/shiny/) package. 

bioCancer is a collaborative project. In part, it is inspired from [radiant](https://github.com/vnijs/radiant) developed by <a href="http://rady.ucsd.edu/faculty/directory/nijs/" target="\_blank">Dr. Vincent Nijs</a>. 


## Key features

- Explore: Quickly and easily summarize, visualize, and analyze your data
- Cross-platform: It runs in a browser on Windows, Mac, and Linux
- Reproducible: Recreate results and share work with others as a state file or an [Rmarkdown](http://rmarkdown.rstudio.com/) report
- Context: Data and examples focus on Cancer genomics data

<!--
<iframe width="640" height="375" src="https://www.youtube.com/embed/ioHopyfD2f0" frameborder="0" allowfullscreen></iframe>
-->

#### Explore

bioCancer is interactive. Results update immediately when inputs are changed (i.e., no separate dialog boxes). This greatly facilitates exploration and understanding of the data.

#### Cross-platform

bioCancer works on Windows, Mac, or Linux. It can run without an Internet connection and no data will leave your computer. You can also run the app as a web application on a server.

> **Note:** For Windows users with data that contain multibyte characters please make sure your data are in ANSI format so R(adiant) can load characters correctly.

#### Reproducible

Simply saving output is not enough. You need the ability to recreate results for the same data and/or when new data become available. Moreover, others may want to review your analysis and results. Save and load the state of the application to continue your work at a later time or on another computer. Share state files with others and create reproducible reports using [Rmarkdown](http://rmarkdown.rstudio.com/). See also the section on `Saving and loading state` below


#### Programming

Although bioCancer's web-interface can handle quite a few data and analysis tasks, at times you may prefer to write your own code. bioCancer provides a bridge to programming in R(studio) by exporting the functions used for analysis. For more information about programming with bioCancer see the [programming](http://vnijs.github.io/radiant/programming.html) page on the documentation site.

#### Context

bioCancer focuses on Cancer Genomics data visualisation and Genes Classifications.



## How to install bioCancer


- Required: [R](http://cran.rstudio.com/) version 3.2 or later
- Required: A modern browser (e.g., [Chrome](https://www.google.com/intl/en/chrome/browser/desktop/) or Safari). Internet Explorer (version 11 or higher) should work as well
- Recommended: [Rstudio](http://www.rstudio.com/products/rstudio/download/)


bioCancer is under development:
```r
 devtools::install_github("kmezhoud/bioCancer")
 library("bioCancer")
```



When bioCancer starts you will see a table of available Cancer Studies.  To close the application click on `Quit` in the Navigation bar and then click the `Quit` button on the left of the screen. The bioCancer process will stop and the browser window will close (or gray-out).



## Documentation


Documentation and tutorials are available in the bioCancer web interface (the `?` icons and the `Help` menu).

<!--
Want some help getting started? Watch the tutorials on the [documentation site of bioCancer](http://kmezhoud.github.io/CancerPortal/tutorials.html)
-->

## Saving and loading state from bioCancer
To save your analyses save the state of the app to a file by clicking on the <i title='Save' class='fa fa-save'></i> icon in the navbar and then on `Save state` (see also the `Data > Manage` tab). You can open this state file at a later time or on another computer to continue where you left off. You can also share the file with others that may want to replicate your analyses. As an example, load the state_file [`RadiantState.rda`](https://github.com/vnijs/radiant/blob/master/inst/examples/RadiantState.rda?raw=true) through the Data > Manage tab. Go to `Data > View`, `Data > Visualize` to see some of the settings. There is also a report in `R > Report` that was created using the Radiant interface. The html file [`RadiantState.html`](https://github.com/vnijs/radiant/blob/master/inst/examples/RadiantState.html?raw=true) contains the output.


Loading and saving state also works with Rstudio. If you start Radiant from Rstudio and use <i title='Power off' class='fa fa-power-off'></i> > `Stop` to stop the app, lists called `r_data` and `r_state` will be put into Rstudio's global workspace. If you start radiant again using `radiant()` it will use these lists to restore state. This can be convenient if you want to make changes to a data file in Rstudio and load it back into Radiant. Also, if you load a state file directly into Rstudio it will be used when you start Radiant to recreate a previous state.

```r
numericInput("sm_comp_value", "Comparison value:", state_init('sm_comp_value',sm_args$comp_value))
```

## Acknowledgment


The `Handle` panel of bioCancer is adapted from [radiant](https://github.com/vnijs/radiant). I would like to thank <a href="http://rady.ucsd.edu/faculty/directory/nijs/" target="\_blank"> Dr. Vincent Nijs</a> for sharing his work for R community.


## License

bioCancer is licensed under the <a href="http://www.tldrlegal.com/l/AGPL3" target="\_blank">AGPLv3</a>. The documentation and videos on this site and the bioCancer help files are licensed under the creative commons attribution, non-commercial, share-alike license <a href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank">CC-NC-SA</a>.

As a summary, the AGPLv3 license requires, attribution, including copyright and license information in copies of the software, stating changes if the code is modified, and disclosure of all source code.

