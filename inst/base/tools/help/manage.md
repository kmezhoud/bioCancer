> Manage data and state: Load data into bioCancer, Save data to disk, Remove a dataset from memory, or Save/Load the full state of the app


### Datasets

When you first start bioCancer a dataset (`epiGenomics`) with information on how it was formatted is shown in `Handle` panel.

It is good practice to add a description of the data and variables to each file you use. For the files that are part of bioCancer you will see a brief overview of the variables etc. below the table of the first 10 rows of the data. If you would like to add a description for your own data check the 'Add/edit data description' check-box. A window will open below the data table where you can add text in
<a href="http://rmarkdown.rstudio.com/authoring_pandoc_markdown.html" target="_blank">markdown</a> format. The descriptions of the data included with bioCancer should serve as a good starting point.

If you would like to rename a dataset loaded in bioCancer check the `Rename data` box, enter a new name for the data, and click the `Rename` button

### Load data

The best way to load and save data for use in bioCancer (and R) is to use the R-data format (rda). These are binary files that can be stored compactly and read into R quickly. Choose `rda` from the `Load data of type` dropdown and click `Choose Files` to locate the file(s) you want to load. If the `rda` file is available online choose `rda (url)` from the dropdown, paste the url into the text input, and press `Load`.

You can get data from a spreadsheet (e.g., Excel or Google sheets) into bioCancer in two ways. First, you can save data from the spreadsheet in csv format and then, in bioCancer, choose `csv` from the `Load data of type` dropdown. Most likely you will have a header row in the csv file with variable names. If the data are not comma separated you can choose semicolon or tab separated. To load a csv file click 'Choose files' and locate the file on your computer. If the `csv` data is available online choose `csv (url)` from the dropdown, paste the url into the text input shown, and press `Load`.

> **Note:** For Windows users with data that contain multibyte characters please make sure your data are in ANSI format so R(adiant) can load the characters correctly.

Alternatively, you can select and copy the data in the spreadsheet using CTRL-C (or CMD-C on mac), go to bioCancer, choose `clipboard` from the dropdown, and click the `Paste data` button. This is a short-cut that can be convenient for smaller datasets that are cleanly formatted. If you see a message in bioCancer that the data were not transferred cleanly try saving the data in csv format and loading it into bioCancer as described above.

To access all data files bundled with bioCancer choose `examples` from the `Load data of type` dropdown and click `Load examples`. These files are used to illustrate the various analysis tools accessible in bioCancer. For example, the catalog sales data is used as an example in the help file for regression (i.e., Regression > Linear (OLS)).

### Save data

As mentioned above, the most convenient way to get data in and out of bioCancer is to use the R-data format (rda). Choose `rda` from the `Save data` dropdown and click the `Save data` button to save selected dataset to file.

It is good practice to add a description of the data and variables to each file you use. For the files that are part of bioCancer you will see a brief overview of the variables etc. below the table of the first 10 rows of the data. If you would like to add a description for your own data check the 'Add/edit data description' check-box. A window will open below that data table where you can add text in
<a href="http://rmarkdown.rstudio.com/authoring_pandoc_markdown.html" target="_blank">markdown</a> format. The descriptions of the data included with bioCancer should serve as a good starting point. When you save the data as an rda file the description you created (or edited) will automatically be added to the file.

Getting data from bioCancer into a spreadsheet can be achieved in two ways. First, you can save data in csv format and load the file into the spreadsheet (i.e., choose `csv` from the `Save data` dropdown and click the `Save data` button). Alternatively, you can copy the data from bioCancer into the clipboard by choosing `clipboard` from the dropdown and clicking the `Copy data` button, open the spreadsheet, and paste the data from bioCancer using CTRL-V (or CMD-V on mac).

### Save and load state

You can save and load the state of the bioCancer app just as you would a data file. The state file (extension rda) will contain (1) the data loaded in bioCancer, (2) settings for the analyses you were working on, (3) and any reports or code from the R-menu. Save the state-file to your hard-disk and when you are ready to continue simply load it by selecting the state radio button and clicking the `Choose file` button.

The best way to save your analyses is to save the state of the app to a file by clicking on the <i title='Save' class='fa fa-save'></i> icon in the navbar and then on `Save state`. Similar functionality is available in `Data > Manage` tab.

This is convenient if you want to save your work to be completed at another time, perhaps on another computer, or to review any assignments you completed using bioCancer. You can also share the file with others that would like to replicate your analyses. As an example, download and then load the state_file [`RadiantState.rda`](https://vnijs.github.io/radiant/examples/RadiantState.rda). Go to `Data > View`, `Data > Visualize` to see some of the settings loaded from the statefile. There is also a report in `R > Report` created using the Radiant interface. The html file <a href="https://vnijs.github.io/radiant/examples/RadiantState.html" target="_blank">`RadiantState.html`</a> contains the output.

A related feature in bioCancer is that state is maintained if you accidentally navigate to another page, close (and reopen) the browser, and/or hit refresh. Use `Reset` in the <i title='Power off' class='fa fa-power-off'></i> menu in the navigation bar to return to a clean/new state.

Loading and saving state also works with Rstudio. If you start bioCancer from Rstudio and use <i title='Power off' class='fa fa-power-off'></i> > `Stop` to stop the app, lists called `r_data` and `r_state` will be put into Rstudio's global workspace. If you start bioCancer again using `bioCancer()` it will use these lists to restore state. This can be convenient if you want to make changes to a data file in Rstudio and load it back into bioCancer. Also, if you load a state file directly into Rstudio it will be used when you start bioCancer to recreate a previous state.

### Remove data from memory

If data are loaded that you no longer need access to in the current session check the `Remove data from memory` box. Then select the data to remove and click the `Remove data` button. One datafile will always remain open.

### Using commands to load and save data

The `loadr` command can be used to load data from a file directly into a Radiant session and add it to the `Datasets` dropdown. The `saver` command can be used to exact data from Radiant and save it to disk. Data can be loaded or saved as `rda` or `rds` format depending on the file extension chosen. These commands can be used both inside or without the Radiant browser interface. See `?loadr` and `?saver` for details.
