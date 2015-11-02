> Show Profiles Data in Table

Profiles panel displays informations related to gene list. User needs to specify a `Study`, a `Case`, and a `Genetic Profile` to get the right profile. 

It is more practice to select that have all data (case`_all`) and change only the profile. 

There are in general but not always, 6 types of genetic profiles:
* Copy Number Alteration ([CNA](https://en.wikipedia.org/wiki/Copy-number_variation)).
* mRNA expression ([mRNA](https://en.wikipedia.org/wiki/Gene_expression))
* Mutations ([Mut](https://en.wikipedia.org/wiki/Mutation))
* Methylation ([Met](https://en.wikipedia.org/wiki/DNA_methylation)): There are two probes `HM_27` and `HM_450`
* microRNA expression ([miRNA](https://en.wikipedia.org/wiki/MicroRNA))
* Reverse Phase Protein Array ([RPPA](https://en.wikipedia.org/wiki/Reverse_phase_protein_lysate_microarray))

It is possible to find other kind of data related to one of listed types. For example the `log` or [z_score](http://www.cbioportal.org/faq.jsp#what-are-mrna-and-microrna-z-scores) of mRNA expression.

#### Load Gene List
User can upload gene list examples or upload own gene list.

When user selects `examples` and clic on `Load examples` button, the gene list examples is loaded in DropDown Gene List.

When User selects  `clipboard`, it is possible to copy own gene list from text file (gene symbol by line) and clic on  `Paste Gene List` button. The gene List will be named  `Genes` in DropDown Gene List.

#### Load Profiles to Datasets

It is interesting to get any statistics analysis or transformation with genetic profiles. Any table from `Profiles` panel can be loaded to `Handle` panel by checking `Load Profiles to Datasets` and press the button. The data frame will be named `ProfData`.

