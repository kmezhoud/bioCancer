require(knitr)
require(markdown)

knit('bioCancer.Rmd','bioCancer.md')

markdownToHTML('bioCancer.md','bioCancer.html',options=c('use_xhml'))

system("pandoc -s bioCancer.html -o bioCancer.pdf")

## Reduce the size of Vignette.pdf file
tools::compactPDF("bioCancer.pdf", gs_quality = "ebook")
