context("bioCancer functions\n")

#test_that("set_class", {
# foo <- . %>% .^2 %>% set_class(c("foo", class(.)))
#expect_equal(3 %>% foo %>% class,c("foo","numeric"))
#})

testthat::test_that("cgdsr connection",
                    {
                      mycgds <- cgdsr::CGDS("http://www.cbioportal.org/")
                      cgdsr::test(mycgds)
                    })

testthat::test_that("ReactomeFI connection",
                    {
                      checkEq <- function(a,b) { if (identical(a,b)) "OK\n" else "FAILED!\n" }
                      source(paste0(system.file(package = "bioCancer"), "/app/tools/bioCancer/getReactomeFUN.R"),
                          local = TRUE)
                      # install.packages('RCurl')
                      # install.packages('XML')
                      # library('Rcurl', 'XML')
                      ReactomeResult <- getReactomeFI(2018, genes = c("TP53","BRCA1"))
                      cat('ReactomeFI connection... ',
                          checkEq(colnames(ReactomeResult), c("first.protein","second.protein"))
                      )
                    }
)





