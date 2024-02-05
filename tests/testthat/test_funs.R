context("bioCancer functions\n")

#test_that("set_class", {
# foo <- . %>% .^2 %>% set_class(c("foo", class(.)))
#expect_equal(3 %>% foo %>% class,c("foo","numeric"))
#})

testthat::test_that("cBioPortal connection",
                    {
                      #mycgds <- CGDS("http://www.cbioportal.org/")
                      cgds <- cBioPortalData::cBioPortal(
                        hostname = "www.cbioportal.org",
                        protocol = "https",
                        api = "/api/v2/api-docs"
                      )

                      test.CGDS(cgds)
                    })

testthat::test_that("ReactomeFI connection",
                    {
                      checkEq <- function(a,b) { if (identical(a,b)) "OK\n" else "FAILED!\n" }
                      source(paste0(system.file(package = "bioCancer"), "/app/tools/bioCancer/getReactomeFUN.R"),
                          local = TRUE)
                      ReactomeResult <- getReactomeFI(2021, genes = c("TP53","BRCA1"))
                      cat('ReactomeFI connection... ',
                          checkEq(colnames(ReactomeResult), c("first.protein","second.protein"))
                      )
                    }
)





