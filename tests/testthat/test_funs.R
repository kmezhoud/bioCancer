context("bioCancer functions")

test_that("set_class", {
  foo <- . %>% .^2 %>% set_class(c("foo", class(.)))
  expect_equal(3 %>% foo %>% class,c("foo","numeric"))
})

test_that("cgdsr connection",
{
  mycgds = CGDS("http://www.cbioportal.org/public-portal/")
  result <- test(mycgds)
  expect_equal(result, NULL)
})

