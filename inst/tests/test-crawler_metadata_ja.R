context("Crawling/Scraping metadata")

test_that("crawler_metadata_ja funciona se receber tipo 'muni'", {

  ufs <- c('AC','AL','AP','AM','BA','CE','DF','ES','GO','MA','MT','MS','MG',
           'PA','PB','PR','PE','PI','RJ','RN','RS','RO','RR','SC','SP','SE','TO')
  
  expect_that(crawler_metadata_ja("muni", "AC"), not(throws_error("Could not resolve host")))
  expect_that(crawler_metadata_ja("muni", "AC"), is_a("data.frame"))
  expect_that(str(crawler_metadata_ja("muni", "AC")), prints_text("3 variables"))
  expect_that(str(crawler_metadata_ja("muni", "AC")), not(prints_text("0 obs")))
  expect_that(all(crawler_metadata_ja("muni")$nome_uf %in% ufs), is_true())
  expect_that(crawler_metadata_ja("muni", "AC", cod_muni = 1), is_identical_to(crawler_metadata_ja("muni", "AC")))
  expect_that(crawler_metadata_ja("muni", "AC", cod_vara = 1), is_identical_to(crawler_metadata_ja("muni", "AC")))
  expect_that(crawler_metadata_ja("muni", "AC", cod_muni = 1, cod_vara = 1), is_identical_to(crawler_metadata_ja("muni", "AC")))
  rm(ufs)
})

## TODO
# test_that("crawler_metadata_ja funciona se receber tipo 'vara'", {
#   
# })
# 
# test_that("crawler_metadata_ja funciona se receber tipo 'prod'", {
#   
# })