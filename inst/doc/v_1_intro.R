## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
customSample = function(sampleSize) {
  list(
    sample1 = rnorm(sampleSize, mean = 0, sd = 10),
    sample2_h0 = rnorm(sampleSize, mean = 0, sd = 10),
    sample2_h1 = rnorm(sampleSize, mean = 5, sd = 10)
  )
}

## -----------------------------------------------------------------------------
customTest = function(sample1, sample2_h0, sample2_h1) {
  c(
    p_h0 = t.test(sample1, sample2_h0, 'less', var.equal = TRUE)$p.value,
    p_h1 = t.test(sample1, sample2_h1, 'less', var.equal = TRUE)$p.value
  )
}

## -----------------------------------------------------------------------------
do.call(customTest, customSample(55))

## ----setup--------------------------------------------------------------------
library('POSSA')

## -----------------------------------------------------------------------------
customSample = function(sampleSize) {
  list(
    sample1 = rnorm(sampleSize, mean = 0, sd = 10),
    sample2_h0 = rnorm(sampleSize, mean = 0, sd = 10),
    sample2_h1 = rnorm(sampleSize, mean = 5, sd = 10)
  )
}

