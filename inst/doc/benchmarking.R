## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library('POSSA')

## -----------------------------------------------------------------------------
# minimal output
sampleEmpty = function(sample_size) {
  list(
    sample1 = rep(1, sample_size),
    sample2_h0 = rep(1, sample_size),
    sample2_h1 = rep(1, sample_size)
  )
}
testEmpty = function(sample1, sample2_h0, sample2_h1) {
  c(
    p_h0 = 0.5,
    p_h1 = 0.5
  )
}

# multiple (five) hypotheses
sampleEmptyMulti = function(sample_size) {
  list(
    sample1 = rep(1, sample_size),
    sample2_h0 = rep(1, sample_size),
    sample2_h1 = rep(1, sample_size),
    sample3_h0 = rep(1, sample_size),
    sample3_h1 = rep(1, sample_size),
    sample4_h0 = rep(1, sample_size),
    sample4_h1 = rep(1, sample_size),
    sample5_h0 = rep(1, sample_size),
    sample5_h1 = rep(1, sample_size)
  )
}
testEmptyMulti = function(sample1, sample2_h0, sample2_h1, sample3_h0, sample3_h1, sample4_h0, sample4_h1, sample5_h0, sample5_h1) {
  c(
    p_h0 = 0.5,
    p_h1 = 0.5,
    p_test_2_h0 = 0.5,
    p_test_2_h1 = 0.5,
    p_test_3_h0 = 0.5,
    p_test_3_h1 = 0.5,
    p_test_4_h0 = 0.5,
    p_test_4_h1 = 0.5,
    p_test_5_h0 = 0.5,
    p_test_5_h1 = 0.5
  )
}

# to check that they work correctly:
# do.call(testEmpty, sampleEmpty(100))
# do.call(testEmptyMulti, sampleEmptyMulti(100))


