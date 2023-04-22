## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library('POSSA')

## -----------------------------------------------------------------------------
createOrdinal = function(s, m) {
    findInterval(rnorm(s, mean = m, sd = 1.3), vec = c(-Inf, 1, 2, 3, 4, Inf))
}

## -----------------------------------------------------------------------------
sampleRank = function(sample_size) {
  list(
    sample1 = createOrdinal(sample_size, 2),
    sample2_h0 = createOrdinal(sample_size, 2),
    sample2_h1 = createOrdinal(sample_size, 3)
  )
}

## -----------------------------------------------------------------------------
testWilc = function(sample1, sample2_h0, sample2_h1) {
  c(
    p_h0 = wilcox.test(sample1, sample2_h0, alternative = 'less')$p.value,
    p_h1 = wilcox.test(sample1, sample2_h1, alternative = 'less')$p.value
  )
}

## -----------------------------------------------------------------------------
sampleAUC = function(sampSize) {
    list(
        # AR
        ARtruth = -runif(n = sampSize, 0, 1),
        ARliar = sort(-rbeta(n = sampSize, shape1 = 0.2, 1)),
        # RT
        RTtruth = rnorm(sampSize, mean = 0, sd = 30),
        RTliar_h0  = rnorm(sampSize, mean = 41, sd = 30),
        RTliar_h1  = rnorm(sampSize, mean = 60, sd = 30)
    )
}

