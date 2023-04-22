## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

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

## -----------------------------------------------------------------------------
customTestWithMeans = function(sample1, sample2_h0, sample2_h1) {
  c(
    p_h0 = t.test(sample1, sample2_h0, 'less', var.equal = TRUE)$p.value,
    p_h1 = t.test(sample1, sample2_h1, 'less', var.equal = TRUE)$p.value,
    meanDiffH0 = mean(sample1 - sample2_h0),
    meanDiffH1 = mean(sample1 - sample2_h1)
  )
}

## -----------------------------------------------------------------------------
do.call(customTestWithMeans, customSample(85))

## -----------------------------------------------------------------------------
tTestPlus = function(x, y) {
  t_info = stats::t.test(x, y, paired = TRUE, var.equal = T)
  sdx = sd(x)
  sdy = sd(y)
  corr = cor(x, y)
  sd_p = sqrt((sdx ** 2 + sdy ** 2) - 2 * corr * sdx * sdy)
  return(
    list(
      pval = t_info$p.value,
      mean_diff = as.numeric(t_info$estimate),
      corr =  corr,
      smd = as.numeric(t_info$estimate) / sd_p
    )
  )
}

## -----------------------------------------------------------------------------
sampFun = function(sampSize, meanH1, corrH1) {
  correlatedSamples = faux::rnorm_multi(
    n = sampSize,
    vars = 3,
    mu = c(0, 0, meanH1),
    sd = 5,
    r = c(corrH1, corrH1, 0)
  )
  list(
    GRP_v1 = correlatedSamples$X1,
    GRP_v2_h0 = correlatedSamples$X2,
    GRP_v2_h1 = correlatedSamples$X3
  )
}

## -----------------------------------------------------------------------------
testFun = function(GRP_v1, GRP_v2_h0, GRP_v2_h1) {
  t0 = tTestPlus(GRP_v2_h0, GRP_v1)
  t1 = tTestPlus(GRP_v2_h1, GRP_v1)
  return(
    c(
      p_h0 = t0$pval,
      meanDiffValueH0 = t0$mean_diff,
      corrValueH0 = t0$corr,
      smdValueH0 = t0$smd,
      p_h1 = t1$pval,
      meanDiffValueH1 = t1$mean_diff,
      corrValueH1 = t1$corr,
      smdValueH1 = t1$smd
    )
  )
}

## -----------------------------------------------------------------------------
multiSample = function(sampSize) {
  corr_vars_h0 = faux::rnorm_multi(
    n = sampSize,
    vars = 2,
    mu = 0,
    sd = 10,
    r = 0.6
  )
  corr_vars_h1 = faux::rnorm_multi(
    n = sampSize,
    vars = 2,
    mu = 5,
    sd = 10,
    r = 0.6
  )
  v1 = rnorm(sampSize, mean = 0, sd = 10)
  list(
    grp_1_myTest_base = v1,
    grp_2_myTestA_h0 = corr_vars_h0$X1,
    grp_2_myTestA_h1 = corr_vars_h1$X1,
    grp_2_myTestB_h0 = corr_vars_h0$X2,
    grp_2_myTestB_h1 = corr_vars_h1$X2
  )
}

## -----------------------------------------------------------------------------
multiTest = function(grp_1_myTest_base,
                     grp_2_myTestA_h0,
                     grp_2_myTestA_h1,
                     grp_2_myTestB_h0,
                     grp_2_myTestB_h1) {
  c(
    p_myTestA_h0 = t.test(grp_1_myTest_base, grp_2_myTestA_h0, 'less', var.equal = TRUE)$p.value,
    p_myTestA_h1 = t.test(grp_1_myTest_base, grp_2_myTestA_h1, 'less', var.equal = TRUE)$p.value,
    p_myTestB_h0 = t.test(grp_1_myTest_base, grp_2_myTestB_h0, 'less', var.equal = TRUE)$p.value,
    p_myTestB_h1 = t.test(grp_1_myTest_base, grp_2_myTestB_h1, 'less', var.equal = TRUE)$p.value
  )
}

## -----------------------------------------------------------------------------
multiSampleGroupParam = function(grp_1, grp_2) {
  corrVarsH0 = faux::rnorm_multi(
    n = grp_2,
    vars = 3,
    mu = 0,
    sd = 10,
    r = c(0, 0.4, 0.8)
  )
  corrVarsH1 = faux::rnorm_multi(
    n = grp_2,
    vars = 3,
    mu = 4,
    sd = 10,
    r = c(0, 0.4, 0.8)
  )
  v1 = rnorm(grp_1, mean = 0, sd = 10)
  list(
    grp_1_test_base = v1,
    grp_2_testX_h0 = corrVarsH0$X1, # correlates with X3 (.4)
    grp_2_testX_h1 = corrVarsH1$X1,
    grp_2_testY_h0 = corrVarsH0$X2, # correlates with X3 (.8)
    grp_2_testY_h1 = corrVarsH1$X2,
    grp_2_testZ_h0 = corrVarsH0$X3, # correlates with X1 and X3
    grp_2_testZ_h1 = corrVarsH1$X3
  )
}

## -----------------------------------------------------------------------------
multiTest3 = function(grp_1_test_base,
                      grp_2_testX_h0,
                      grp_2_testX_h1,
                      grp_2_testY_h0,
                      grp_2_testY_h1,
                      grp_2_testZ_h0,
                      grp_2_testZ_h1) {
  c(
    p_testX_h0 = t.test(grp_1_test_base, grp_2_testX_h0, 'less', var.equal = TRUE)$p.value,
    p_testX_h1 = t.test(grp_1_test_base, grp_2_testX_h1, 'less', var.equal = TRUE)$p.value,
    p_testY_h0 = t.test(grp_1_test_base, grp_2_testY_h0, 'less', var.equal = TRUE)$p.value,
    p_testY_h1 = t.test(grp_1_test_base, grp_2_testY_h1, 'less', var.equal = TRUE)$p.value,
    p_testZ_h0 = t.test(grp_1_test_base, grp_2_testY_h0, 'less', var.equal = TRUE)$p.value,
    p_testZ_h1 = t.test(grp_1_test_base, grp_2_testY_h1, 'less', var.equal = TRUE)$p.value
  )
}

