context("Waveform normalization")
library(datagoboop)


test_that("wave_norm works on mono inputs", {
  
  # Max sample magnitude is 1 (assuming non-constant input)
  expect_equal(max(abs(wave_norm(0.4 * sin(1:4000)))), 1)
  
  # Two waveforms that differ by a scalar multiple are equal after normalization
  expect_equal(wave_norm(0.4 * sin(1:4000)), wave_norm(0.8 * sin(1:4000)))
})


test_that("wave_norm works on stereo inputs", {
  
  # Both channels are debiased
  expect_identical(wave_norm(cbind(rep(1, 4000),  rep(1, 4000))),
                   wave_norm(cbind(rep(0, 4000),  rep(0, 4000))))
  
  # Both channels are debiased *separately*
  expect_identical(wave_norm(cbind(rep(-0.1, 4000),  rep(0.1, 4000))),
                   wave_norm(cbind(rep(0, 4000),  rep(0, 4000))))
  
  # Both channels are scaled together (if one channel is smaller in magnitude everywhere, this feature is preserved)
  expect_identical(t(diff(t(abs(wave_norm(cbind(rep(c(0.7, -0.7), 2000), rep(c(0.3, -0.3), 2000))))))) < 0,
                   as.matrix(rep(TRUE, 4000)))
})
