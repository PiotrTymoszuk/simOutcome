# Testing of the package functions

# tools -------

  library(simOutcome)

# Binary classifier --------

  test_binary <- binary_SeSp(prevalence = 0.5,
                             n = 10000,
                             Se = 0.75,
                             Sp = 0.85)

  summary(test_binary)

  attributes(dplyr::sample_n(test_binary, 100))

