## HazardRateTest
An R package for a statistical test that compares the hazard rates of two samples.

Disclaimer: due to the complexity of calculating the test statistics, it is not recommended to use this test for large sample sizes in this packages current version. It has only been tested up to samples sizes of n=m=100, which took a few minutes to run, and the run time increases exponentially with the sample sizes.

## To Install
Installation requires the `devtools` package.

```{r}
devtools::install_github("wbbreslin/HazardRateTest")
```

## Thesis Paper
https://wbbreslin.github.io/Statistics-Projects/Masters-Thesis.html
