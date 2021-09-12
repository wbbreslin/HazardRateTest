## HazardRateTest
An R package for a statistical test that compares the hazard rates of two samples. 

## To Install
Installation requires the `devtools` package.

```{r}
devtools::install_github("wbbreslin/HazardRateTest")
```

## Using this Package
The main function in this package is the `hazard.test` function. This function is analogous to the `t.test` or `wilcox.test` functions in base R. All of these functions output the test statistic and p-value for their respective tests. Consider the two data sets generated by the R code below.
```{r}
set.seed(314)
x = rexp(n=10, rate=1)
y = rexp(n=10, rate=1/2)
```
The `hazard.test` function can be applied to these two data sets.
```{r}
hazard.test(x,y)
```
This will output:
```{r}
$Phi
[1] -798

$W
[1] -0.3940741

$Z
[1] -1.505205

$p.value
[1] 0.1322714
```

---

Interpreting the output:
* Phi is the kernel of the test statistic, which is the total number of 4-length x-pair and y-pair sequences suggesting X greater than Y in hazard rate ordering, minus those suggesting X less than Y in hazard rate ordering
* W is our test statistic, which is Phi rescaled such that it is between -1 and 1
* Z standardizes W to have mean=0 and sd=1, which is used for the normal approximation of the p-value
* The p-value is exact for sample sizes n and m less than or equal to 10, otherwise the normal approximation is used

The other function in this package is `null.dist`, which is used to find the exact null distribution of the test statistic given two sample sizes. Using two samples with 3 data points each as an example:
```{r}
null.dist(m=3,n=3)
```
This will ouput the null distribution in a frequency table.
```
              W Occurrences Probability
[1,] -1.0000000           2        0.10
[2,] -0.5555556           3        0.15
[3,] -0.3333333           2        0.10
[4,] -0.1111111           3        0.15
[5,]  0.1111111           3        0.15
[6,]  0.3333333           2        0.10
[7,]  0.5555556           3        0.15
[8,]  1.0000000           2        0.10
```
The exact null distribution is not needed for sample sizes m>8 and n>8, as the normal approximation is sufficient beyond that point. It is not recommended to use this function for larger sample sizes due to the computation time required for it to run.

## Releases

### v2.0 Computation Speed Update (9/9/2021)

This update resolves the issue of computational complexity by counting the kernel sequence for the test statistics using combinatorics, rather than by brute force. 

For context, before this update, with samples of size n=100 and m=100, the code took about 2 minutes to run. With this update, for the same sample sizes, the run time is less than a second. However, the test still takes a decent amount of time to run for larger samples. For samples of size n=1,000 and m=1,000, the test took ~20 seconds to run. For n=2,000 and m=2,000, it took 2.5 minutes to run. This a a major improvement from the v1.0 release.

(Note that these times may vary based on the power of your computer)

### v1.0 Initial Release (2/13/2021)
