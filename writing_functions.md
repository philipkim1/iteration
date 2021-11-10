Writing Functions
================
Philip Kim
11/10/2021

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.221053092 -0.396420540 -1.902899542 -0.957562794  0.218179004
    ##  [6]  1.113284948  0.004065062 -0.573516482  0.654358194  1.083730330
    ## [11]  0.183758157 -0.476498225  0.727959151  0.352286654 -1.579071598
    ## [16] -0.173109592 -0.108609980 -1.272731117 -1.138421611 -0.594774145
    ## [21]  0.706807375  2.116087998  0.863906673  1.721704248 -0.793565261

``` r
# 27:36
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)

    return(z)

}

z_scores(x = x_vec)
```

    ##  [1]  0.221053092 -0.396420540 -1.902899542 -0.957562794  0.218179004
    ##  [6]  1.113284948  0.004065062 -0.573516482  0.654358194  1.083730330
    ## [11]  0.183758157 -0.476498225  0.727959151  0.352286654 -1.579071598
    ## [16] -0.173109592 -0.108609980 -1.272731117 -1.138421611 -0.594774145
    ## [21]  0.706807375  2.116087998  0.863906673  1.721704248 -0.793565261

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1] -1.333077274  1.134983287  1.283338206  1.169220154  0.018771607
    ##  [6] -0.470761573  0.190578608  0.245059018  0.210150457  0.955541507
    ## [11] -0.502583071  0.452993040  0.209730347 -0.890823450 -0.615560434
    ## [16]  1.002872062  0.026861705 -1.298683757  0.411938639 -0.140147121
    ## [21] -2.056584977  0.545266841 -0.531778201 -0.485822764  1.521397124
    ## [26] -1.303290966 -0.533787614  0.552882326  0.691313533  0.005146482
    ## [31] -1.531530915 -0.067784436 -0.274658646  1.656995199 -1.224018753
    ## [36]  2.379101215  0.347935815 -2.015442488  0.586802680 -0.322543407

How great is this?

Only kinda great.

Let’s try again.

``` r
#34:10

z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)

    return(z)

}
```

``` r
#32:00
z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(c("my", "name", "is", "jeff")): x needs to be numeric

## Multiple outputs

``` r
mean_and_sd = function(x) {                   #38:30
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
    return(output_df)

}

mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.258

## Different sample sizes, means, sd

``` r
#45:00

sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.48  3.62

Let’s write a function that simulates data, computes the mean and sd.

``` r
sim_mean_sd = function(n, mu, sigma) {
  sim_data = 
  tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
  )
}

sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.86  3.23
