Writing Functions
================
Philip Kim
11/10/2021

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.96280936  2.46588053  0.30346644  0.49315557  0.27715935  0.74519093
    ##  [7] -0.13047098 -0.07589696  0.46399037 -1.65291209 -0.28188124 -0.03180222
    ## [13]  1.19278113  0.73078842 -0.45820270 -0.11698385  0.05140177 -2.59793435
    ## [19] -0.45807458  0.95759496 -0.19813904 -0.53732621 -0.56884633 -0.03153578
    ## [25] -1.50421250

``` r
# 27:36
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)

    return(z)

}

z_scores(x = x_vec)
```

    ##  [1]  0.96280936  2.46588053  0.30346644  0.49315557  0.27715935  0.74519093
    ##  [7] -0.13047098 -0.07589696  0.46399037 -1.65291209 -0.28188124 -0.03180222
    ## [13]  1.19278113  0.73078842 -0.45820270 -0.11698385  0.05140177 -2.59793435
    ## [19] -0.45807458  0.95759496 -0.19813904 -0.53732621 -0.56884633 -0.03153578
    ## [25] -1.50421250

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1]  0.30051330  0.57733173  0.98399230  0.12442683  0.22222947 -0.49601184
    ##  [7]  2.28746109 -0.89470047 -0.33751328 -1.55551576 -0.04578495 -0.11447840
    ## [13] -0.54031271 -0.72720170  0.95235933 -0.12557816 -0.28139944  0.25265375
    ## [19] -0.42086925  1.09027210  1.57744507 -0.17872908  1.54187988 -0.44580634
    ## [25]  0.09382308 -0.70965544 -1.62869148  0.02168658  0.11118735  1.85108858
    ## [31] -1.07832837  0.73780566  1.82254131  0.68328981 -0.13273663 -0.39707850
    ## [37] -0.61522337 -1.40481544 -2.23251443 -0.86904218

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
    ## 1  12.0 0.301

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
    ## 1  1.07  2.83

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
    ## 1  3.52  2.54

## Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Okay but there are a lo of pages of reviews #1:01:00

Write a function that gets reviews based on page url

``` r
get_page_reviews = function(page_url) {
    
  page_html = read_html(page_url)
  
  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews = 
    tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
  return(reviews)
}

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5]))
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 it was                                                5.0 ou… "\n  mad good …
    ##  2 Fun!                                                  4.0 ou… "\n  Fun and e…
    ##  3 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  4 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  5 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  6 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  7 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  8 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  9 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ## 10 Classic Film                                          5.0 ou… "\n  Had to or…
    ## # … with 40 more rows
