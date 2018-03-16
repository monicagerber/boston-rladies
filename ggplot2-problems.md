ggplot2 problems
================
Monica Gerber, MPH
March 16, 2018

Problem 1
---------

Here's some fake data

``` r
test_data <- data.frame(stress = factor(c(1, 2, 3),
                                        levels = c(1, 2, 3),
                                        labels = c("Low Stress", "Moderate Stress", "High Stress")),
                        estimate = c(0, .5, 3),
                        lowercl = c(NA, -0.5, 2),
                        uppercl = c(NA, 1.5,  4))
```

Here's my plot

``` r
dodge <- position_dodge(width=0.9)

ggplot(test_data, aes(x = stress, y = estimate, group = 1, colour = stress, shape = stress)) +
    geom_point(position = dodge) +
    geom_errorbar(aes(ymax = uppercl, ymin = lowercl), position = dodge, width = 0.08, size = 1) +
    theme(axis.text.x =  element_blank(),
          axis.ticks.x =  element_blank()) +
    scale_colour_manual(values=c("#f03b20", "#253494", "#000000"), name="") +
    scale_shape(name = "") +
    labs(y = expression(paste("Outcome, ", beta, " (95% CI)")),
         x = "",
         title = "Effect of moderate and high stress on an outcome compared to low stress") +
    theme_bw() +
    geom_hline(yintercept = 0)
```

    ## Warning: Removed 1 rows containing missing values (geom_errorbar).

![](ggplot2-problems_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)

### What's the issue?

My problem: why doesn't removing the x-axis text and tick marks work?

### How I solved it

I solved this but it took me an embarassingly long time to figure out! How did I solve this? I re-wrote each line of the code and looked the plot after each new line until I figured out what was happening. It may be obvious to some but I was clueless.

Here's the new plot! (fill in at R-ladies meeting)

Problem 2
---------

Ok, so what's the best way to create a whole bunch of plots at once?

Because I work in public health I am going to tell you about an example from my work. To check the quality of data I was getting from a community health center, I needed to plot the number of pediatric visits by month using a bar chart. The y-axis is the count of visits and the x-axis is the month. There are 15 community health centers, so I need to do this 15 times.

This is what I came up with at first.