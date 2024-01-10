--- 
title: "The R Opus v2"
author: "Jacob Lahne"
date: "2024-01-10"
site: bookdown::bookdown_site
documentclass: book
bibliography: [bib/references.bib, bib/packages.bib]
# url: your book url like https://bookdown.org/yihui/bookdown
cover-image: img/r-opus-cover.jpg
description: |
  This bookdown is a complete update of Hildegarde Heymann's "R Opus"
  documentation, originally compiled in 2015.  It gives helpful details and
  walkthroughs on common multivariate analyses in `R` for sensory-evaluation data.
link-citations: yes
github-repo: jlahne/r-opus-v2
---



# About {-}

To paraphrase one of my [inspirations for this project](https://bookdown.org/content/3890/) [@kurzDoingBayesianDataAnalysis2023]: This is a labor of love.  In 2015, [Hildegarde Heymann](https://wineserver.ucdavis.edu/people/hildegarde-heymann#/) (from here on: HGH), Distinguished Professor of Viticulture & Enology at UC-Davis, was kind enough to host me as a visiting scholar at her laboratory.  Among many other formative experiences I had as a sensory scientist at UC-Davis, HGH shared with me the [**R Opus**](files/The R Opus - May 2015.pdf), a handbook she had written and compiled of common analytical approaches to multivariate (food-sensory) data in `R`.  Like many of her other mentees, students, and postdocs, I benefited immensely from HGH's practical insight into how to apply abstruse multivariate analyses to real problems in research, and the **R Opus** manifested that knowledge into a hands-on guide for how to implement those tools.

In the time since, I have passed on the **R Opus** to my own mentees, students, and postdocs.  As `R` has continued to mature and become more accessible as a scripting language for data analysis--in particular, as ["tidy" programming principles](https://r4ds.hadley.nz/) have become more dominant--I have found myself also passing on my own set of tips and tricks for how to transform the tools found in the original **R Opus** into the current vernacular.  I began teaching data analytics and coding for researchers using `R`, and after learning how to (clumsily) transform my course notes into accessible **[bookdowns](https://bookdown.org/yihui/bookdown/)**, I thought: why not the **R Opus**?

This is that thought put into some sort of action.  I hope it is useful for you.

## Usage {-}

This **bookdown** is constructed around typical workflows and data analysis needs for [sensory scientists](https://en.wikipedia.org/wiki/Sensory_analysis).  You know who you are.

For all others, this bookdown is a structured introduction to the analysis of multivariate data from a practical and applied perspective.  Specifically, we investigate how to apply a series of common multivariate statistical analyses to a set of data derived from the tasting and rating of wines by both trained and untrained human subjects.  I place almost all of the emphasis on "how to", and much less on the statistical theory behind these approaches.  As I have spent longer and longer using the statistical tools sensory scientists commonly apply (and occasionally developing and adapting new ones), I have come to believe that it is much more important to think of statistical analyses as tools that we apply, rather than worrying about our complete abstract understanding.  The latter, of course, is also extremely important, but cannot be built without any possibility of first understanding why we might choose a particular analysis, and what it will look like applied to a particular dataset.

Even if you do not work in the field of sensory science, I hope that these examples will prove useful and easily understandable.  Plus: thinking about how wine tastes is interesting, especially when we combine it with complicated statistics!

## `R` Setup {-}

You can read this bookdown entirely online using the navigation panels.  However, if you want to learn to use `R` to conduct analyses like these, I strongly suggest you follow along.  You'll need to install `R` to do so.  To install `R`, go to https://cran.r-project.org/ and follow the appropriate instructions for your operating system.

While it is not strictly necessary, you will almost certainly find it more pleasant to use the RStudio IDE (Interactive Development Environment) for `R`, which can be downloaded and installed from https://posit.co/products/open-source/rstudio/. 

Here's the list of packages I used in this bookdown:



|package     |
|:-----------|
|tidyverse   |
|FactoMineR  |
|patchwork   |
|ggrepel     |
|ggedit      |
|ggforce     |
|DistatisR   |
|SensoMineR  |
|paletteer   |
|here        |
|broom       |
|skimr       |
|factoextra  |
|naniar      |
|agricolae   |
|tidytext    |
|brms        |
|tidybayes   |
|simputation |
|missMDA     |
|corrr       |
|widyr       |
|rgl         |
|candisc     |
|MASS        |
|ca          |
|pls         |




Once you have set up `R` (and RStudio), you can run the following lines of code to install the packages I use in this bookdown.  This might take a minute--and you might have to restart `R` to do it.  Go get a snack!


```r
packages <- c("tidyverse", "FactoMineR", "patchwork", "ggrepel", "ggedit", "ggforce", "DistatisR", "SensoMineR", "paletteer", "here", "broom", "skimr", "factoextra", "naniar", "agricolae", "tidytext", "brms", "tidybayes", "simputation", "missMDA", "corrr", "widyr", "rgl", "candisc", "MASS", "ca", "pls")

install.packages(packages, dependencies = TRUE)
```

Some of these tools install some additional (extra-`R`) tools.  The Stan-based tools are most likely to cause trouble.  If you cannot install them, consider the [walkthroughs on the main Stan website](https://mc-stan.org/users/interfaces/).  Not installing them means you just won't be able to replicate my extended experiments with Bayesian modeling (probably not the greatest loss in the world for you).

## What this is not {-}

I will not be going over the basics of `R` coding and programming.  You can pick up a a fair amount by following along, or if you are truly new to `R` I recommend checking out [Wickham et al's now-classic introduction](https://r4ds.hadley.nz/), the [Stat545 website](https://stat545.com/), or any of the [Carpentries workshops](https://datacarpentry.org/r-socialsci/).

## About me {-}

I'm an [associate professor of Food Science & Technology at Virginia Tech](https://www.fst.vt.edu/aboutus/faculty/jlahne.html).  I teach about sensory evaluation and about applied data analysis for food and ag scientists.  I am not a statistician, but I interact with and consume a lot of statistics.


As you work, you may start a local server to live preview this HTML book. This preview will update as you edit the book when you save individual .Rmd files. You can start the server in a work session by using the RStudio add-in "Preview book", or from the R console:



## Session Info {-}

At the end of chapter, I will be including a `sessionInfo()` chunk to try to make it easier to reproduce the work, as well as to diagnose any problems.


```r
sessionInfo()
#> R version 4.3.1 (2023-06-16)
#> Platform: aarch64-apple-darwin20 (64-bit)
#> Running under: macOS Ventura 13.6.1
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRblas.0.dylib 
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> time zone: America/New_York
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets 
#> [6] methods   base     
#> 
#> other attached packages:
#>  [1] lubridate_1.9.2 forcats_1.0.0   stringr_1.5.0  
#>  [4] dplyr_1.1.2     purrr_1.0.1     readr_2.1.4    
#>  [7] tidyr_1.3.0     tibble_3.2.1    ggplot2_3.4.3  
#> [10] tidyverse_2.0.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] gtable_0.3.4      jsonlite_1.8.7    compiler_4.3.1   
#>  [4] tidyselect_1.2.0  jquerylib_0.1.4   scales_1.2.1     
#>  [7] yaml_2.3.7        fastmap_1.1.1     R6_2.5.1         
#> [10] generics_0.1.3    knitr_1.43        bookdown_0.37    
#> [13] munsell_0.5.0     bslib_0.5.1       pillar_1.9.0     
#> [16] tzdb_0.4.0        rlang_1.1.1       utf8_1.2.3       
#> [19] stringi_1.7.12    cachem_1.0.8      xfun_0.39        
#> [22] sass_0.4.7        timechange_0.2.0  cli_3.6.1        
#> [25] withr_2.5.0       magrittr_2.0.3    digest_0.6.33    
#> [28] grid_4.3.1        rstudioapi_0.15.0 hms_1.1.3        
#> [31] lifecycle_1.0.3   vctrs_0.6.3       evaluate_0.21    
#> [34] glue_1.6.2        fansi_1.0.4       colorspace_2.1-0 
#> [37] rmarkdown_2.23    tools_4.3.1       pkgconfig_2.0.3  
#> [40] htmltools_0.5.6
```

