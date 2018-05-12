# An R Package with Utilities for Graphics and Plots

[<img alt="Travis CI Build Status" src="https://img.shields.io/travis/thomasWeise/plotteR/master.svg" height="20"/>](https://travis-ci.org/thomasWeise/plotteR/)

## 1. Introduction

In this package, we put some simple utilities for graphics and plots.
The main goal is to provide some wrapper around the `plot` function to make it easier to plot several data sets coming from a list (`batchPlot.list`) or a list of objects/lists (`batchPlot.groups`).
These plots then can automatically use distinctive colors.

## 2. Examples

### 2.1. `batchPlot.list`

![Example image for `batchPlot.list`](examples/batchPlotList.png)

    library(plotteR)
    
    # set a random seed for replicability
    set.seed(1367);
    
    # make an example
    make.example <- function(f) {
      n <- as.integer(round(runif(n=1, min=10, max=200)));
      x <- sort(runif(n=n, min=0, max=3)); # generate x data
      y <- rnorm(n=n, mean=f(x), s=0.1);  # noisy y
      x <- rnorm(n=n, mean=x, s=0.1); # noisy x
      return(list(x=x, y=y, f=f));
    }
    
    # the three base functions
    f <- c(function(x) 1 - 0.2*x + 0.75*x*x - 0.3*x*x*x,
           function(x) 0.1 * exp(3 - x),
           function(x) 1.2 + 0.7*sin(2*x));
    
    # create the three example data sets
    examples <- lapply(X=f, FUN=make.example);
    
    # plot the original data
    batchPlot.list(examples,
                   names=c("f1", "f2", "f3"),
                   ffun = function(l, x) l$f(x),
                   main="Original Data and Function Values for x",
                   legend=list(x="bottom", horiz=TRUE));


### 2.2. `batchPlot.groups`

![Example image for `batchPlot.groups`](examples/batchPlotGroups.png)

    library(plotteR)
    
    # set a random seed for replicability
    set.seed(2677);
    
    # the three base functions with the mean parameter values
    f <- list(
      list(f=function(x, par) par[1] + par[2]*x + par[3]*x*x + par[4]*x*x*x,
           m=c(1, -0.2, 0.75, -0.3)),
      list(f = function(x, par) par[1] * exp(par[2] - x),
           m=c(0.1, 3)),
      list(f=function(x, par) par[1] + par[2]*sin(par[3]*x),
           m=c(0, 1, 3)));
    
    # create the three example data sets
    examples <- lapply(X=f, FUN=function(example) {
    # for each example function, plot 4 to 50 instances
      lapply(seq_len(runif(n=1, min=4, max=50)),
    # for each instance
             FUN=function(i) {
    # randomly choose the x-coordinates
               x <- runif(n=as.integer(round(runif(n=1, min=10, max=200))),
                          min=0, max=3);
               m <- example$m;
    # pick parameters which are normally distributed around the suggestion
               par <- rnorm(n=length(m), mean=m, s=0.1*abs(m));
    # and construct a function
               fff <- function(x) example$f(x, par);
    # and pass this function as result example together with the x values
               list(x=x, f=fff)
             })
    });
    
    # plot the original data
    batchPlot.groups(examples,
                     names=c("f1", "f2", "f3"),
                     ffun = function(l, x) l$f(x),
                     main="Several Groups of Functions",
                     plotXY=FALSE, plotXF=TRUE,
                     legend=list(x="bottom", horiz=TRUE));


## 3. Installation

You can install the package directl from GitHub by using the package
[`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) as
follows:

    library(devtools)
    install_github("thomasWeise/plotteR")

If `devtools` is not yet installed on your machine, you need to FIRST do

    install.packages("devtools")

## 4. License

The copyright holder of this package is Prof. Dr. Thomas Weise (see Contact).
The package is licensed under the  GNU LESSER GENERAL PUBLIC LICENSE Version 3, 29 June 2007.

## 5. Contact

If you have any questions or suggestions, please contact
[Prof. Dr. Thomas Weise](http://iao.hfuu.edu.cn/team/director) of the
[Institute of Applied Optimization](http://iao.hfuu.edu.cn/) at
[Hefei University](http://www.hfuu.edu.cn) in
Hefei, Anhui, China via
email to [tweise@hfuu.edu.cn](mailto:tweise@hfuu.edu.cn).
