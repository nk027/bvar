BVAR: Hierarchical Bayesian Vector Autoregression
=======

[![CRAN](http://www.r-pkg.org/badges/version/BVAR)](http://cran.r-project.org/package=BVAR)
[![month](http://cranlogs.r-pkg.org/badges/BVAR)](http://www.r-pkg.org/pkg/BVAR)
[![total](http://cranlogs.r-pkg.org/badges/grand-total/BVAR)](http://www.r-pkg.org/pkg/BVAR)

Estimation of hierarchical Bayesian vector autoregressive models. Implements hierarchical prior selection for conjugate priors in the fashion of Giannone, Lenza & Primiceri (2015). Functions to calculate forecasts, and compute and identify impulse responses and forecast error variance decompositions are available. Several methods to print, plot and summarise results facilitate analysis.

Installation
-------

BVAR is available on CRAN. The development version can be installed from GitHub.
``` r
install.packages("BVAR")
devtools::install_github("nk027/BVAR")
```

Usage
-------

The main function to perform hierarchical Bayesian VAR estimation is `bvar()`. Calls can be customised with regard to the sampling (e.g. via `n_draw`, or see `bv_mh()`) or with regard to the priors (see `bv_priors()`). Forecasts and impulse responses can be computed at runtime, or afterwards (see `predict()` and `irf()`).

Analysis is facilitated by a variety of standard methods. The default `plot()` method provides trace and density plots of hyperparameters and optionally coefficients. Impulse responses and forecasts can easily be assessed with the provided `plot()` methods. Other available methods include `summary()`, `fitted()`, `residuals()`, `coef()`, `vcov()` and `density()`. Note that BVAR generates draws from the posterior -- all methods include functionality to access this distributional information.

BVAR comes with the FRED-MD and FRED-QD datasets (McCracken and Ng, 2016). They can be accessed using `data("fred_md")` or `data("fred_qd")` respectively. The dataset is licensed under a modified ODC-BY 1.0 license, that is available in the provided *LICENSE* file.

References
-------

Domenico Giannone, Michele Lenza and Giorgio E. Primiceri (2015). Prior Selection for Vector Autoregressions. The Review of Economics and Statistics, 97:2, 436-451, DOI: [10.1162/REST_a_00483](dx.doi.org/10.1162/REST_a_00483.).

Michael W. McCracken and Serena Ng (2016). FRED-MD: A Monthly Database for Macroeconomic Research. Journal of Business & Economic Statistics, 34:4, 574-589, DOI: [10.1080/07350015.2015.1086655](dx.doi.org/10.1080/07350015.2015.1086655).
