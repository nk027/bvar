# v0.2.0, CRAN Update 2 / JSS Revision 1

- Extend methods further
  - Add `summary()` methods for `bvar`, `bvar_irf` and `bvar_fcast`
  - Add `as.mcmc` method to interface with *coda*
  - Support hyperparameters and coefficients in `plot.bvar()` and `density()`
  - Add `companion()` method to retrieve the companion matrix
  - Improve `ylim` of `plot.bvar()` density plots with multiple chains
  - Add `logLik()` method for `bvar` objects
- Fix `bv_psi()` errors for modes other than "auto"
- Change `fcast` and `irf` defaults of `bvar()` to `NULL`
- Move from *MASS* to *mvtnorm* for `dmvnorm()` (used in `logLik()`)
- Be more specific in error messages
- Add *coda* to suggestions for convergence assessment etc.
- Add files *LICENSE*, *CITATION* and *NEWS.md*
- Improve examples for further test coverage
  - `R CMD check --run-donttest`: One warning (deprecated functions)


# v0.1.6, Internal Release 1

- Prepare for minor release 0.2.0
- Provide several standard methods for objects generated with *BVAR*
  - `predict()` for ex-post forecasts and updating quantiles
  - `irf()` / `fevd()` for ex-post irfs, fevds and updating quantiles
  - `fitted()`, `residuals()`, `coef()` and `vcov()`
  - `density()` as shorthand for calling `density()` on hyperparameters
- Add `print()` methods for intermediate objects
  - Includes `bv_minnesota`, `bv_metropolis` and method outputs
- Rework plotting
  - `plot.bvar()` now supports types, subsets and multiple chains
  - Deprecate `bv_plot_trace()` and `bv_plot_density()`
    - Add `density()`, including a plot method
    - Replace by `plot.bvar()`, incl. plotting of multiple chains
  - Change plotting of `bvar_fcast` and `bvar_irf`
    - Deprecate `bv_plot_irf(x)` for `plot(x$irf)` or `plot(irf(x))`
    - Move `conf_bands` argument to `predict()` / `irf()`
  - Add plot methods for `bvar_resid` and `bvar_density`
- Add `sign_lim` to `bv_irf()` to set maximum number of sign restriction draws
- Standardise prior construction further
  - Align `alpha` and `lambda`, improve `psi` alignment
  - `bv_mn()` may be called with a numeric vector for `alpha` and/or `lambda`
- Improve documentation
  - Document related functions in joint
  - Add details on priors
- Add less concise aliases for `bv_mh()` and `bv_mn()`
  - `bv_metropolis()` and `bv_minnesota()`
- Fix bugs related to single confidence bands at 0.5
- Save `fred_qd` with format version 2, lowering R dependency to (>= 3.3.0)
- Add *vars* to suggests for shared methods, import is bypassed


# v0.1.5, CRAN Update 1

- Try to clarify licensing terms with the Federal Reserve
  - Some copyrighted series may have to be removed
  - Subset the dataset to only include variables in public domain for now


# v0.1.4, JSS Submission

- Fix addition of prior pdfs to ML
  - `alpha` needs an sd parameter
  - `psi` now needs proper shape and scale parameters
  - Add normalising constant
- Add lines to all density plots (when supplied via ellipsis)
- Add documentation on using `scale_hess` as a vector
- Add two pre-constructed dummy priors `bv_soc()` and `bv_sur()`
- Further split up calculation of marginal likelihood


# v0.1.3, CRAN Submission 2

- Update DESCRIPTION with linked DOI
- Change `\dontrun{}` examples to `\donttest{}`
- Improve examples for plotting and printing
- Fix bounds in `plot_hyper()`
- `R CMD check --as-cran`: No errors or warnings, one note (New submission)


# v0.1.3, CRAN Submission 1

- Update references with links via DOI
- Add examples to `print` and `plot` methods
- `R CMD check --as-cran`: No errors or warnings, one note (New submission)
