# v0.2.0, CRAN Update 2 / JSS Revision 1

*Under Construction*

- Provide several standard methods for objects generated with *BVAR*
  - `summary()` **To-Do**
  - `predict()` for ex-post forecasts and generating quantiles
  - `irf()` / `fevd()` for ex-post irfs and fevds and generating quantiles
  - `fitted()`, `residuals()`, `coef()` and `vcov()`
  - `density()` to summarise and plot hyperparameters **To-Do**
- Added `print()` methods for intermediate objects
- Reworked plotting
  - `plot.bvar()` now supports several types
  - Plots of fcasts and irfs have been changed
    - The preferred way of calling is now: `plot(x$irf)` or `plot(irf(x))`
    - The previous `bv_plot_irf(x)` is still supported, however
    - Confidence bands are now set in `predict()` / `irf()` instead
  - Added plot methods for `bvar_resid` and `bvar_density`
- Added option to set maximum number of draws for sign restrictions
- Prior construction has been standardised further
  - `alpha`, `lambda` and dummy-priors are quite alike, `psi` has been aligned
  - `bv_mn()` may be called with a numeric vector for `alpha` and/or `lambda`
- Documentation has been improved
  - Related functions are now documented in joint
  - Details on priors have been added
- Added less concise aliases for `bv_mh()` and `bv_mn()`
  - `bv_metropolis()` and `bv_minnesota()`
- Fixed bugs related to single confidence bands at 0.5
- Saved `fred_qd` with format version 2, lowering R dependency to (>= 3.3.0)
- Added *vars* and *coda* to suggests
  - Shared methods with *vars*, import is bypassed
  - Upcoming use of *coda* methods **To-Do**


# v0.1.5, CRAN Update 1

- Tried to clarify licensing terms with the Federal Reserve
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

- Updated DESCRIPTION with linked DOI
- Change `\dontrun{}` examples to `\donttest{}`
- Improve examples for plotting and printing
- Small fix to bounds in `plot_hyper()`
- R CMD check --as-cran: No errors or warnings, one note (New submission)


# v0.1.3, CRAN Submission 1

- Updated references with links via DOI
- Added examples to `print` and `plot` methods
- R CMD check --as-cran: No errors or warnings, one note (New submission)
