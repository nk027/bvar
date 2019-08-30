# v0.1.6, Internal Release 1

- Prepare for first minor release
- Provide several standard methods for objects generated with *BVAR*
  - `predict()` for ex-post forecasts and generating quantiles
  - `irf()` / `fevd()` for ex-post irfs, fevds and generating quantiles
  - `fitted()`, `residuals()`, `coef()` and `vcov()`
  - `density()` as shorthand for calling `density()` on hyperparameters
- Added `print()` methods for intermediate objects
  - Includes `bv_minnesota`, `bv_metropolis` and method outputs
- Reworked plotting
  - `plot.bvar()` now supports types, subsets and multiple chains
  - `bv_plot_trace()` and `bv_plot_density()` are now deprecated
    - `density()` offers a plot method
    - Both can be replaced by `plot.bvar()`, incl. plotting of multiple chains
  - Plots of `bvar_fcast` and `bvar_irf` have been changed
    - The preferred way of calling is now: `plot(x$irf)` or `plot(irf(x))`
    - The previous `bv_plot_irf(x)` is now deprecated
    - Confidence bands are now set in `predict()` / `irf()` instead
  - Added plot methods for `bvar_resid` and `bvar_density`
- Added `sign_lim` to set maximum number of sign restriction draws
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
- Added *vars* to suggests for shared methods, import is bypassed


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
