
# v1.0.5, CRAN Update 10
- Add historical decompositions and RMSE/LPS, and WAIC for analysis
- Improve the algorithm to draw sign restrictions
  - Shocks are now built and checked per variable
- Fix the CRAN *NOTE* on missing "tangle output"
- Update FRED-datasets to 2023-10 vintage
  - Newer FRED-QD files include (unannounced) small naming changes


# v1.0.4, CRAN Update 9

- Fix bug from last update, where automatic ARIMA-priors where sqrt'd twice
  - Thanks to Michael Wolf for pointing this out, and
  - many thanks to Nirai Tomass for discovering the bug
- Update **FRED-MD** and **FRED-QD** datasets to the 2023-02 vintage


# v1.0.3, CRAN Update 8

- Fix bug where warnings caused an error during automatic ARIMA-based priors
  - Thanks to Martin Feldkircher for pointing this out


# v1.0.2, CRAN Update 7 / JSS Release

- Add DOI in the CITATION file for a new **JSS publication**
  - DOI will be registered after publication on CRAN
  - References now use the DOI interface instead of URLs
- Update **FRED-QD** and **FRED-MD** datasets to 2021-10
- Fix minor issues with vignette (e.g. fixed dataset, references, etc)
- Add verbosity to ARIMA-based automatic prior settings
- Add hook to simplify use of shared generics with *vars*
- Add *README* file to the package with correct URLs


# v1.0.1, CRAN Update 6

- Add identification via zero and sign restrictions
- Update *CITATION* with the forthcoming JSS reference
- Prepare for tidy outputs in *BVARverse*
- Fix minor bugs and typos (internal checks, documentation, and vignette)


# v1.0.0, CRAN Update 5 / JSS Revision 2

- Add fancy **vignette** with background and demonstrations
- Add new features
  - New **FRED-MD** database and updated FRED-QD (`data("fred_md")`)
  - Transformation helper functions (`fred_transform()`, `fred_code()`)
  - Add **Conditional forecasting** (see `?bv_fcast()`)
  - New replacement functions for `irf()` and `predict()` (`irf(x) <- irf(x)`)
  - Provide wrapper for parallelised execution (`par_bvar()`)
- Improve existing features
  - Enhance IRF and forecast plotting
    - `area` argument adds polygons for credible intervals
    - `t_back` allows adding realised values before forecasts
    - `col` and `fill` arguments allow changing colours
    - transparence is applied to sequential lines / polygons
    - Improved x-axis labelling
  - Regex may be used for `vars`, `vars_response`, and `vars_impulse`
  - New `type` for `coef()`, `fitted()`, etc, to retrieve means / quantiles
  - Add constructors for the prior mean `b` argument in `bv_mn()`
  - Auto `psi` now allows for one order of integration
- **Enhance speed** considerably (~2-10 times faster)
  - Move IRF and forecasts out of MCMC
  - Capitalise upon matrix properties
  - Cached and customised multivariate normal drawing
  - Optimised FEVD computation
- Fix bugs
  - IRF calculation is now ordered properly (please recalculate)
  - *coda* methods are now proper methods (potential issue on Windows)
  - Vectorised `scale_hess` now works properly
- Remove deprecated functions and arguments
- Work on documentation and examples
- Update citation information
- Improve upon internal structure
  - Unit tests with *tinytest* for development (skipped on CRAN)
  - Outsource additional steps to dedicated functions
  - More robustness checks and add verbosity to errors
- Tested extensively on R 4.0.0 and R 3.6.3


# v0.2.2, CRAN Update 4 / Impulse Response Hotfix

- Fix impulse response calculation (stray transpose of coefficients)
  - Thanks, Maximilian Böck for helping us track this down
- Add some verbosity to error messages
- Prepare for next major release
  - Include messages about future extensions and changes of FEVDs
  - Update docs on future construction of sign restrictions
  - Update docs on prediction and IRFs in `bvar()`
- Add FRED-MD co-author Serena Ng to data contributors


# v0.2.1, CRAN Update 3 / FRED-QD ODC-BY 1.0

- Clarify exact ToU of the FRED-QD dataset with St. Louis Fed
  - Comply with the new modified **ODC-BY 1.0** for FRED-QD
  - Mention and add license to *LICENSE* file (linked in *DESCRIPTION*)
  - Add the copyrighted series we are allowed to use
  - Mention updates and license in the data documentation
- Fix and improve documentation


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
- Fix addition of prior pdfs to ML
  - `alpha` needs an sd parameter
  - `psi` now needs proper shape and scale parameters
  - Add normalising constant
- Add lines to all density plots (when supplied via ellipsis)
- Add documentation on using `scale_hess` as a vector
- Add two pre-constructed dummy priors `bv_soc()` and `bv_sur()`
- Further split up calculation of marginal likelihood


# v0.1.3, CRAN Submission

- Update DESCRIPTION with linked DOI
- Change `\dontrun{}` examples to `\donttest{}`
- Fix bounds in `plot_hyper()`
- Update references with links via DOI
- Add and improve examples for `print` and `plot` methods
- `R CMD check --as-cran`: No errors or warnings, one note (New submission)
