BVAR
=======

Toolkit for the estimation of hierarchical Bayesian VARs. Implements prior selection for conjugate priors in the fashion of Giannone, Lenza and Primiceri (2015), accounting for uncertainty about prior hyperparameters. Allows for the computation of impulse responses as well as forecasts based on draws of coefficients from their respective posterior distribution.

The main function to perform hierarchical Bayesian VAR estimation is `bvar()`, which includes settings regarding the lag length, the number of iterations to run and several others that are constructed via helpter functions. The included FRED-QD dataset (McCracken and Ng, 2016) can be accessed using `data("fred_qd")` and may be used to investigate a variety of economic issues.

The function `bv_priors()` is used for settings regarding the priors; i.e. which to include and how. Other helper functions functions (see `bv_mn()`, `bv_dummy()`) may be used to set up the Minnesota prior or other conjugate dummy observation priors.
The helper function `bv_mh()` provides settings regarding the Metropolis-Hastings step. It allows scaling the Hessian matrix that is used to generate hyperparameters, thus allowing fine-tuning of the acceptance rate.
Further options regarding the computation of impulse responses and forecasts can be accessed via the functions `bv_irf()` and `bv_fcast()`. Adjustments to the horizon can be set as well as the type of identifcation of shocks for the impulse responses.

The generated `bvar` and contained `bvar_fcast` and `bvar_irf` objects can be further analysed using a variety of methods, including `summary()`, `plot()`, `fitted()`, `residuals()`, `coef()`, `vcov()`, `predict()`, `irf()`, `fevd()` and `density()`.

### References

Giannone, D., Lenza, M., & Primiceri, G. E. (2015). Prior Selection for Vector Autoregressions. Review of Economics and Statistics, 97, 436-451.

McCracken, M. W., and Ng, S. (2016). FRED-MD: A Monthly Database for Macroeconomic Research. Journal of Business & Economic Statistics, 34, 574-589.
