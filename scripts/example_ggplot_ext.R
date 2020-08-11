
library("BVAR")
data("fred_qd")
df <- fred_qd[, c("GDPC1", "CPIAUCSL", "FEDFUNDS")]
for (i in c("GDPC1", "CPIAUCSL"))
  df[5:nrow(df), i] <- diff(log(df[, i]), lag = 4) *100
df <- df[5:nrow(df),]
plot.ts(df)

# configure priors and other aspects of the model
mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
  alpha = bv_alpha(mode = 2, sd = 0.25, min = 1, max = 3),
  var = 1e07)

soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)

# once the priors are specified we provide them to bv_priors()
priors <- bv_priors(hyper = "auto", mn = mn, soc = sur, sur = sur)

# adjust the MH step (Metropolis-Hastings algorithm) to achieve a suitable acceptrance rate
mh <- bv_metropolis(scale_hess = 0.005, adjust_acc = TRUE,
                    acc_lower = 0.25, acc_upper = 0.35, acc_change = 0.02)

# Identification is performed via Cholesky decomposition unless sign restrictions are provided.
signs <- matrix(c(1, 1, 1, NA, 1, 1, -1, -1, 1), ncol = 3)
irf_signs <- bv_irf(horizon = 12, fevd = TRUE, identification = TRUE, sign_restr = signs)
fcasts <- NULL

run_signs <- bvar(df, lags = 5, n_draw = 2500, n_burn = 1000, priors = priors, mh = mh,
                  fcast = fcasts, irf = irf_signs, verbose = TRUE)

plot(irf(run_signs))


gg_df <- function(x) {

  stopifnot(inherits(x, "bvar_irf"))

  # So we know what's what
  dimnames(x[["quants"]])[[2]] <- x[["variables"]]
  dimnames(x[["quants"]])[[4]] <- x[["variables"]]
  dimnames(x[["quants"]])[[3]] <- seq(dim(x[["quants"]])[3])

  out <- as.data.frame.table(x[["quants"]]) # Magic base R
  colnames(out) <- c("quant", "response", "time", "shock", "value")
  out[["time"]] <- as.integer(out[["time"]]) # Can't be a factor for the x axis

  return(out)
}

x <- irf(run_signs)
df <- gg_df(x)

library("ggplot2")

ggplot(df, aes(x = time, y = value)) +
  facet_grid(response ~ shock) +
  geom_line(aes(color = quant)) +
  geom_hline(yintercept = 0, color="black") +
  theme_gray() +
  ggtitle("BVAR impulse response")


# Subset to a specific shock
df_gdp <- df[df$shock == "GDPC1", ]
ggplot(df_gdp, aes(x = time, y = value)) +
  facet_grid(response ~ .) +
  geom_line(aes(color = quant)) +
  geom_hline(yintercept = 0, color="black") +
  theme_gray() +
  ggtitle("BVAR impulse response")

# Alternatively use dplyr
# library("dplyr")
# df_gdp <- df %>% filter(shock == "GDPC1") %>%
#   ggplot() # ...
  
# Change colour of the lines
ggplot(df_gdp, aes(x = time, y = value)) +
  facet_grid(response ~ .) +
  geom_line(aes(color = quant)) +
  geom_hline(yintercept = 0, color="black") +
  scale_color_manual(values = c("darkblue", "darkblue", "darkblue")) +
  theme_gray() +
  ggtitle("BVAR impulse response")

# Plot ribbons
df_ribbons <- tidyr::pivot_wider(df_gdp, names_from = quant)

ggplot(df_ribbons, aes(x = time)) + 
    facet_grid(response ~ shock) + 
    geom_line(aes(y = `50%`), colour = "darkblue", size = 1) + 
    geom_line(aes(y = `16%`), colour = "darkblue") + 
    geom_line(aes(y = `84%`), colour = "darkblue") + 
    geom_ribbon(aes(ymin = `16%`, ymax = `84%`), fill = "darkblue", alpha = 0.25) + 
    geom_hline(yintercept = 0, color="black") + 
    theme_gray() + 
    ggtitle("BVAR impulse response") + ylab("value")

