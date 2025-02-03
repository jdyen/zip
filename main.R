# code to run a zero-inflated Poisson model in Stan and brms

# load some packages
library(readr)
library(dplyr)
library(brms)
library(rstan)
library(jagsUI)
library(ggplot2)
library(ragg)

# load some helper functions 
source("R/utils.R")

# load the data
birds <- read_csv("data/data.csv")

# compile the Stan version of the model
stan_mod <- stan_model("src/zip.stan")

# prepare a data object
stan_data <- list(
    y = birds$y,
    x = birds$x,
    n = nrow(birds)
)

# and sample from the model
iter <- 2000
warmup <- 1000
draws <- sampling(
    object = stan_mod,
    data = stan_data,
    iter = iter,
    warmup = warmup
)

# now fit a brms version
draws_brms <- brm(
    y ~ x,
    data = birds,
    family = zero_inflated_poisson,
    iter = iter,
    warmup = warmup
)

# fit a JAGS version
draws_jags <- jags(
    data = stan_data, 
    inits = \() list(p = 1, a = rnorm(1), b = rnorm(1)),
    parameters.to.save = c("p", "a", "b"),
    model.file = "src/zip-jags.txt", 
    n.chains = 4,
    n.thin = 5,
    n.iter = 5 * iter, 
    n.burnin = warmup
)

# extract and plot posterior intervals for p, a, b for all 
#   four models
true_values <- tibble(
    par = c("alpha", "beta", "p"),
    value = c(0.24, 0.37, 0.86)
)

# extract model estimates
estimates_stan <- summarise_stan(draws)
estimates_brms <- summarise_stan(draws_brms, c("b_Intercept", "b_x", "zi"))
estimates_jags <- summary(draws_jags)[c("a", "b", "p"), c("2.5%", "50%", "97.5%")]

# convert brms value to probability of non-zero rather than prob of zero
estimates_brms <- estimates_brms |>
    mutate(
        lower = ifelse(par == "zi", 1 - lower, lower),
        mid = ifelse(par == "zi", 1 - mid, mid),
        upper = ifelse(par == "zi", 1 - upper, upper)
    )

# combine everything
estimates <- true_values |>
    mutate(lower = NA, mid = value, upper = NA, model = "Actual") |>
    select(-value) |>
    bind_rows(
        estimates_stan |>
            mutate(model = "Stan")
    ) |>
    bind_rows(
        estimates_brms |>
            mutate(
                model = "brms",
                par = c("alpha", "beta", "p")
            )
    ) |>
    bind_rows(
        estimates_jags |>
            as_tibble() |>
            rename(
                lower = `2.5%`,
                mid = `50%`,
                upper = `97.5%`
            ) |>
            mutate(
                par = c("alpha", "beta", "p"),
                model = "JAGS",
                par = c("alpha", "beta", "p")
            ) |>
            select(par, lower, mid, upper, model)
    )

# and plot these
p_estimates <- estimates |> 
    mutate(
        model = factor(
            model,
            levels = c("Actual", "JAGS", "brms", "Stan")
        )
    ) |>
    ggplot(aes(y = mid, x = model, ymin = lower, ymax = upper, col = model)) +
    geom_point(size = 2) +
    geom_errorbar(linewidth = 0.9, width = 0.6) +
    scale_colour_brewer(palette = "Set2") +
    xlab("") +
    ylab("Parameter estimate") +
    facet_wrap( ~ par) +
    theme(legend.position = "none")

# save to file
ggsave(
    filename = "outputs/estimates.png",
    plot = p_estimates,
    device = agg_png,
    width = 7,
    height = 4,
    dpi = 300,
    units = "in"
)
