# quick summary function to grab what we need from Stan outputs
summarise_stan <- function(
        x,
        pars = c("alpha", "beta", "p"),
        probs = c(0.025, 0.5, 0.975)
) {
    
    # grab all draws
    x <- as.matrix(x)
    
    # pull out target parameters
    x <- x[, pars]
    
    # summarise and return
    tibble(
        par = pars,
        lower = apply(x, 2, quantile, probs = probs[1]),
        mid = apply(x, 2, quantile, probs = probs[2]),
        upper = apply(x, 2, quantile, probs = probs[3])
    )
    
}
