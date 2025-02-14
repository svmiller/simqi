#' @keywords internal
#' @noRd

.sim_qi.lm <- function(mod, nsim = 1000, newdata, original_scale = TRUE) {


if(original_scale == FALSE) {
    warning("There is no conversion of a link function to note in a linear model. This function ignores this argument for models of class 'lm'.")
}

siggies <- summary(mod)$sigma
nobs <- summary(mod)$df[1] + summary(mod)$df[2]
k <- summary(mod)$df[1]

siggies*sqrt((nobs-k)/rchisq(nsim,nobs-k))
list(a = smvrnorm(nsim, coef(mod), vcov(mod)),
     b = siggies*sqrt((nobs-k)/rchisq(nsim,nobs-k))) -> sim_params

if(missing(newdata)) {
    newdata <- model.frame(mod)
}

modmat <- model.matrix(terms(mod), newdata)

output <- tibble(y = numeric(), sim = numeric())

for (i in (1:nsim)) {

    yi <- modmat %*% sim_params$a[i,]

    simval <- rep(i, length(yi))
    hold_me <- data.frame(y = yi,
                          sim = i)

    output <- rbind(output, hold_me)
    output <- as_tibble(output)

}

return(output)

}
