#' @keywords internal
#' @noRd

.sim_qi.lm <- function(mod, nsim = 1000, newdata, original_scale = TRUE, vcov = NULL) {

    # Assorted stops and warnings ----
    if(original_scale == FALSE) {
        warning("There is no conversion of a link function to note in a linear model. This function ignores this argument for models of class 'lm'.")
        }

    # Fill in newdata  ----
    dv <- all.vars(mod$call)[1]

    if(missing(newdata)) {
        newdata <- model.frame(mod)
    } else { # newdata is supplied, but I need to make sure the DV is there.
        stopifnot(is.data.frame(newdata))

        if(!(dv %in% colnames(newdata))) {

            newdata[[dv]] <- rep(0, nrow(newdata))

        }
    }

    # If you have a custom vcov, declare it now... ---

    if(is.null(vcov)) {
        svcov <- vcov(mod)
    } else {
        svcov <- vcov
    }

siggies <- summary(mod)$sigma
nobs <- summary(mod)$df[1] + summary(mod)$df[2]
k <- summary(mod)$df[1]

siggies*sqrt((nobs-k)/rchisq(nsim,nobs-k))
list(a = mvrnorm(nsim, coef(mod), svcov),
     b = siggies*sqrt((nobs-k)/rchisq(nsim,nobs-k))) -> sim_params


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
