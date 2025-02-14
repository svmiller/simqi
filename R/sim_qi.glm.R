#' @keywords internal
#' @noRd

.sim_qi.glm <- function(mod, nsim = 1000, newdata, original_scale = TRUE) {


    fam <- mod$family$family
    the_link <- mod$family$link


    # if(!(the_link %in% c("logit", "probit"))) {
    #
    #     stop("For now, this function works on just GLMs with a link of either probit or logit.")
    # }

    if(!(fam %in% c("binomial", "poisson"))) {
        stop("For now, this function assumes either binomial (probit, logit) or poisson.")
    }

    smvrnorm(nsim, coef(mod), vcov(mod)) -> sim_params

    if(missing(newdata)) {
        newdata <- model.frame(mod)
    }

    modmat <- model.matrix(terms(mod), newdata)

    output <- tibble(y = numeric(), sim = numeric())

    for (i in (1:nsim)) {

        yi <- modmat %*% sim_params[i,]

        simval <- rep(i, length(yi))
        hold_me <- data.frame(y = yi,
                              sim = i)

        output <- rbind(output, hold_me)
        output <- as_tibble(output)

    }

    if(original_scale == FALSE) {
        if(fam == "binomial" && the_link == "logit") {
            output$y <- plogis(output$y)
        } else if(fam == "binomial" && the_link == "probit") {
            output$y <- pnorm(output$y) # returns probability
        } else if(fam == "poisson" && the_link == "log") {
            output$y <- exp(output$y) # exponentiates logged lambda

        }

    } else { # original_scale == TRUE, don't do anything...

    }

    # if(original_scale == FALSE && the_link == "logit") {
    #     output$y <- plogis(output$y)
    # } else if(original_scale == FALSE && the_link == "probit") {
    #
    #     output$y <- pnorm(output$y)
    # }

    return(output)


}
