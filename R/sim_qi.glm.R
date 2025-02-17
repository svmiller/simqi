#' @keywords internal
#' @noRd

.sim_qi.glm <- function(mod, nsim = 1000, newdata, original_scale = TRUE, vcov = NULL) {


    fam <- mod$family$family
    the_link <- mod$family$link

    if(!(fam %in% c("binomial", "poisson"))) {
        stop("This function currently supports glm() functions where the family is binomial (logit, probit) or poisson (log). Feel free to raise an issue on the project's Github to extend this function forward.")
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

    smvrnorm(nsim, coef(mod), svcov) -> sim_params

    # if(missing(newdata)) {
    #     newdata <- model.frame(mod)
    # }

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
