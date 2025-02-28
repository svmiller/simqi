#' @keywords internal
#' @noRd

.sim_qi.logistf <- function(mod, nsim = 1000, newdata, original_scale = TRUE, vcov = NULL) {

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

    mvrnorm(nsim, coef(mod), svcov) -> sim_params


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

        output$y <- plogis(output$y)


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
