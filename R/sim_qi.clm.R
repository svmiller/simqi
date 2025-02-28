#' @keywords internal
#' @noRd
#'

.sim_qi.clm <- function(mod, nsim = 1000, newdata, original_scale = TRUE, vcov = NULL) {

    # Basic stops, for now ----

    if(!(clmlink %in% c("logit", "probit"))) {

        stop("For now, this function works on just ordinal models produced in the clm() function with a link of either probit or logit.")
    }

    if("nom.terms" %in% names(mod)) {
        stop("This function currently does not support nominal effects specified in this formula. Feel free to raise an issue on the project's Github if you can do this for me.")
    }

    # Extract some information from the model first. ----
    y_levs <- mod$y.levels
    ylabs <- paste0("y", y_levs)

    clmlink <-  mod$info$link
    dv <- all.vars(mod$call$formula)[1]

    # Fill in newdata  ----
    if(missing(newdata)) {
        newdata <- model.frame(mod)
    } else { # newdata is supplied, but I need to make sure the DV is there.

        stopifnot(is.data.frame(newdata))

        if(!(dv %in% colnames(newdata))) {

            newdata[[dv]] <- rep(0, nrow(newdata))

        }
    }

    modmat <- model.matrix(terms(mod), newdata)
    modmat <- modmat[, !grepl("Intercept", colnames(modmat))] # remove intercept from thingie

    # If you have a custom vcov, declare it now... ---

    if(is.null(vcov)) {
        svcov <- vcov(mod)
    } else {
        svcov <- vcov
    }


    # Simulate the important stuff ----
    Sims <- mvrnorm(nsim, coef(mod), svcov)

    cnSims <- colnames(Sims)
    simA <- Sims[, grepl("\\|", cnSims)]
    simB <- Sims[, !grepl("\\|", cnSims)]


    data.frame(matrix(nrow = 0, ncol = length(ylabs)+1,
                      dimnames = list(NULL, c("sim", ylabs)))) -> output

    for (i in (1:nsim)) {

        xb <- modmat %*% simB[i,]
        alphas <- do.call(rbind,
                          replicate(n = length(xb),
                                    expr = simA[i, ],
                                    simplify = FALSE))


        data.frame(matrix(ncol = length(ylabs),
                          nrow = length(xb),
                          dimnames=list(NULL, ylabs))) -> ys

        aaa <- sweep(alphas, 1, xb, "-")

        ys[1] <- plogis(aaa[, 1])

        ncol_min1 <- ncol(aaa)

        for(j in 2:ncol_min1) {
            ys[j] <- plogis(aaa[, j]) - plogis(aaa[, j-1])

        }

        ys[ncol(ys)] <- 1 - rowSums(ys[, 1:ncol(ys)-1])
        ys$sim <- i

        output <- rbind(output, ys)
        output <- as_tibble(output)


    }

    output <- output[c("sim", setdiff(names(output), "sim"))]
    return(output)

}

