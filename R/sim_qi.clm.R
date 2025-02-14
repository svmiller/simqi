#' @keywords internal
#' @noRd

.sim_qi.clm <- function(mod, nsim = 1000, newdata, original_scale = TRUE) {


    if(missing(newdata)) {
        newdata <- model.frame(mod)
    }

    the_link <- mod$info$link

    if(!(the_link %in% c("logit", "probit"))) {

        stop("For now, this function works on just ordinal models with a link of either probit or logit.")
    }


    smvrnorm(nsim, coef(mod), vcov(mod)) -> simmies
    simmies <- as.data.frame(simmies)

    simmies$sim <- seq(1:nrow(simmies))

    # This is going to 1) replicate the simmies the number of times corresponding
    #  with the number of rows there are in newdata. The do.call() just rbinds them.

    sss <- do.call(rbind, replicate(n = nrow(newdata), expr = simmies,
                                    simplify = FALSE))

    # This just orders them for my own sanity.
    sss[order(sss$sim),] -> sss

    # Now, we're going to do the same that we did with simmies, this time with the
    #  newdata. We're just going to replicate them the number of times corresponding
    #  with the number of simulations we asked for (nsim)
    nnn <- do.call(rbind, replicate(n = nsim, expr = newdata, simplify = FALSE))

    # This is a fairly complicated operation to do a simple thing. To clarify:
    #  - Using sss, we're going to grab the column names with a "|" in them, and
    #    split by the "|".
    #  - Then, we're going to collapse them to a character vector (unlist), get just
    #    the unique values (unique), and convert them to a numeric.
    #  - Then, we'll just add an element to it that is the max of it, plus one.
    #    This identifies all unique values of the DV.
    ## x <- as.numeric(unique(unlist(strsplit(grep("\\|", colnames(sss), value=TRUE), split="\\|"))))
    ## x <- c(x, max(x)+1)
    # Alternatively...
    y_levs <- mod$y.levels

    # find intersection (i.e. just the stuff that we'd want for xb)
    col_intersection <- intersect(colnames(nnn), colnames(sss))
    # col_intersection

    # sss$xb <- rowSums(mapply(`*`, nnn[, col_intersection],
    #                          simmies[, col_intersection]))

    # create an xb that is the simple multiplication of the intersections.
    # This has the effect of multiplying our "set" values in newdata by the
    # simulated coefficients.
    xb <- rowSums(mapply(`*`, nnn[, col_intersection],
                         simmies[, col_intersection]))


    # Here's what this is going to do, which is the base R equivalent of a
    #  mutate_at(). We're going to create a custom function (funcy) that's just
    #  going to subtract the xbs from the thresholds. "Subbies" is going to take
    #  some of the code that I commented out above, simplifying identifying which
    #  columns in simmies have those thresholds. Then, apply() is going to take
    #  the sss column (matching the dimensions that will ultimately come) and apply
    #  the function. The 2 you see applies to the margin argument, indicating you
    #  you should apply this to the column.

    if(the_link == "logit") {

        funcy <- function(x) {plogis(x - xb)}

    } else if (the_link == "probit"){

        funcy <- function(x) {pnorm(x - xb)}
    }


    subbies <- grep("\\|", colnames(simmies), value=TRUE)
    data.frame(apply(sss[, subbies], 2, funcy)) -> aaa


    # Now, we're going to create our quantities of interest. This is going to create
    #  a blank data frame (ys) that is as many rows as sss with as many columns as
    #  there are unique values of y_levs.

    ylabs <- paste0("y", y_levs)
    data.frame(matrix(ncol= length(ylabs),
                      nrow=nrow(sss),
                      dimnames=list(NULL, ylabs))) -> ys

    # This is kind of fun. To populate ys, the first/lowest values of ys is going to
    # just be assigned from the first threshold we calculated. The second through
    # the length(ylabs)-1 value is going to be the value from aaa minus the one
    # before it. The last one is simply going to be 1 - whatever that is.

    ys[1] <- aaa[1]
    ncol_min1 <- ncol(aaa)

    for(i in 2:ncol_min1) {
        ys[i] <- aaa[i] - aaa[i-1]

    }

    ys[ncol(ys)] <- 1 - aaa[ncol(aaa)]

    if(original_scale == TRUE & the_link == "probit") {

        ys <- apply(ys, 2, function(x) {qnorm(x)})
    } else if(original_scale == TRUE & the_link == "logit") {

        ys <- apply(ys, 2, function(x) {log(x / (1 - x))})
    }

    as_tibble(data.frame(sim = sss$sim, ys)) -> output


    return(output)


}
