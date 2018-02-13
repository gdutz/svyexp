piaac_mean <- function(datapath, countries, variable, FUN = piaac_tidy, ...) {
    pb <- txtProgressBar(min = 0, max = length(countries), style = 3)

    results <- list()

    for (i in seq_along(countries)) {

        d <- piaac_read(datapath, countries[[i]])
        names(d) <- tolower(names(d))
        d <- dplyr::select(d, variable, cntryid, vemethod, dplyr::contains("spfwt"))

    }

    close(pb)
    rm(pb)
}
