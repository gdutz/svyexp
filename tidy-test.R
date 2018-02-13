tidy <- function(d, ...) {

    d$cntryname <- countrycode::countrycode(unique(d$cntryid), "iso3n", "country.name.en")

    if (unique(d$cntryid) == 826) { # UK braucht einen kürzeren Namen für Stata
        d$cntryname <- "UK"

    } else if (unique(d$cntryid) == 840) { # USA auch
        d$cntryname <- "USA"

    } else if (unique(d$cntryid) == 40) { # Für Österreich muss die edlevel3 Variable gebaut werden
        d <- dplyr::mutate(d, edlevel3 = ifelse(edcat6 == 1 | edcat6 == 2, 1,
                                                ifelse(edcat6 == 3 | edcat6 == 4, 2,
                                                       ifelse(edcat6 == 5 | edcat6 == 6 | edcat6 == 7, 3, NA))))
    }

    # Variablen bearbeiten (fehlende Werte, umwandeln in Faktoren, umordnen der Level)
    # todo
    d$imyrcat[d$imyrcat == 9] <- NA
    d$imyrcat <- relevel(as_factor(d$imyrcat), ref = 3)
    d$pared[d$pared > 3] <- NA
    d$homlang[d$homlang > 1] <- NA
    d$nativelang[d$nativelang > 1] <- NA

    return(d)
}
