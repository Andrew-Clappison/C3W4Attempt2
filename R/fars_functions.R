#' Read File
#'
#' This function is used to read in accident data files.
#'
#' @param filename A string that corresponds to the name of the file to be opened.
#'
#' @return The function returns the contents of the filename in the form of a data frame. If the filename does not exist, the user will be notified.
#'
#'
#' @import dplyr
#' @importFrom readr read_csv
#'
#'
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create Filename
#'
#' This function creates the file name for the accident data for a given year.
#'
#' @param year An integer or string that corresponds to a year.
#'
#' @return The corresponding filename as a string for a given year.
#'
#' @examples
#' make_filename("2013")
#' make_filename(2015)
#'
#'@export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Get data for Years
#'
#' This function is used to select the relevant columns of data from years specified by the user. This data is used by the function fars_summarize_years().
#'
#' @param years An integer or string or list of integers and strings that correspond to years.
#'
#' @return This function returns a list of data frames that have a column for month and year of each accident. If there is a problem reading in or using the data an error will be returned.
#'
#'
#' @importFrom dplyr mutate select
#'
#'
#'
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}


#' Summarize Accident Data
#'
#' This is a function that summarizes accident data by totaling the number of accidents in the country by month.
#'
#' @param years A integer or string or a list of integers or strings that are years.
#'
#' @return The function returns a tibble. There is a column for month, and for each year provided there is a column for the number of accidents in the US for that month.
#'
#'
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows group_by summarize
#'
#'
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Map of Accidents
#'
#' This function prints maps of US states with dots representing the location of accidents for a given state in a given year. The user provides the state and year that will be mapped.
#'
#' @param state.num An integer that corresponds to a state.
#' @param year An integer that corresponds to a year.
#'
#' @return This function plots a map of the locations of crashes for the chosen state in the chosen year.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
