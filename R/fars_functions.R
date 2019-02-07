#' Read fars file
#'
#' This is a function that will search for and read-in a csv file based on a given filename. If the file exists, it will
#' read the csv file and convert it into a tibble/data.frame
#'
#' @param filename A character string that the function will search for.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @return If the filename exists, this function returns the csv as a tibble/data.frame. If the file does not exist,
#' the function will instead will return with a message saying that the file does not exist.
#'
#' @examples
#' \dontrun{fars_read("accident_2013.csv.bz2")}
#' \dontrun{fars_read("accident_2015.csv.bz2")}
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create the filename
#'
#' This function will take digits representing the year and insert it as part of a new string
#' to compose a filename.
#'
#' @param year Consecutive digits in either string or numeric format that will be inserted to a filename string template
#'
#' @return This function returns a string
#'
#'
#' @examples
#' \dontrun{make_filename("2017")}
#' \dontrun{make_filename(2017)}
#' \dontrun{make_filename(17)}
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read multiple fars files
#'
#' This function will read in multiple years worth of files and return a list of tibble/data.frames of just the columns
#' MONTH and year.
#'
#' @param years A vector containing one or more years. The years can be numeric or string.
#' @inheritParams make_filename
#' @inheritParams fars_read
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @return If the file of a given year in the vector of years exists, then this function will return a list of
#' tibble/data.frames of columns MONTH and year from each file. If a file does not exist with one of the listed years,
#' then it will print a warning saying that that year is invalid and return a NULL object.
#'
#'
#' @examples
#' \dontrun{fars_read_years(c(2014, 2015))}
#' \dontrun{fars_read_years(c(2014))}
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(dat$MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}


#' Summarise the number of accidents by month and year
#'
#' This function will create a summary crosstabulating of the number of observations by month and year in a tibble/data.frame.
#'
#' @param years A vector containing one or more years. The years can be numeric or string.
#' @inheritParams fars_read_years
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @return This function returns a tibble/data.frame containing the number of observations for all given years for each month.
#' The given years are the column names while each row represents monthly counts.
#'
#' @examples
#' \dontrun{fars_summarize_years(c(2014, 2015))}
#' \dontrun{fars_summarize_years(c(2014))}
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(dat_list$year, dat_list$MONTH) %>%
                dplyr::summarize(n = dat_list$n()) %>%
                tidyr::spread(dat_list$year, dat_list$n)
}

#' Map Locations by State
#'
#' This function will map the longitude and latitude of each observation within a given state and year.
#'
#' @param state.num The state code that is in numeric or string format
#' @param year A single year in numeric or string format
#' @inheritParams make_filename
#' @inheritParams fars_read
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @return If the given state code exists and the filename of the given year exists, this function will return a map displaying
#' the state boundary and all accidents in that given year as points. If the state number does not exist within the dataset,
#' a message will print saying that the state number is invalid. If there are no observations for a given year with a state,
#' the function will return a message reporting that there are no accidents to plot.
#'
#' @examples
#' \dontrun{fars_map_state(53, 2013)}
#' \dontrun{fars_map_state(53, 2014)}
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, data$STATE == state.num)
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
