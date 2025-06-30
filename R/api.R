#' @importFrom httr2 request req_url_path_append req_perform
#' @importFrom httr2 resp_body_json resp_header req_cache req_method
#' @importFrom dplyr last
#' @importFrom purrr list_transpose
#' @importFrom tibble as_tibble
#' @importFrom rappdirs user_cache_dir

pkgname <- "eudata"

cache_dir <- function()
  fs::path_abs(rappdirs::user_cache_dir(pkgname))

topic_endpoints <-
  list(
    "Coastal lines" = "coas",
    "Communes" = "communes",
    "Countries" = "countries",
    "Local Administrative Units" = "lau",
    "NUTS" = "nuts",
    "Postal codes" = "pcode",
    "Urban Audit" = "urau"
  )

api <- request("https://gisco-services.ec.europa.eu/distribution/v2/")

#' @title Retrieve Topics from API
#' @description Retrieves a list of topics from the specified API
#' endpoint `https://gisco-services.ec.europa.eu/distribution/v2/`.
#' @return A tibble the topics retrieved from the API.
#' @details This function sends a request to the given API endpoint
#' and parses the response to extract topic information.
#' @examples
#' # Retrieve topics from the default endpoint
#' topics <- get_topics()
#' @export
get_topics <- function() {
  api |>
    req_url_path_append("themes.json") |>
    req_perform() |>
    resp_body_json() |>
    purrr::list_transpose() |>
    tibble::as_tibble()
}


#' Retrieve Topic Information
#'
#' This function fetches the details of a specific topic based
#' on the provided topic name.
#'
#' @param topic A string representing the topic to retrieve.
#' @return A request object to the specific endpoint.
#' @examples
#' get_topic("Coastal lines")
#' get_topic("Postal")
#' @export
get_topic <- function(topic) {
  topic <- match.arg(topic,  names(topic_endpoints))
  topic <- topic_endpoints[[topic]]
  api |>
    req_url_path_append(topic)
}


#' Retrieve available datasets from an endpoint
#'
#' This function returns the list of available datasets as a tibble.
#' The columns of the tibble provide information about each dataset.
#'
#' @param api An endpoint
#'
#' @return A tibble of available datasets.
#'
#' @examples
#' get_topic("Coastal lines") |>
#'   get_datasets()
#'
#' @export
get_datasets <- function(api) {
  api |>
    req_url_path_append("datasets.json") |>
    req_cache(path = cache_dir()) |>
    req_perform() |>
    resp_body_json() |>
    purrr::list_transpose() |>
    tibble::as_tibble()
}

#' Retrieve the latest files from the API
#'
#' This function retrieves the files belonging to the
#' latest version of the given dataset.
#' When the dataset is not updated the cached version is returned.
#'
#' @param api An endpoint to the dataset.
#' @return A named list of files.
#' @examples
#' get_latest_files(get_topic("Postal"))$csv
#' @export
get_latest_files <- function(api) {
  files_json <-
    (get_datasets(api) |>
    dplyr::last())$files

  api |>
    req_url_path_append(files_json) |>
    req_cache(path = cache_dir()) |>
    req_perform() |>
    resp_body_json()
}

#' Get the content length of a file to download
#'
#' This function retrieves the content length of a file
#' to be downloaded from the API.
#'
#' @param api An endpoint to the dataset variants.
#' @param file_to_download A character vector of file names to download.
#' @return An integer vector of content lengths, named by the file names.
#' @examples
#' api <- get_topic("Postal")
#' files <- get_latest_files(api)$csv
#' purrr::map(
#'   files,
#'   get_content_length,
#'   api = api) |>
#'   tibble::as_tibble()
#' @export
get_content_length <- function(api, file_to_download) {
  api |>
    req_url_path_append(file_to_download) |>
    req_method("HEAD") |>
    req_perform() |>
    resp_header("content-length") |>
    as.integer() |>
    `names<-`(names(file_to_download))
}

#' Get content from the API
#'
#' This function retrieves the content from the API
#' and saves it to a file if `save_to_file` is TRUE.
#'
#' @param api An endpoint to the dataset.
#'
#' @param end_point A character vector of the endpoint to retrieve content from.
#'
#' @param save_to_file A logical value indicating whether to save the
#' content to a file
#'
#' @param dest A character vector specifying the destination file path.
#' If `save_to_file` is TRUE, this should be a valid file path.
#'
#' @return A `httr2` response object.
#' The content retrieved from the API is either the `body` of response or
#' the path to the file when `save_to_file` is TRUE.
#'
#' @examples
#' api <- get_topic("Postal")
#' files <- get_latest_files(api)$csv
#' file_to_download <- grep("_4326", files, value=TRUE)
#' response <- get_content(
#'   api,
#'   file_to_download,
#'   save_to_file = TRUE,
#'   dest = fs::file_temp(ext = "csv")
#' )
#' response$body
#' @export
get_content <-
  function(
    api,
    end_point,
    save_to_file = FALSE,
    dest = if (save_to_file) fs::path_file(end_point) else NULL
  ) {
    if (is.character(dest)) {
      dest <- fs::path_abs(dest)
    } else if (!is.null(dest)) {
      cli::cli_abort(
        "{.var dest} is either {.var NULL} or {.var character}, ",
        "got {.var {dest}}"
      )
    }
    api |>
      req_url_path_append(end_point) |>
      req_cache(path = cache_dir()) |>
      req_perform(path = dest)
  }
