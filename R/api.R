# library(httr2)
# library(dplyr)
# library(purrr)
# library(fs)

pkgname <- "eudata"
cache_dir <- fs::path_abs(rappdirs::user_cache_dir(pkgname))

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

#' @export
get_topics <- function() {
  api |>
    req_url_path_append("themes.json") |>
    req_perform() |>
    resp_body_json() |>
    purrr::list_transpose() |>
    tibble::as_tibble()
}

#' @export
get_topic <- function(topic) {
  topic <- match.arg(topic,  names(topic_endpoints))
  topic <- topic_endpoints[[topic]]
  api |>
    req_url_path_append(topic)
}

#' @export
get_datasets <- function(api) {
  api |>
    req_url_path_append("datasets.json") |>
    req_cache(path = cache_dir) |>
    req_perform() |>
    resp_body_json() |>
    purrr::list_transpose() |>
    tibble::as_tibble()
}

#' @export
get_latest_files <- function(api) {
  files_json <-
    (get_datasets(api) |>
    dplyr::last())$files

  api |>
    req_url_path_append(files_json) |>
    req_cache(path = cache_dir) |>
    req_perform() |>
    resp_body_json()
}

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
      req_cache(path = cache_dir) |>
      req_perform(path = dest)
  }
