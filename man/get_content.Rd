% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{get_content}
\alias{get_content}
\title{Get content from the API}
\usage{
get_content(
  api,
  end_point,
  save_to_file = FALSE,
  dest = if (save_to_file) fs::path_file(end_point) else NULL
)
}
\arguments{
\item{api}{An endpoint to the dataset.}

\item{end_point}{A character vector of the endpoint to retrieve content from.}

\item{save_to_file}{A logical value indicating whether to save the
content to a file}

\item{dest}{A character vector specifying the destination file path.
If `save_to_file` is TRUE, this should be a valid file path.}
}
\value{
A `httr2` response object.
The content retrieved from the API is either the `body` of response or
the path to the file when `save_to_file` is TRUE.
}
\description{
This function retrieves the content from the API
and saves it to a file if `save_to_file` is TRUE.
}
\examples{
api <- get_topic("Postal")
files <- get_latest_files(api)$csv
file_to_download <- grep("_4326", files, value=TRUE)
response <- get_content(
  api,
  file_to_download,
  save_to_file = TRUE,
  dest = fs::file_temp(ext = "csv")
)
response$body
}
