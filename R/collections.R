#' Get number of items in collection
#'
#' @param url Website URL
#' @param collection Collection name
#'
#' @keywords internal
#'
#' @importFrom httr2 request req_perform resp_body_json
#'
get_collection_params <- function(url, collection) {
  # init step -> get number of request to do
  param_request <-
    request(paste0(
      url,
      "/api/collections/",
      collection,
      "/entries/?limit=1&page=1"
    )) |>
    req_perform() |>
    resp_body_json()

  # return
  param_request$meta$total
}

#' Get collection for one page
#'
#' @param url Website URL
#' @param collection Collection name
#' @param page Page number
#' @param limit Number of item by page
#'
#' @keywords internal
#'
#' @importFrom httr2 request req_perform resp_body_json
#'
get_collection_page <- function(url, collection, page, limit) {
  # init step -> get number of request to do
  req_request <-
    request(
      paste0(
        url,
        "/api/collections/",
        collection,
        "/entries/?limit=",
        limit,
        "&page=",
        page
      )
    ) |>
    req_perform() |>
    resp_body_json()

  # return JSON data part
  req_request$data
}

#' Get all entries in a collection
#'
#' @param url Website URL
#' @param collection Collection name
#' @param limit Number of item by page
#'
#' @return A dataframe with users data
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom httr2 request req_perform resp_body_json
#'
get_collection <- function(url, collection, limit = 10) {
  # init step -> get number of request to do
  nb_items <-
    get_collection_params(url = url, collection = collection)

  # request all pages
  json_collection <- lapply(
    seq(1, nb_items / limit + 1, 1),
    \(x)get_collection_page(
      url = url,
      collection = collection,
      page = x,
      limit = limit
    )
  ) |>
    unlist(recursive = FALSE)

  # format
  tibble::tibble(col = json_collection) |>
    tidyr::unnest_wider(col)
}
