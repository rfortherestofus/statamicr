#' Get number of items in collection
#'
#' @param url Website URL
#' @param collection Collection name
#' @param token Bearer token
#'
#' @keywords internal
#'
#' @importFrom httr2 request req_perform resp_body_json req_auth_bearer_token
#'
get_collection_params <- function(url, collection, token) {
  # init step -> get number of request to do
  param_request <-
    request(paste0(
      url,
      "/api/collections/",
      collection,
      "/entries/?limit=1&page=1"
    )) |>
    req_auth_bearer_token(token = token) |>
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
#' @param token Bearer token
#'
#' @keywords internal
#'
#' @importFrom httr2 request req_perform resp_body_json req_auth_bearer_token
#'
get_collection_page <-
  function(url, collection, page, limit, token) {
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
      req_auth_bearer_token(token = token) |>
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
#' @param token Bearer token
#'
#' @return A dataframe with users data
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom purrr map
#' @importFrom httr2 request req_perform resp_body_json
#'
get_collection <- function(url, collection, limit = 10, token) {
  # init step -> get number of request to do
  nb_items <-
    get_collection_params(url = url,
                          collection = collection,
                          token = token)

  # request all pages
  json_collection <-  purrr::map(
    seq(1, nb_items / limit + 1, 1),
    \(x)get_collection_page(
      url = url,
      collection = collection,
      page = x,
      limit = limit,
      token = token
    ),
    .progress = TRUE
  ) |>
    unlist(recursive = FALSE)

  # format
  tibble::tibble(col = json_collection) |>
    tidyr::unnest_wider(col)
}


#' Get ID and titles of items in collection
#'
#' @param url Website URL
#' @param collection Collection name
#' @param custom_fields Custom fiels added, separated by , (also for the start)
#' @param token Bearer token
#'
#' @return A dataframe with fields
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom purrr map_chr
#' @importFrom httr2 request req_perform resp_body_json req_auth_bearer_token
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
get_collection_list <-
  function(url, collection, custom_fields = "", token) {
    # filter collection on ID and title
    json_collection_list <- request(
      paste0(
        url,
        "/api/collections/",
        collection,
        "/entries?limit=10000&fields=id,title",
        custom_fields
      )
    ) |>
      req_auth_bearer_token(token = token) |>
      req_perform() |>
      resp_body_json()

    # format
    tibble::tibble(col = json_collection_list$data) |>
      tidyr::unnest_wider(col) |>
      dplyr::mutate(id = purrr::map_chr(.data$id,
                                        as.character))
  }
