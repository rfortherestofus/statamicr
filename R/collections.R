#' Get number of items in collection
#'
#' @param url Website URL
#' @param collection Collection name
#' @param token Bearer token
#' @param rate Rate limit, default to 50 per minute
#'
#' @keywords internal
#'
#' @importFrom httr2 request req_perform resp_body_json req_auth_bearer_token req_throttle
#'
get_collection_params <-
  function(url, collection, token, rate = 50 / 60) {
    # init step -> get number of request to do
    param_request <-
      request(paste0(
        url,
        "/api/collections/",
        collection,
        "/entries/?limit=1&page=1"
      )) |>
      req_auth_bearer_token(token = token) |>
      req_throttle(rate = rate) |>
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
#' @param rate Rate limit, default to 50 per minute
#'
#' @return A dataframe with users data
#' @export
#'
#' @importFrom httr2 request req_perform resp_body_json req_auth_bearer_token req_throttle
#'
get_collection_page <-
  function(url,
           collection,
           page,
           limit,
           token,
           rate = 50 / 60) {
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
      req_throttle(rate = rate) |>
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
#' @param rate Rate limit, default to 50 per minute
#' @param start_page start page
#'
#' @return A dataframe with users data
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom purrr map
#' @importFrom httr2 request req_perform resp_body_json req_throttle
#'
get_collection <-
  function(url,
           collection,
           limit = 10,
           token,
           rate = 50 / 60,
           start_page = 1) {
    # init step -> get number of request to do
    nb_items <-
      get_collection_params(
        url = url,
        collection = collection,
        token = token,
        rate = rate
      )

    # request all pages
    json_collection <-  purrr::map(seq(start_page, nb_items / limit + 1, 1),
                                   \(x)try(get_collection_page(
                                     url = url,
                                     collection = collection,
                                     page = x,
                                     limit = limit,
                                     token = token,
                                     rate = rate
                                   ))
                                   ,
                                   .progress = TRUE) |>
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
#' @param rate Rate limit, default to 50 per minute
#'
#' @return A dataframe with fields
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom purrr map_chr
#' @importFrom httr2 request req_perform resp_body_json req_auth_bearer_token req_throttle
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
get_collection_list <-
  function(url,
           collection,
           custom_fields = "",
           token,
           rate = 50 / 60) {
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
      req_throttle(rate = rate) |>
      req_perform() |>
      resp_body_json()

    # format
    tibble::tibble(col = json_collection_list$data) |>
      tidyr::unnest_wider(col) |>
      dplyr::mutate(id = purrr::map_chr(.data$id,
                                        as.character))
  }
