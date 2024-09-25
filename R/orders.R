#' Get orders for one page
#'
#' @param url Website URL
#' @param page Page number
#' @param limit Number of item by page
#' @param token Bearer token
#' @param rate Rate limit, default to 50 per minute
#'
#' @keywords internal
#'
#' @importFrom httr2 request req_perform resp_body_json req_auth_bearer_token req_throttle
#'
get_orders_page <- function(url, page, limit, token, rate = 50 / 60) {
  # request page
  req_request <-
    request(paste0(url, "/api/runway/orders/?limit=", limit, "&page=", page)) |>
    req_auth_bearer_token(token = token) |>
    req_throttle(rate = rate) |>
    req_perform() |>
    resp_body_json()

  # return JSON data part
  req_request$data
}


#' Get all orders
#'
#' @param url Website URL
#' @param limit Number of item by page
#' @param token Bearer token
#' @param rate Rate limit, default to 50 per minute
#'
#' @return A dataframe with orders data
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom purrr map
#' @importFrom httr2 request req_perform resp_body_json req_auth_bearer_token req_throttle
#'
get_orders <- function(url,
                      limit = 100,
                      token,
                      rate = 50 / 60) {
  # init step -> get number of request to do
  param_request <-
    request(paste0(url, "/api/runway/orders?limit=1&page=1")) |>
    req_auth_bearer_token(token = token) |>
    req_throttle(rate = rate) |>
    req_perform() |>
    resp_body_json()

  # number of orders
  nb_orders <- param_request$meta$total

  # request all pages
  json_orders <- purrr::map(
    seq(1, nb_orders / limit + 1, 1),
    \(x)get_orders_page(
      url = url,
      page = x,
      limit = limit,
      token = token,
      rate = rate
    ),
    .progress = TRUE
  ) |>
    unlist(recursive = FALSE)

  # format
  tibble::tibble(col = json_orders) |>
    tidyr::unnest_wider(col)
}
