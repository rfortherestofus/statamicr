#' Get users for one page
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
get_users_page <- function(url, page, limit, token, rate = 50 / 60) {
  # request page
  req_request <-
    request(paste0(url, "/api/users/?limit=", limit, "&page=", page)) |>
    req_auth_bearer_token(token = token) |>
    req_throttle(rate = rate) |>
    req_perform() |>
    resp_body_json()

  # return JSON data part
  req_request$data
}


#' Get all users
#'
#' @param url Website URL
#' @param limit Number of item by page
#' @param token Bearer token
#' @param rate Rate limit, default to 50 per minute
#'
#' @return A dataframe with users data
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom purrr map
#' @importFrom httr2 request req_perform resp_body_json req_auth_bearer_token req_throttle
#'
get_users <- function(url,
                      limit = 100,
                      token,
                      rate = 50 / 60) {
  # init step -> get number of request to do
  param_request <-
    request(paste0(url, "/api/users/?limit=1&page=1")) |>
    req_auth_bearer_token(token = token) |>
    req_throttle(rate = rate) |>
    req_perform() |>
    resp_body_json()

  # number of users
  nb_users <- param_request$meta$total

  # request all pages
  json_users <- purrr::map(
    seq(1, nb_users / limit + 1, 1),
    \(x)get_users_page(
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
  tibble::tibble(col = json_users) |>
    tidyr::unnest_wider(col)
}
