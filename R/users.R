#' Get users for one page
#'
#' @param url Website URL
#' @param page Page number
#' @param limit Number of item by page
#'
#' @keywords internal
#'
#' @importFrom httr2 request req_perform resp_body_json
#'
get_users_page <- function(url, page, limit) {
  # init step -> get number of request to do
  req_request <-
    request(paste0(url, "/api/users/?limit=", limit, "&page=", page)) |>
    req_perform() |>
    resp_body_json()

  # return JSON data part
  req_request$data
}


#' Get all users
#'
#' @param url Website URL
#' @param limit Number of item by page
#'
#' @return A dataframe with users data
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom httr2 request req_perform resp_body_json
#'
get_users <- function(url, limit = 100) {
  # init step -> get number of request to do
  param_request <-
    request(paste0(url, "/api/users/?limit=1&page=1")) |>
    req_perform() |>
    resp_body_json()

  # number of users
  nb_users <- param_request$meta$total

  # request all pages
  json_users <- lapply(seq(1, 5874 / limit + 1, 1),
                       \(x)get_users_page(
                         url = url,
                         page = x,
                         limit = limit
                       )) |>
    unlist(recursive = FALSE)

  # format
  tibble::tibble(col = json_users) |>
    tidyr::unnest_wider(col)
}
