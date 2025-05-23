% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/users.R
\name{get_users}
\alias{get_users}
\title{Get all users}
\usage{
get_users(url, limit = 100, token, rate = 50/60)
}
\arguments{
\item{url}{Website URL}

\item{limit}{Number of item by page}

\item{token}{Bearer token}

\item{rate}{Rate limit, default to 50 per minute}
}
\value{
A dataframe with users data
}
\description{
Get all users
}
