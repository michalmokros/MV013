setwd("~/Documents/school/MV013/seminar03")

test_vector <- c(131, 132, 135, rep(141, 4), 142, 143, rep(146,2), 151)
str(test_vector)

# s for sample

s_minimum <- function(v) {
  tmp <- v[1]
  for (i in 1:length(v)) {
    if (v[i] < tmp) {
      tmp <- v[i]
    }
  }
  return(tmp)
}

s_maximum <- function(v) {
  tmp <- v[1]
  for (i in v) {
    if (i > tmp) {
      tmp <- i
    }
  }
  return (tmp)
}

s_arithmetic_avg <- function(v) {
  tmp <- 0
  for (i in v) {
    tmp <- tmp + i
  }
  return (tmp/length(v))
}

s_quartile <- function(v, q) {
  sorted_v <- sort(v)
  border <- q * length(sorted_v)
  if (round(border / 2) != border / 2) {
    return (sorted_v[border])
  }
  return ((sorted_v[border] + sorted_v[border + 1]) / 2)
}