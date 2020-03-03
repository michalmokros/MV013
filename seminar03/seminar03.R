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
  if (round(border) != border) {
    return (sorted_v[border])
  }
  return ((sorted_v[border] + sorted_v[border + 1]) / 2)
}

s_fiveNumberSalary <- function(v) {
  return (c(s_minimum(v), s_quartile(v, 0.25), s_quartile(v, 0.5), s_quartile(v, 0.75), s_maximum(v)))
}

s_variance <- function(v) {
  avg <- s_arithmetic_avg(v)
  tmp <- 0
  for (i in v) {
    tmp <- tmp + (i - avg)^2
  }
  return (tmp/(length(v) - 1))
}

s_stdDeviation <- function(v) {
  return (sqrt(s_variance(v)))
}

s_varianceOfAvg <- function(v) {
  return (s_variance(v) / length(v))
}

s_stdDeviationOfAvg <- function(v) {
  return (s_stdDeviation(v) / sqrt(length(v)))
}

#Cramer's estimate
s_coeffOfSkewness <- function(v) {
  avg <- s_arithmetic_avg(v)
  tmp <- 0
  for (i in v) {
    tmp <- tmp + (i - avg)^3
  }
  return (tmp/(length(v) * s_stdDeviation(v)^3))
}

#Cramer's estimate
s_coeffOfKurtosis <- function(v) {
  avg <- s_arithmetic_avg(v)
  tmp <- 0
  for (i in v) {
    tmp <- tmp + (i - avg)^4
  }
  return ((tmp/(length(v) * s_stdDeviation(v)^4)) - 3)
}

s_range <- function(v) {
  return (s_maximum(v) - s_minimum(v))
}

s_interQRange <- function(v) {
  return (s_quartile(v, 0.75) - s_quartile(v, 0.25))
}

s_innerBounds_max <- function(v) {
  return (s_quartile(v, 0.75) + 1.5 * s_interQRange(v))
}

s_innerBounds_min <- function(v) {
  return (s_quartile(v, 0.25) - 1.5 * s_interQRange(v))
}

s_outerBounds_Bl <- function(v) {
  Q1 <- s_quartile(v, 0.25)
  Q3 <- s_quartile(v, 0.75)
  return (Q1 - 3 * (Q3 - Q1))
}

s_outerBounds_Bu <- function(v) {
  Q1 <- s_quartile(v, 0.25)
  Q3 <- s_quartile(v, 0.75)
  return (Q3 + 3 * (Q3 - Q1))
}

s_quantileCoeffOfSkewness <- function(v) {
  Q1 <- s_quartile(v, 0.25)
  median <- s_quartile(v, 0.5)
  Q3 <- s_quartile(v, 0.75)
  return (((Q3 - median) - (median - Q1)) / (Q3 - Q1))
}