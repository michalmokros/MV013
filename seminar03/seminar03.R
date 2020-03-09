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
    return (sorted_v[ceiling(border)])
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

s_quartileCoeffOfSkewness <- function(v) {
  Q1 <- s_quartile(v, 0.25)
  median <- s_quartile(v, 0.5)
  Q3 <- s_quartile(v, 0.75)
  return (((Q3 - median) - (median - Q1)) / (Q3 - Q1))
}

#distribution is symmetric or positively/negatively skewed
isSymmOrSkewed <- function(v) {
  b1 <- s_coeffOfSkewness(v)
  if (b1 > 0) {
    return ("positively skewed")
  }
  if (b1 < 0) {
    return ("negatively skewed")
  }
  return ("symmetric")
}

#distribution is mesokurtic, leptokurtic or platykurtic
calculateKurtic <- function(v) {
  b2 <- s_coeffOfKurtosis(v)
  if (b2 > 0) {
    return ("leptokurtic")
  }
  if (b2 < 0) {
    return ("platykurtic")
  }
  return ("mesokurtic")
}

#distribution is between quartiles symmetric or positively/negatively skewed
calculateDistrBetweenQuartiles <- function(v) {
  b1Q <- s_quantileCoeffOfSkewness(v)
  if (b1Q > 0) {
    return ("positively skewed")
  }
  if (b1Q < 0) {
    return ("negatively skewed")
  }
  return ("symmetric")
}

calculateAllMeasures <- function(v) {
  cat("x_min: ", s_minimum(v))
  cat("\nx_max: ", s_maximum(v))
  cat("\narithmetic avg.: ", s_arithmetic_avg(v))
  cat("\nmedian: ", s_quartile(v, 0.5))
  cat("\nQ1: ", s_quartile(v, 0.25))
  cat("\nQ3: ", s_quartile(v, 0.75))
  cat("\n5-number salary: ", s_fiveNumberSalary(v))
  cat("\nvariance: ", s_variance(v))
  cat("\nstandard deviation: ", s_stdDeviation(v))
  cat("\nvariance of arith. avg.: ", s_varianceOfAvg(v))
  cat("\nstd. deviation of arith. avg.: ", s_stdDeviationOfAvg(v))
  cat("\ncoefficient of skewness: ", s_coeffOfSkewness(v))
  cat("\ncoefficient of kurtosis: ", s_coeffOfKurtosis(v))
  cat("\nrange: ", s_range(v))
  cat("\ninterquartile range: ", s_interQRange(v))
  cat("\ninner bounds -> max: ", s_innerBounds_max(v), ", min: ", s_innerBounds_min(v))
  cat("\nouter bounds -> Bl: ", s_outerBounds_Bl(v), ", Bu: ", s_outerBounds_Bu(v))
  cat("\nquartile coefficient of skewness: ", s_quartileCoeffOfSkewness(v))
  cat("\nDistribution is: ", isSymmOrSkewed(v))
  cat("\nDistribution is: ", calculateKurtic(v))
  cat("\nDistribution is between quartiles: ", calculateDistrBetweenQuartiles(v))
}


body <- read.table('body.txt', header=T)
missing <- is.na(body)
sum(missing)
body <- na.omit(body)
calculateAllMeasures(as.vector(body$hip.C))
calculateAllMeasures(body[body$sex == 'f', ]$hip.C)
calculateAllMeasures(body[body$sex == 'm', ]$hip.C)