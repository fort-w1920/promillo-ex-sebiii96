#' @import checkmate testthat
#' @importFrom ggplot2 qplot
NULL
#' calculates blood alcohol concentration according to Widmark and Whatson
#'
#' @param age a number between 10 and 110
#' @param sex either "male" or "female"
#' @param height a number between 100 and 230
#' @param weight a number between 40 and 300
#' @param drinking_time a sorted posixct vector of length 2 that contains the
#' beginning and the end of the drinking time
#' @param drinks a named vector whose names are a subset of "massn", "hoibe",
#' "wein" and "schnaps" and whose entries contain the amount of consumed units
#' @return the estimated blood alcohol concentration
#'
#' @example tell_me_how_drunk(
#' age = 38,
#' sex = "male",
#' height = 190,
#' weight = 134,
#' drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 22:00:00")),
#' drinks = c("hoibe" = 1))
#' @export
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
  drinking_time, drinks) {
  drinks <- unlist(drinks) # homogenize inputs
  alcohol_drunk <- get_alcohol(drinks)
  bodywater <- get_bodywater(sex, age, height, weight, drinks)
  get_permille(alcohol_drunk, bodywater, drinking_time)
}

# utilities --------------------------------------------------------------------

#' get the amount of pure alcohol someone drank
#' @param drinks a named vector whose names are a subset of "massn", "hoibe",
#' "wein" and "schnaps" and whose entries contain the amount of consumed units
#' @return the output is the amount of pure alcohol drank (in ml)
#' @keyword Internal
get_alcohol <- function(drinks) {
  assert_subset(names(drinks), choices = c("massn", "hoibe", "wein", "schnaps"),
    empty.ok = FALSE)
  assert_numeric(drinks, lower = 0)

  volume <- c(
    "massn"   = 1000,
    "hoibe"   = 500,
    "wein"    = 200,
    "schnaps" = 40)
  alcohol_concentration <- c(
    "massn"   = 0.06,
    "hoibe"   = 0.06,
    "wein"    = 0.11,
    "schnaps" = 0.4)
  alcohol_density <- 0.8

  sum(drinks * volume[names(drinks)] *
      alcohol_concentration[names(drinks)] * alcohol_density)
}

#' get the body water and give error message in case of illegal drinking
#'
#' @param sex either male or female
#' @param age a number between 10 and 110
#' @param height a height between 100 and 230
#' @param weight a weight between 40 and 300
#' @param drinks the drinks that were consumed - the reason why this is to be
#' passed as an argument is only for a possible warning in case of illegal
#' drinking behvaiour
#' @keyword Internal

get_bodywater <- function(sex = c("male", "female"), age, height, weight,
  drinks) {
  sex <- tolower(sex)
  sex <- match.arg(sex)

  assert_number(age, lower = 10, upper = 110)
  if (age < 16 | age > 90) {
    warning("illegal")
  }

  drinks_schnaps <- !is.na(drinks["schnaps"]) && (drinks["schnaps"] > 0)

  if (age >= 16 && age < 18 && drinks_schnaps) warning("illegal")

  assert_number(height, lower = 100, upper = 230)
  assert_number(weight, lower = 40, upper = 300)

  coef <- if (sex == "male") {
    c(2.447, -0.09516, 0.1074, 0.3362)
  } else {
    c(0.203, -0.07, 0.1069, 0.2466)
  }
  t(coef) %*% c(1, age, height, weight)
}

#' gets the blood alcohol concentratio in permille
#'
#' @description a subfunction of tell_me_how_drunk that calculates
#' @inheritParams tell_me_how_drunk
#' @param alcohol_drunk a number that indicates the amount of pure alcohol
#' drunk (in ml) - should be the output of get_alcohol
#' @param bodywater water in the body in liter
#'  - should be the output of get_bodywater
#' @keyword Internal

get_permille <- function(alcohol_drunk, bodywater, drinking_time){
  assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)

  alcohol_density <- 0.8
  blood_density <- 1.055
  permille <- alcohol_density * alcohol_drunk / (blood_density * bodywater)

  partylength <- difftime(drinking_time[2], drinking_time[1], units = "hours")
  sober_per_hour <- 0.15
  # sobering up starts only after one hour & you can't be more sober than 0:
  max(0, permille  - (max(0, partylength - 1) * sober_per_hour))
}


#' plots the blood alcohol concentration in permille over time
#'
#' @inheritParams tell_me_how_drunk
#' @description plots the blood alcohol concentration
#' @return the ggplot object
#' @export

show_me_how_drunk <- function(age, sex, height, weight, drinking_time, drinks) {
  plot_data <- do.call("create_plot_data", as.list(match.call())[-1])
  # note that one of the reasons why we split up that function is that
  # tests on the plot_data are easier than plots on the ggplot object

  qplot(x = plot_data[,"time"], y = plot_data[,"promille"],
    main = "blood alcohol concentration over time",
    xlab = "time",
    ylab = "promille")
}

#' create the data required for plotting the BAC over time
#'
#' @inheritParams tell_me_how_drunk
#' @return a data-frame that contains the columns time and promille
#' @keyword Internal

# because all the functions get_<BLA> have their own input checks we don't
# have to do this again
create_plot_data <- function(age, sex, height, weight, drinking_time, drinks) {
  assert_posixct(drinking_time, sorted = TRUE, len = 2)
  partylength <- difftime(drinking_time[2], drinking_time[1], units = "mins")
  assert_true(unclass(partylength) >= 5)

  time_points <- seq(from = drinking_time[1], to = drinking_time[2], by = 5 * 60)
  # note that in case the time-difference between the points is not divisible
  # by 5 it only goes to the highest time that is below the end of drinking

  # the 'trick' we use is to set the beginning and the end of drinking to be
  # the same time so no alcohol is broken down

  bodywater <- get_bodywater(sex, age, height, weight, drinks)
  alcohol <- get_alcohol(drinks)
  permille_total <- get_permille(alcohol, bodywater, rep(drinking_time[1],2))

  permille_points <- rep(permille_total, times = min(12, length(time_points)))
  if (length(time_points) >= 13) {
    permille_points <- c(permille_points,
      seq(from = permille_total, by = -1/12 * 0.15,
        length.out = length(time_points) - 12))
  }

  permille_points <- vapply(X = permille_points, FUN.VALUE = numeric(1),
    FUN = function(x) max(x, 0))

  data.frame("time" = time_points, "promille" = permille_points)
}

