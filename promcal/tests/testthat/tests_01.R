context("tell_me_how_drunk")

test_that("basic implementation is correct", {
  expect_equivalent(
    tell_me_how_drunk(
      age = 39,
      sex = "male",
      height = 190,
      weight = 87,
      drinking_time = as.POSIXct(c("2016-10-03 17:15:00", "2016-10-03 22:55:00")),
      drinks = c("massn" = 3, "schnaps" = 4)
    ),
    2.359, tolerance = 0.01)
  expect_equivalent(
    tell_me_how_drunk(
      age = 24,
      sex = "female",
      height = 160,
      weight = 54,
      drinking_time = as.POSIXct(c("2016-10-03 14:00:00", "2016-10-03 21:00:00")),
      drinks = list("hoibe" = 1, "schnaps" = 2)
    ),
    0.40, tolerance = 0.01)
  expect_equivalent(
    tell_me_how_drunk(
      age = 68,
      sex = "male",
      height = 169,
      weight = 84,
      drinking_time = as.POSIXct(c("2016-10-03 08:10:00", "2016-10-03 08:15:00")),
      drinks = c("schnaps" = 3)
    ),
    0.687, tolerance = 0.01)
  expect_equivalent(
    tell_me_how_drunk(
      age = 38,
      sex = "male",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 22:00:00")),
      drinks = c("hoibe" = 1)
    ),
    0)
})

# use input homogenization and match.arg for this:
test_that("interface is implemented flexibly", {
  expect_equal(
    tell_me_how_drunk(
      age = 38,
      sex = "male",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 22:00:00")),
      drinks = c("hoibe" = 1)
    ),
    tell_me_how_drunk(
      age = 38,
      sex = "m",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 22:00:00")),
      drinks = c("hoibe" = 1)
    ))
  expect_equal(
    tell_me_how_drunk(
      age = 38,
      sex = "M",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 22:00:00")),
      drinks = c("hoibe" = 1)
    ),
    tell_me_how_drunk(
      age = 38,
      sex = "m",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 22:00:00")),
      drinks = c("hoibe" = 1)
    ))
  #------------------
  expect_equal(
    tell_me_how_drunk(
      age = 38,
      sex = "M",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 22:00:00")),
      drinks = list(list("hoibe" = 1), "schnaps" = 2)
    ),
    tell_me_how_drunk(
      age = 38,
      sex = "m",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 22:00:00")),
      drinks = c("schnaps" = 2, "hoibe" = 1)
    ))
})

# anything under age < 16 not ok, hard liquor under age 18 not ok.
test_that("legal drinking age is checked", {
  expect_warning(
    tell_me_how_drunk(
      age = 14,
      sex = "female",
      height = 160,
      weight = 50,
      drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 22:00:00")),
      drinks = c("hoibe" = 7)),
    regexp = "illegal"
  )
  expect_warning(
    tell_me_how_drunk(
      age = 17,
      sex = "female",
      height = 160,
      weight = 50,
      drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 22:00:00")),
      drinks = c("schnaps" = 7)),
    regexp = "illegal"
  )
  expect_silent(
    tell_me_how_drunk(
      age = 17,
      sex = "female",
      height = 160,
      weight = 50,
      drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 22:00:00")),
      drinks = c("massn" = 2))
  )
})
# in order to make the tests better readable
testing_args <- list(age = 20, sex = "male", height = 180, weight = 60,
  drinking_time = as.POSIXct(c("2020-01-15 16:00:00", "2020-01-15 24:00:00")),
  drinks = c("hoibe" = 1))

testing_data <- do.call("create_plot_data", testing_args)



# note that we have to round the differences because otherwise there might be
# more than two different levels for the time and the promille due to
# the use of floating point numbers
test_that("plotting works", {
  # the results of tell_me_how_drunk and show_me_how_drunk should be consistent
  expect_equivalent(
    testing_data[testing_data$time == as.POSIXct("2020-01-15 24:00:00"),2],
    do.call("tell_me_how_drunk", testing_args))

  # do some tests on the structure of the premille-trajectory

  # nothing changes in the beginning
  expect_equivalent(testing_data[1,2], testing_data[13,2])
  # after 1 hour the body starts to break down the alcohol
  expect_equivalent(testing_data[1,1] + 5 * 60, testing_data[2,1])
  # time steps are correct
  expect_equivalent(testing_data[13,2] - 1/12 * 0.15, testing_data[14,2])
  # permille does not become negative
  expect_true(min(testing_data$promille) == 0)

  # the permille at the beginning for testing_data2 was 0.4544333
  # so when breaking down 0.15 per hour the permille should be 0 starting from row
  # 50 onwards (in the first hour nothing changes)
  expect_true(all(testing_data[50:97,2] == 0))
  # before that it should be > 0 but lower than 0.46
  expect_true(all(testing_data[1:37,2] > 0 & testing_data[1:37,2] < 0.46))
})

