

test_that(
  "check_dots works",
  {

    expect_error(
      check_dots(.dots = list(a=1), valid_args = 'c'),
      regexp = 'unrecognized: a'
    )

  }
)
