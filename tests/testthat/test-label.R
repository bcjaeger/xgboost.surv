
test_that(
  "correct inputs work",
  {

    slab = sgb_label(
      time = c(1,2,3),
      status = c(1,0,1)
    )

    expect_equal(slab, c(1,-2,3))

    expect_equal(get_time(slab), c(1,2,3))

    expect_equal(get_status(slab), c(1, 0, 1))

  }
)

test_that(
  "incorrect inputs throw errors",
  {

    expect_error(
      sgb_label(
        time = c(1,2,rep(-3,10)),
        status = c(1,0,rep(1,10))
      ),
      regexp = 'time vector'
    )

    expect_error(
      sgb_label(
        time = c(1,2,rep(3,10)),
        status = c(1,0,rep(2,10))
      ),
      regexp = 'status values'
    )
  }
)
