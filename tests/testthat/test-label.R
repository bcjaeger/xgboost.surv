
test_that(
  "correct inputs work",
  {

    slab = lbl_survival(
      time = c(1,2,3),
      status = c(1,0,1)
    )

    expect_equal(slab, c(1,-2,3))

    blab = lbl_binary(response = factor(x = c("A","A","B")))

    expect_equal(blab, c(0,0,1))

    mlab = lbl_multi(
      response = factor(x = c("A","C","B"), levels = c("C","A","B"))
    )

    expect_equal(mlab, c(1,0,2))



  }
)
