

test_that(
  "simple inputs work",
  {

    x1 <- rnorm(100)
    x2 <- rnorm(100)
    s  <- as.numeric(x1 + x2 + rnorm(100) > 0)
    t  <- runif(100, min=1, max=10)

    df = data.frame(time=t, status=s, x1=x1, x2=x2)
    df = as_sgb_data(df, time=time, status=status)

    tbl_df <- tibble::as_tibble(df)

    expect_equal(nrow(tbl_df), 100)
    expect_equal(ncol(tbl_df), 4)
    expect_is(tbl_df, 'tbl_df')
    expect_true(all(tbl_df$time == t))
    expect_true(all(tbl_df$status == s))

    expect_equal(
      sgb_params(max_depth=1),
      as_sgb_params(list(max_depth=1))
    )

    sgb_booster <- sgb_fit(
      sgb_df = df,
      params = sgb_params(max_depth=1),
      nrounds = NULL,
      early_stopping_rounds = 5,
      verbose = TRUE,
      print_every_n = 10
    )

    expect_error(
      predict(sgb_booster, new_data = df, eval_times = c(2,1)),
      regexp = 'eval_times'
    )

    expect_equal(
      sgb_booster$fit$params,
      list(
        max_depth = 1,
        objective = "survival:cox",
        eval_metric = "cox-nloglik",
        silent = 1
      )
    )

    expect_equal(
      sgb_booster$fit$feature_names,
      c("x1", "x2")
    )

    sprob = predict(sgb_booster, new_data = df)

    expect_true(all(sprob <= 1))
    expect_true(all(sprob > 0))

  }
)

test_that(
  "bad inputs get good errors",
  {
    x1 <- rnorm(100)
    x2 <- rnorm(100)
    s  <- as.numeric(x1 + x2 + rnorm(100) > 0)
    t  <- runif(100, min=1, max=10)

    bad_df = data.frame(time=t, status=s, x1=x1, x2=x2, bad_col = 'a')

    expect_error(
      as_sgb_data(bad_df, time=time, status=status),
      regexp = 'must be numeric'
    )

    bad_df = data.frame(time=t, sstatus=s, x1=x1, x2=x2)

    expect_error(
      as_sgb_data(bad_df, time=time, status=status),
      regexp = 'status not found'
    )

    bad_df = data.frame(time_shmime = t, status=s, x1=x1, x2=x2)

    expect_error(
      as_sgb_data(bad_df, time = time, status = status),
      regexp = 'time not found'
    )

    bad_df = data.frame(time = t, status=s)

    expect_error(
      as_sgb_data(bad_df, time = time, status = status),
      regexp = 'only contain time and status'
    )

  }
)
