
test_that(
  "good and bad inputs work",
  {

    df <- data.frame(a=1:100, b=100:1, c=rep(letters[1:2],50))
    folds <- xgb_folds(data = df, nfolds = 10)

    expect_is(
      object = folds ,
      class = 'list'
    )

    expect_equal(length(folds), 10)

    folds <- xgb_folds(data = df, nfolds = 100)

    expect_equal(length(folds), 100)

    expect_error(
      xgb_folds(data = df, nfolds = 101),
      regexp = 'must be >='
    )

  }
)
