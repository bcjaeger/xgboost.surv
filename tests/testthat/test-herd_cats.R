
test_that(
  "correct inputs work",
  {

  df <- data.frame(
    x = factor(rep(letters[1:2], 50)),
    y = 1:100
  )

  one_hot_df <- cat_spread(df)

  expect_equal(
    names(one_hot_df),
    c('x_a', 'x_b','y')
  )

  no_hot_df <- cat_gather(one_hot_df, factor_levels = list(x=c('a','b')))

  expect_true(all(df$x==no_hot_df$x))

  expect_true(all(df$y==no_hot_df$y))

  df_nocats <- df
  df_nocats[, 'x'] <- NULL

  expect_equal(cat_spread(df_nocats), df_nocats)

  df_train <- data.frame(
    x = c("A","B","C","D"),
    y = 1:4
  )

  expect_warning(cat_spread(df_train), regexp = ': x')


  df_train <- data.frame(
    x = c("A","B","C","D","D"),
    y = 1:5
  )

  df_test <- data.frame(
    x = c("A","A","B","C","C"),
    y = 2:6
  )

  df_test <- cat_transfer(to = df_test, from = df_train)

  df_test_spread <- cat_spread(df_test)

  expect_true(all(df_test$x_D==0))

  expect_equal(
    names(df_test_spread),
    names(cat_spread(df_train))
  )

  df_bad_test <- data.frame(
    x = c("A","A","B","C"),
    y = 2:5,
    z = c('a','b','c','d')
  )

  df_bad_train <- data.frame(
    x = c("A","A","B","C"),
    y = 2:5,
    z = c('a','b','c','d')
  )

  expect_error(
    object = cat_transfer(to = df_bad_test, from = df_train),
    regexp = "from: z"
  )

  expect_error(
    object = cat_transfer(to = df_test, from = df_bad_train),
    regexp = "to: z"
  )


})
