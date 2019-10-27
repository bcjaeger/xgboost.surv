
test_that(
  "normal inputs work and bad inputs get good error messages",
  {

    library(prodlim)
    library(survival)
    library(pec)

    # predict 1/2 for everyone, all the time
    surv_probs <- matrix(
      data = rep(1/2, 1000),
      nrow = 100,
      ncol = 10
    )

    # make random time & status values
    time = runif(100, min = 1, max = 10)
    status = c(rep(1, 50), rep(0, 50))

    # use all the possible times
    eval_times = seq(1, 10, length.out = 10)

    # censor data has one variable that is
    # related to time values
    cens_data = data.frame(x1 = time + rnorm(100, sd = 1/2))

    # note that the Brier score is 1/4, as expected, when
    # no adjustment is applied for censoring.
    expect_equal(
      eval_bscore(
        surv_probs = surv_probs,
        label = sgb_label(time, status),
        eval_times = eval_times,
        cens_data = cens_data,
        cens_model = 'marginal',
        scale = FALSE
      ),
      0.25
    )

    expect_equal(
      eval_bscore(
        surv_probs = surv_probs,
        label = sgb_label(time, status),
        eval_times = eval_times,
        cens_model = 'marginal',
        scale = FALSE
      ),
      0.25
    )

    expect_equal(
      eval_bscore(
        surv_probs = surv_probs,
        time = time,
        status = status,
        eval_times = eval_times,
        cens_data = cens_data,
        cens_model = 'marginal',
        scale = FALSE
      ),
      0.25
    )

    scaled_bs <- eval_bscore(
      surv_probs = surv_probs,
      time = time,
      status = status,
      eval_times = eval_times,
      cens_data = cens_data,
      cens_model = 'marginal',
      scale = TRUE
    )

    expect_true(scaled_bs < 0)

    expect_error(
      eval_bscore(
        surv_probs = surv_probs[,-1],
        label = sgb_label(time, status),
        eval_times = eval_times,
        cens_data = cens_data,
        cens_model = 'marginal',
        scale = FALSE
      ),
      regexp = 'is not equal to'
    )

    expect_error(
      eval_bscore(
        surv_probs = surv_probs,
        label = sgb_label(time, status),
        eval_times = NULL,
        cens_data = cens_data,
        cens_model = 'marginal',
        scale = FALSE
      ),
      regexp = 'eval_times'
    )

    expect_error(
      eval_bscore(
        surv_probs = surv_probs,
        time = time[-1],
        status = status,
        eval_times = eval_times,
        cens_data = cens_data,
        cens_model = 'marginal',
        scale = FALSE
      ),
      'time and status should be equal'
    )

    expect_error(
      eval_bscore(
        surv_probs = surv_probs,
        time = time,
        status = status,
        eval_times = eval_times,
        cens_data = cens_data[-1, , drop=FALSE],
        cens_model = 'marginal',
        scale = FALSE
      ),
      'is not equal to length'
    )

  }
)
