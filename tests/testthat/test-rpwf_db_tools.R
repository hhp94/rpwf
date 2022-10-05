test_that("switching works", {
  expect_equal(id_col_switch_("cost_tbl"), "cost_id")
  expect_error(id_col_switch_("wflow_tbl"))
})
