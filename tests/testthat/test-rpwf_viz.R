test_that("Testing if the dm object imported all the tables", {
  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  con = dummy_con_(tmp_dir = tmp_dir)

  dm_obj = rpwf_dm_obj(con)

  expect_true(all(sort(colnames(dm_obj)) == sort(names(rpwf_schema()))))
})
