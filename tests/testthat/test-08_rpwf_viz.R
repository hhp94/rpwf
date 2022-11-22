test_that("Testing if the dm object imported all the tables", {
  board <- pins::board_temp()
  tmp_dir <- withr::local_tempdir()
  db_name <- paste(tmp_dir, "db.SQLite", sep = "/")
  db_con <- rpwf_connect_db(db_name, board)

  dm_obj <- rpwf_dm(db_con)
  # colnames of dm_obj should be the table names stored in rpwf_schema()
  expect_true(all(sort(colnames(dm_obj)) == sort(names(rpwf_schema()))))
})
