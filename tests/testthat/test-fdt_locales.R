test_that("A vector can be obtained from the `fdt_locales_vec()` function", {

  expect_vector(fdt_locales_vec())
  expect_type(fdt_locales_vec(), "character")
  expect_length(fdt_locales_vec(), 574)
})

test_that("The `fdt_locales_lst` object has a particular structure", {

  expect_type(fdt_locales_lst, "list")
  expect_named(fdt_locales_lst, fdt_locales_vec())
  expect_equal(unlist(fdt_locales_lst, use.names = FALSE), fdt_locales_vec())
})

test_that("The `fdt_locales_lst` object can be used with `fdt()`", {

  for (i in seq_along(fdt_locales_vec())) {

      fmt_val <-
        fdt(
          input = "2018-07-04 22:05",
          locale = fdt_locales_lst[[i]]
        )

      expect_vector(fmt_val)
      expect_length(fmt_val, 1)
      expect_type(fmt_val, "character")
  }
})
