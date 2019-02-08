library(testthat)


context("append_to_rda() if file does not exist")

rda_file <- "appended.Rdata"
if (file.exists(rda_file)) file.remove(rda_file)

test_that("if Rdata file does not exist, create empty", {
    a <- 123
    append_to_rdata(a, file = rda_file)
    expect_true(file.exists(rda_file))
})

test_that(
    "add a vector object and matrix appends to existing Rdata", {
        set.seed(12345)
        m <- matrix(1:50, nrow=10, ncol=5)
        append_to_rdata(m, file = rda_file)
        rm(m)
        load(file = rda_file)
        expect_equal(class(m), "matrix")
        expect_equal(class(a), "numeric")
        expect_equal(a, 123)
        m1 <- matrix(1:50, nrow=10, ncol=5)
        expect_equal(m, m1)
    })

test_that(
    "a dataframe appends to existing Rdata", {
        df1 <- data.frame(p = 1:7, q = 6:12)
        append_to_rdata(df1, file = rda_file)
        rm(df1)
        load(file = rda_file)
        expect_equal(class(df1), "data.frame")
        df0 <- data.frame(p = 1:7, q = 6:12)
        expect_equal(df1, df0)
    })

test_that(
    "a dataframe appends to existing Rdata", {
        df2 <- data.frame(p = 1:10, q = 11:20)
        append_to_rdata(df2, file = rda_file)
        rm(df2) # no more in global environment
        load(file = rda_file)
        expect_equal(class(df2), "data.frame")
        df0 <- data.frame(p = 1:10, q = 11:20)
        expect_equal(df2, df0)
    })

test_that(
    "number of objects match", {
        load(file = rda_file)
        expect_equal(length(ls()), 4)
    })

test_that(
    "a list is appended to existing Rdata", {
        a_list <- list(a = 100, b = 200, c =300)
        append_to_rdata(a_list, file = rda_file)
        rm(a_list)
        load(file = rda_file)
        expect_equal(class(a_list), "list")
        a_list_0 <- list(a = 100, b = 200, c =300)
        expect_equal(a_list, a_list_0)
    }
)

test_that(
    "number of objects match to 5", {
        load(file = rda_file)
        expect_equal(length(ls()), 5)
    })

test_that(
    "an object cannot be overwritten", {
        a <- c(1, 2, 3, 4, 5, 6, 8)
        append_to_rdata(a, file = rda_file)
        rm(a)
        load(file = rda_file)
        expect_equal(a, c(1, 2, 3, 4, 5, 6, 8))
    }
)

test_that(
    "number of objects match to 5", {
        load(file = rda_file)
        expect_equal(length(ls()), 5)
    })

test_that(
    "an object is overwritten", {
        a <- c(1, 2, 3, 4, 5, 6, 8)
        append_to_rdata(a, file = rda_file)
        rm(a)
        load(file = rda_file)
        a0 <- c(1, 2, 3, 4, 5, 6, 8)
        expect_equal(a, a0)
    }
)

test_that(
    "number of objects match to 5", {
        load(file = rda_file)
        expect_equal(length(ls()), 5)
    })


test_that(
    "write multiple object at once", {
        x <- 100
        y <- c(200, 300, 400)
        z <- list(one = x, three = y)
        append_to_rdata(x, y, z, file = rda_file)
        rm(x,y,z)
        load(file = rda_file)
        expect_equal(x, 100)
        expect_equal(y, c(200, 300, 400))
        expect_equal(z, list(one = x, three = y))
    }
)

test_that(
    "number of objects match to 8", {
        load(file = rda_file)
        expect_equal(length(ls()), 8)
    })

# context("add_object_to_rda() if file does not exist")
#
# rda_file <- "growingRda.Rdata"
# if (file.exists(rda_file)) file.remove(rda_file)
#
# test_that("if Rdata file does not exist, create empty", {
#     a <- 123
#     add_object_to_rda(a, rda_file = "growingRda.Rdata")
#     expect_true(file.exists("growingRda.Rdata"))
# })
#
# test_that(
#     "add a vector object and matrix appends to existing Rdata", {
#         set.seed(12345)
#         m <- matrix(1:50, nrow=10, ncol=5)
#         add_object_to_rda(m, rda_file, overwrite = TRUE)
#         rm(m)
#         load(file = rda_file)
#         expect_equal(class(m), "matrix")
#         expect_equal(class(a), "numeric")
#         expect_equal(a, 123)
#         m1 <- matrix(1:50, nrow=10, ncol=5)
#         expect_equal(m, m1)
# })
#
# test_that(
#     "a dataframe appends to existing Rdata", {
#         df1 <- data.frame(p = 1:7, q = 6:12)
#         add_object_to_rda(df1, rda_file, overwrite = TRUE)
#         rm(df1)
#         load(file = rda_file)
#         expect_equal(class(df1), "data.frame")
#         df0 <- data.frame(p = 1:7, q = 6:12)
#         expect_equal(df1, df0)
#     })
#
# test_that(
#     "a dataframe appends to existing Rdata", {
#         df2 <- data.frame(p = 1:10, q = 11:20)
#         add_object_to_rda(df2, rda_file, overwrite = TRUE)
#         rm(df2) # no more in global environment
#         load(file = rda_file)
#         expect_equal(class(df2), "data.frame")
#         df0 <- data.frame(p = 1:10, q = 11:20)
#         expect_equal(df2, df0)
#     })
#
# test_that(
#     "number of objects match", {
#         load(file = rda_file)
#         expect_equal(length(ls()), 4)
#     })
#
# test_that(
#     "a list is appended to existing Rdata", {
#         a_list <- list(a = 100, b = 200, c =300)
#         add_object_to_rda(a_list, rda_file, overwrite = TRUE)
#         rm(a_list)
#         load(file = rda_file)
#         expect_equal(class(a_list), "list")
#         a_list_0 <- list(a = 100, b = 200, c =300)
#         expect_equal(a_list, a_list_0)
#     }
# )
#
# test_that(
#     "number of objects match to 5", {
#         load(file = rda_file)
#         expect_equal(length(ls()), 5)
#     })
#
# test_that(
#     "an object cannot be overwritten", {
#         a <- c(1, 2, 3, 4, 5, 6, 8)
#         add_object_to_rda(a, rda_file, overwrite = FALSE)
#         rm(a)
#         load(file = rda_file)
#         expect_equal(a, 123)
#     }
# )
#
# test_that(
#     "number of objects match to 5", {
#         load(file = rda_file)
#         expect_equal(length(ls()), 5)
#     })
#
# test_that(
#     "an object is overwritten", {
#         a <- c(1, 2, 3, 4, 5, 6, 8)
#         add_object_to_rda(a, rda_file, overwrite = TRUE)
#         rm(a)
#         load(file = rda_file)
#         a0 <- c(1, 2, 3, 4, 5, 6, 8)
#         expect_equal(a, a0)
#     }
# )
#
# test_that(
#     "number of objects match to 5", {
#         load(file = rda_file)
#         expect_equal(length(ls()), 5)
#     })
#
#
#
#
# context("add_object_to_rda() if file does exist")
#
# rda_file <- "growingRda.Rdata"
#
# test_that("if Rdata file does not exist, create empty", {
#     a <- 123
#     add_object_to_rda(a, rda_file = "growingRda.Rdata")
#     expect_true(file.exists("growingRda.Rdata"))
# })
#
# test_that(
#     "number of objects match", {
#         load(file = rda_file)
#         expect_equal(length(ls()), 5)
#     })
#
# test_that(
#     "add a vector object and matrix appends to existing Rdata", {
#         set.seed(12345)
#         m <- matrix(1:50, nrow=10, ncol=5)
#         add_object_to_rda(m, rda_file, overwrite = TRUE)
#         rm(m)
#         load(file = rda_file)
#         expect_equal(class(m), "matrix")
#         expect_equal(class(a), "numeric")
#         if (length(a) == 1) expect_equal(a, 123)
#         m1 <- matrix(1:50, nrow=10, ncol=5)
#         expect_equal(m, m1)
#     })
#
# test_that(
#     "a dataframe appends to existing Rdata", {
#         df1 <- data.frame(p = 1:7, q = 6:12)
#         add_object_to_rda(df1, rda_file, overwrite = TRUE)
#         rm(df1)
#         load(file = rda_file)
#         expect_equal(class(df1), "data.frame")
#         df0 <- data.frame(p = 1:7, q = 6:12)
#         expect_equal(df1, df0)
#     })
#
# test_that(
#     "number of objects match", {
#         load(file = rda_file)
#         expect_equal(length(ls()), 5)
#     })
#
# test_that(
#     "a dataframe appends to existing Rdata", {
#         df2 <- data.frame(p = 1:10, q = 11:20)
#         add_object_to_rda(df2, rda_file, overwrite = TRUE)
#         rm(df2) # no more in global environment
#         load(file = rda_file)
#         expect_equal(class(df2), "data.frame")
#         df0 <- data.frame(p = 1:10, q = 11:20)
#         expect_equal(df2, df0)
#     })
#
# test_that(
#     "number of objects match", {
#         load(file = rda_file)
#         expect_equal(length(ls()), 5)
#     })
#
# test_that(
#     "a list is appended to existing Rdata", {
#         a_list <- list(a = 100, b = 200, c =300)
#         add_object_to_rda(a_list, rda_file, overwrite = TRUE)
#         rm(a_list)
#         load(file = rda_file)
#         expect_equal(class(a_list), "list")
#         a_list_0 <- list(a = 100, b = 200, c =300)
#         expect_equal(a_list, a_list_0)
#     }
# )
#
# test_that(
#     "number of objects match to 5", {
#         load(file = rda_file)
#         expect_equal(length(ls()), 5)
#     })
#
# test_that(
#     "an object cannot be overwritten", {
#         a <- c(1, 2, 3, 4, 5, 6, 8)
#         add_object_to_rda(a, rda_file, overwrite = FALSE)
#         rm(a)
#         load(file = rda_file)
#         if (length(a) > 1) expect_equal(a, c(1, 2, 3, 4, 5, 6, 8))
#         else expect_equal(a, 123)
#     }
# )
#
# test_that(
#     "number of objects match to 5", {
#         load(file = rda_file)
#         expect_equal(length(ls()), 5)
#     })
#
# test_that(
#     "an object is overwritten", {
#         a <- c(1, 2, 3, 4, 5, 6, 8)
#         add_object_to_rda(a, rda_file, overwrite = TRUE)
#         rm(a)
#         load(file = rda_file)
#         a0 <- c(1, 2, 3, 4, 5, 6, 8)
#         expect_equal(a, a0)
#     }
# )
#
# test_that(
#     "number of objects match to 5", {
#         load(file = rda_file)
#         expect_equal(length(ls()), 5)
#     })







# context("test stopif()")
#
# test_that("stopif all false returns NULL", {
#     expect_equal(NULL, stopif(FALSE))
#     expect_equal(NULL, stopif(FALSE, FALSE))
#     expect_equal(NULL, stopif(FALSE, FALSE, FALSE))
# })
#
# test_that("stopif causes error if condition is TRUE", {
#     expect_error(stopif(TRUE))
#     expect_error(stopif(TRUE, FALSE))
#     expect_error(stopif(TRUE, FALSE, TRUE))
# })
#
# test_that("stopif causes error if condition is TRUE", {
#     expect_equal(NULL, stopif())
# })
#
#
# context("test get_list_examples()")
#
# test_that("examples is empty", {
#     expect_error(get_list_examples()) # must specify the package
#     examples <- get_list_examples(aPackage = "rNodal")
#     # print(class(examples))
#     if (length(examples) == 0) expect_equal(unlist(examples), character())
#
# })
#
# context("shift()")
#
# test_that("", {
#     x <- c(1, 2, 3)
#     sx <- shift(x, 1)
#     # print(sx)
#     expect_true(identical(sx, c(NA, 1, 2)))
#     sx <- shift(x, -1)
#     # print(sx)
#     expect_true(identical(sx, c(2, 3, NA)))
#     sx <- shift(x, 0)
#     expect_true(identical(sx, c(1, 2, 3)))
#     expect_error(shift(x))
# })
