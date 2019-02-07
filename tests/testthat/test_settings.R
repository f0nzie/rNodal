library(testthat)

context("test setProjectEnvironment")

test_that("paths by setProjectEnvironment are all set", {
    res <- unlist(strsplit(system.file(package = "rNodal"), split = "/"))
    last_two <- paste(res[length(res)-1], res[length(res)], sep = "/")

    setProjectEnvironment()
    expect_true(grepl("rNodal/inst", project.env[["pkg.root"]]) ||
                    grepl("Temp", project.env[["pkg.root"]]) # needed by covr
                )
    expect_true(grepl("inst/extdata", project.env[["pkg.extdata"]]))
    print(project.env[["pkg.data"]])
    expect_true(grepl("inst/data", project.env[["pkg.data"]]) ||
                    grepl("Temp", project.env[["pkg.data"]]) # needed by covr
                )
    expect_true(grepl("inst/extdata", project.env[["data.folder"]]))
    expect_true(grepl("default.rda", project.env[["datafile.rda"]]))
})

test_that("saveToProjectEnv saves an object", {
    saveToProjectEnv("x", 123)
    xx <- readFromProjectEnv("x")
    expect_equal(xx, 123)
})
