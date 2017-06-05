# out <- tryCatch(
#     {
#         message("this is the try part")
#         rprojroot::find_rstudio_root_file(path = "..")
#     },
#     error = function(e) {
#         message(e)
#         return(NA)
#     },
#     warning = function(w) {
#         message(w)
#         return(NULL)
#     },
#     finally = {
#         message("finally")
#     }
# )
#
# out
