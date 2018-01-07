#' OptionsManager
#'
#' Create an OptionsManager object, to easily manage an option file to be used in your functions
#' and scripts.
#'
#' @param options_path the path to the option file. For script I recommend to use something like "./option_file.json",
#' for R packages something like "~/.package_name/option_file.json"
#' @param default_options this should be a list containing the default values to use when creating an options file.
#' @param permissions a string containing the permissions to apply to the file. It defaults to "600"
#' @param strict whether only option names appearing in "default_options" should be accepted or not. It defaults to FALSE,
#' but I recommend to set it to TRUE if you are using simpleroptions in a package.
#' @param auto_save whether to save automatically the option file when you change an option value. It defaults to TRUE
#' @param verbose whether to show some information messages or not. It defaults to FALSE.
#'
#' @return an "OptionsManager" R6 object
#' @export
#'
#' @examples
#' 
#' exOpt <- OptionsManager(
#'   options_path = "./example_options.json",
#'   default_options = list(
#'     image_directory = ".",
#'     image_extension = ".jpg"
#'     )
#' )
#' 
#' # You can modify the values in the options file using the `set()` method
#' exOpt$set(image_directory = "images",
#'           image_extension = ".png")
#'           
OptionsManager <- function(options_path,
                           default_options = list(),
                           permissions = "600",
                           auto_save = TRUE,
                           strict = FALSE,
                           verbose = FALSE) {
  invisible(
    OptionsManager_R6$new(
      options_path = options_path,
      default_options = default_options,
      permissions = permissions,
      auto_save = auto_save,
      strict = strict,
      verbose = verbose
    )
  )
}
