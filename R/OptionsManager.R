##### Helper functions

user_folder <- function() {
  folder <- Sys.getenv("HOME")
  if (nchar(folder) == 0)
    folder = "~"
  normalizePath(file.path(folder, paste0(".", getPackageName())), mustWork = FALSE)
}

user_options_path <- function(){
  file.path(user_folder(), paste0(getPackageName(), ".json"))
}

##### OptionsManager Class
OptionsManager <-
  R6::R6Class(
    "OptionsManager",
    ##### Public Variables
    public = list(
      default_options = NULL,
      auto_save = NULL,
      strict = NULL,
      permissions = NULL,
      verbose = FALSE,
      current_options = NULL,
      options_path = NULL,
      allowed_options = NULL,
      
      ##### Constructor
      initialize = function(default_options = list(),
                            options_path = user_options_path(),
                            permissions = "644",
                            strict = TRUE,
                            auto_save = TRUE,
                            verbose = FALSE) {
        self$default_options <- default_options
        self$auto_save <- auto_save
        self$strict <- strict
        self$permissions <- permissions
        self$verbose <- verbose
        self$options_path <- options_path
        
        self$initialize_options()
      },
      initialize_options = function() {
        self$allowed_options <- names(self$default_options)
        
        if (file.exists(self$options_path)) {
          options <- jsonlite::fromJSON(self$options_path)
          
          if (self$strict) {
           options_names <- names(options)
           
           if (any(!options_names %in% self$allowed_options)) {
              stop("Some options names are not in the allowed set and strict mode is on.")
           }
          }
          
          self$current_options <- options
        } else {
          self$current_options <- self$default_options
          if (self$auto_save) {
            self$save()
          }
        }
        return(TRUE)
      },
      
      save = function(filename = self$options_path) {
        
        options_dir <- dirname(normalizePath(filename, mustWork = F))
        if (!file.exists(options_dir)) {
          dir.create(
            path = options_dir,
            showWarnings = self$verbose,
            recursive = TRUE
          )
        } else {
          dirinfo <- file.info(options_dir)
          if (!dirinfo[["isdir"]]) {
            warning(options_dir,
                    " exists but is not a directory. Options won't be saved.")
          }
        }
        json <- jsonlite::toJSON(self$current_options, pretty = T)
        readr::write_file(json, path = filename)
      },
      set = function(option_name, option_value) {
        package_options <- names(self$default_options)
        if (self$strict) {
          # In strict mode you can only set parameters that have been
          # provided by the package author
          if (!option_name %in% package_options) {
            stop(
              "You can't set option \" ",
              option_name,
              ".\nAllowed values are ",
              paste0("'", package_options,"'", collapse = ","), "."
            )
          }
        }
        
        self$current_options[[option_name]] <- option_value
        
        if (self$auto_save) {
            self$save()
        }
      },
      get = function(name) {
       if (name %in% self$allowed_options) {
        self$current_options[[name]]
       } else {
        stop("Option ", shQuote(name), " not found.")
       }
      }
    )
  )
