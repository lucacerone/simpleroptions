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
          
          ### In strict mode the user can't define
          ### options for a package that haven't been 
          ### thought of by the package developer.
          if (self$strict) {
           options_names <- names(options)
           
           if (any(!options_names %in% self$allowed_options)) {
              stop("Some options names are not in the allowed set and strict mode is on.")
           }
          }
          
          ### Here I check whether there is any new option in the
          ### default values that doesn't appear in the options files.
          ### If so the values are set and a warning is issued.
          
          not_in_options_file <- self$allowed_options[! self$allowed_options %in% names(options)]
          
          for (opt in not_in_options_file) {
            options[[opt]] <- self$default_options[[opt]]
            warning("Added ", shQuote(opt), " to current options using its default value.") 
          }
          
          self$current_options <- options
          if (self$auto_save && length(not_in_options_file) > 0) {
            self$save()
          }
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
