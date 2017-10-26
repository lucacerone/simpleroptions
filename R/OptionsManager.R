##### Helper functions

get_current_folder <- function() {
  normalizePath(".", mustWork = F)
}

get_user_folder <- function() {
  user_folder <- Sys.getenv("HOME")
  if (nchar(user_folder) == 0)
    user_folder = "~"
  normalizePath(file.path(user_folder, paste0(".", getPackageName())), mustWork = FALSE)
}

get_system_folder <- function() {
  normalizePath(file.path("", "usr", "share", "R", getPackageName()),
                mustWork = FALSE)
}

##### OptionsManager Class
OptionsManager <-
  R6::R6Class(
    "OptionsManager",
    ##### Public Variables
    public = list(
      default_options = NULL,
      search_directories = NULL,
      auto_save = NULL,
      strict = NULL,
      encrypted = NULL,
      permissions = NULL,
      allow_subset = NULL,
      verbose = FALSE,
      current_options = NULL,
      current_options_path = NULL,
      options_filename = NULL,
      level = NULL,
      
      ##### Constructor
      initialize = function(default_options = list(),
                            search_directories = list(project = get_current_folder(),
                                                      user = get_user_folder(),
                                                      global = get_system_folder()),
                            options_filename = paste0(getPackageName(), ".json"),
                            auto_save = TRUE,
                            strict = TRUE,
                            encrypted = FALSE,
                            permissions = list(project = "644",
                                               user = "644",
                                               global = "644"),
                            allow_subset = TRUE,
                            verbose = FALSE) {
        self$default_options <- default_options
        self$search_directories <- search_directories
        self$auto_save <- auto_save
        self$strict <- strict
        self$encrypted <- encrypted
        self$permissions <- permissions
        self$allow_subset <- allow_subset
        self$verbose <- verbose
        self$options_filename <- options_filename
        self$level <- private$get_level()
        
        self$current_options_path <-
          private$options_full_path(self$level)
        
        self$set_current_options()
        
        if (self$auto_save) {
          self$save_options_files()
        }
      },
      set_current_options = function() {
        if (file.exists(self$current_options_path)) {
          level_options <-  jsonlite::toJSON(self$current_options_path)
        } else {
          level_options <- list()
        }
        
        self$current_options <-
          modifyList(self$default_options, level_options)
      },
      save_options_files = function() {
        json <- jsonlite::toJSON(self$current_options, pretty = T)
        
        options_dir <- self$search_directories[[self$level]]
        if (!file.exists(options_dir)) {
          dir.create(
            path = options_dir,
            showWarnings = self$verbose,
            recursive = TRUE,
            mode = "700"
          )
        } else {
          dirinfo <- file.info(options_dir)
          if (!dirinfo[["isdir"]]) {
            warning(options_dir,
                    " exists but is not a directory. Options won't be saved.")
          }
        }
        
        readr::write_file(json, path = self$current_options_path)
      }
    ),
    private = list(
      options_full_path = function(level) {
        path <- self$search_directories[[level]]
        filename <-
          file.path(path, self$options_filename)
        return(filename)
      },
      get_level = function() {
        # Navigate the search path.
        # If an options file is found then return the corresponding level, 
        # otherwise fallback to user.
        for (level in names(self$search_directories)) {
          options_full_path <-
            private$options_full_path(level)
          if (file.exists(options_full_path)) {
            return(level)
          }
        }
        
        return("user")
      }
    )
  )
