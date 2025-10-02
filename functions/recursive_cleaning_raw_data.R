##### RECURSIVE CLEANING #####
# assumes clean_text(df) is already defined and libraries/dictionaries are loaded


recursive_clean_raw_data <- function(input_dir = "data/raw_data",
                                     output_dir = "data/cleaned_data",
                                     pattern = "\\.csv$"){
  
  # make sure the output folder exists. if not, create one
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # recursively collect all files to process
  files <- list.files(
    input_dir, 
    recursive = TRUE, 
    full.names = TRUE, # true = return full paths, not just filenames
    pattern = pattern) # only files ending with .csv (as specified above)
  
  # normalize the input root path
  # - makes it absolute
  # - forces forward slashes ("/") for cross-platform consistency
  # - mustWork = FALSE → won’t error if the folder doesn’t exist yet
  in_root <- normalizePath(input_dir, winslash = "/", mustWork = FALSE)
  
  # loop through each file
  for (f in files) {
    message("Processing: ", f)   # print which file we’re on
    
    # STEP 1: read CSV
    dat <- read.csv(
      f,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8-BOM",
      row.names = NULL
    )
    
    # drop any columns with missing/blank names
    dat <- dat[, names(dat) != "" & !is.na(names(dat)), drop = FALSE]
    
    # STEP 2: apply cleaning function
    dat2 <- clean_text(dat)
    
    # STEP 3: build the output path
    # normalize the file path
    f_norm <- normalizePath(f, winslash = "/", mustWork = FALSE)
    
    # strip off the input_dir part, leaving just the relative path
    # e.g. "C:/proj/data/raw_data/2021/2021-Jan.csv"
    #  -> "2021/2021-Jan.csv"
    rel <- sub(paste0("^", in_root, "/?"), "", f_norm)
    
    # stick that relative path under the output_dir
    # e.g. "data/cleaned_data/2021/2021-Jan.csv"
    out_path <- file.path(output_dir, rel)
    
    # STEP 4: create subfolders if needed
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    
    # STEP 5: write cleaned CSV
    write.csv(dat2, out_path, row.names = FALSE)
    message("Saved: ", out_path, "\n")
  }
}

recursive_clean_raw_data()
