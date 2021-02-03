## Summary Function
SummarizeNeurons <- function(folderdir) {
  
  # create Results folder
  resultsdir <- file.path(folderdir, "Results")
  dir.create(resultsdir, showWarnings = FALSE)
  
  # read filelist
  folderlist <- list.files(path = folderdir, include.dirs = F, recursive = F, pattern = ".txt$") # pattern only includes txt-files
  csv_marker = FALSE
  if (length(folderlist) == 0) { # NeuronJ or Fiji-Update 2020 saves results as .csv
    folderlist <- list.files(path = folderdir, include.dirs = F, recursive = F, pattern = ".csv$")
    csv_marker = TRUE
    if(length(folderlist) == 0) {
      stop("This folder doesn't seem to contain any NeuronJ measurements files")
      }
  }
  
  # define variable names in Results dataframes
  Axon_results <- data.frame(
    Image = NA, #1
    total_axon = NA, #2 
    primary_axon = NA, #3
    total_branch_length = NA, #4
    total_filo_length = NA, #5
    max_filo_length = NA, #6
    mean_filo_length = NA, #7
    Number_branch = NA, #8
    Number_filo = NA, #9
    Number_branch_filo = NA, #10
    branches_by_primary_axon = NA, #11
    filopodia_by_primary_axon = NA, #12 
    branch_filopodia_by_branch_length = NA, #13
    Number_axons = NA, #14
    Total_filo = NA, #15
    total_filopodia_by_total_length = NA #16
  ) [numeric(0), ]
  
  Neurite_results <- data.frame(
    Image = NA, 
    total_neurite = NA, 
    mean_neurite = NA, 
    Number_neurites = NA, 
    Number_neurite_branches = NA,
    Number_neurite_filo = NA,
    Branches_per_neurite = NA, 
    Filo_per_neurite = NA,
    total_length = NA, 
    axon_by_neurite = NA
  )[numeric(0), ]
  
  Leftover <- data.frame(Image = NA, Number_others = NA) [numeric(0), ]
  
  
  # open results per cell
  for (i in 1:length(folderlist)) {
   temp = NULL
    # if loading doesn't work, skip it
    fail <- tryCatch(error = function(cnd) {
        warning(paste(folderlist[i], "could not be read:", cnd$message))
        cnd
      },
      
      # load as csv or txt depending on which files are detected in folder
      if(csv_marker) {
        temp <- read.table(paste(folderdir, "/", folderlist[i], sep = ""), header = TRUE, sep = ",")
      } else {
        temp <- read.table(paste(folderdir, "/", folderlist[i], sep = ""), header = TRUE, sep = "\t")
      }
      )
      
    if(inherits(fail, "error")) next
    
    # Test for correct format of csv-files > else skip this file
    else if(is.null(temp$Type)) {  
    Axon_results[i, 1] <- folderlist[i]   
    Neurite_results[i, 1] <- folderlist[i]      
    Leftover[i, 1] <- folderlist[i]
    warning(paste(folderlist[i], "seems to have a different structure than NeuronJ Tracing measurements.\n"))
    next
    }
    
    ## empty vectors for lengths in each Category
    Axon <- vector()
    Branch <- vector()
    Filopodium <- vector()
    Branch_filopodium <- vector()
    Neurite_filopodium <- vector()
    Neurite <- vector()
    Neurite_branch <- vector()
    Other <- vector()
    k <- 1                         # reset counter
    
    ## store lengths for each category in individual vector, store uncategorized in remaining
    for (k in 1:nrow(temp)) {
      # alternative option: switch-statement
      # switch(tolower(temp$Type[k]), 
      #        axon             = Axon[k] <- temp[k, 6],
      #        primary          = Primary[k] <- temp[k, 6],
      #        secondary        = Secondary[k] <- temp[k, 6],
      #        tertiary         = Tertiary[k] <- temp[k, 6],
      #        quartiary        = Quartiary[k] <- temp[k, 6],
      #        Neurite         = Neurite[k] <- temp[k, 6],
      #        Neurite_primary = Neurite_primary[k] <- temp[k, 6],
      #        ##leftovers
      #        Other[k] <- temp[k, 6]
      # )
      if (tolower(temp$Type[k]) == "axon") {
        Axon[k] <- temp[k, 6]
      }
      else if (tolower(temp$Type[k]) == "primary branch") {
        Branch[k] <- temp[k, 6]
      }
      else if (tolower(temp$Type[k]) == "filopodium") {
        Filopodium[k] <- temp[k, 6]
      }
      else if (tolower(temp$Type[k]) == "branch filopodium") {
        Branch_filopodium[k] <- temp[k, 6]
      }
      else if (tolower(temp$Type[k]) == "neurite filopodium") {
        Neurite_filopodium[k] <- temp[k, 6]
      }
      else if (tolower(temp$Type[k]) == "neurite") {
        Neurite[k] <- temp[k, 6]
      }
      else if (tolower(temp$Type[k]) == "neurite branch") {
        Neurite_branch[k] <- temp[k, 6]
      }
      else {
        Other[k] <- temp[k, 6]
      }
    }
    
    
    # Enter stats to table (somehow using column names instead of indices runs into error after second loop )
    
    Axon_results[i, 1] <- folderlist[i]                                                      # Image-Name
    Axon_results[i, 2] <- sum(Axon, Branch, na.rm = TRUE)                                    # Total axon length
    Axon_results[i, 3] <- sum(Axon, na.rm = TRUE)                                            # Primary Axon length
    Axon_results[i, 4] <- sum(Branch, na.rm = TRUE)                                          # Total Branch length
    Axon_results[i, 5] <- sum(Filopodium, na.rm = TRUE)                                      # Primary branch length
    Axon_results[i, 6] <- max(Filopodium, na.rm = TRUE)                                      # Secondary branch length
    Axon_results[i, 7] <- mean(Filopodium, na.rm = TRUE)                                     # Primary branch number
    Axon_results[i, 8] <- length(na.omit(Branch))                                            # Secondary branch number
    Axon_results[i, 9] <- length(na.omit(Filopodium))                                        # Tertiary branch number
    Axon_results[i, 10] <- length(na.omit(Branch_filopodium))                                # Quartiary branch number
    Axon_results[i, 11] <- Axon_results[i, 8] / Axon_results[i, 3]                           # Primary branches / primary axon length
    Axon_results[i, 12] <- Axon_results[i, 9] / Axon_results[i, 3]                           # Secondary branches / primary branch length
    Axon_results[i, 13] <- Axon_results[i, 10] / Axon_results[i, 4]                           # Tertiary branches / secondary axon length
    Axon_results[i, 14] <- length(na.omit(Axon))                                             # Number of Axons
    Axon_results[i, 15] <- sum(Axon_results[i, 9:10])                                        # Total number of branches
    Axon_results[i, 16] <- Axon_results[i, 15] / Axon_results[i, 2]                          # Total branches / total axon length
    
    Neurite_results[i, 1] <- folderlist[i]                                    # Image-Name
    Neurite_results[i, 2] <- sum(Neurite, Neurite_branch, na.rm = TRUE)    # Total Neurite length
    Neurite_results[i, 4] <- length(na.omit(Neurite))                        # Neurite number
    Neurite_results[i, 3] <- Neurite_results[i, 2] / Neurite_results[i, 4]  # Mean Neurite length (including branches)
    Neurite_results[i, 5] <- length(na.omit(Neurite_branch))                # Total Neurite branch length
    Neurite_results[i, 6] <- length(na.omit(Neurite_filopodium))                # Total Neurite branch length
    Neurite_results[i, 7] <- Neurite_results[i, 5] / Neurite_results[i, 2]  # Branche number / Neurite length
    Neurite_results[i, 8] <- Neurite_results[i, 6] / Neurite_results[i, 2]  # Branche number / Neurite length
    Neurite_results[i, 9] <- Axon_results[i, 2] + Neurite_results[i, 2]      # Total neurite length (Total axon + Total Neurite)
    Neurite_results[i, 10] <- Axon_results[i, 2] / Neurite_results[i, 2]      # Axon to Neurite ratio (Total axon / Total Neurite)
    
    Leftover[i, 1] <- folderlist[i]                                            # Image-Name
    Leftover[i, 2] <- length(na.omit(Other))                                   # Number of non-/ mis-classified processes
    
    if(Leftover[i, 2] > 0) {
      warning(paste(folderlist[i], ": some tracings have an unfamiliar type. Consider revising the NeuronJ tracings before you continue. \nSupported types are: Axon, Primary, Secondary, Tertiary, Quartiary, Neurite & Neurite_primary\n"))
    }
    
  }
  

  
  # # separate file name into Group & Image ID
  # Axon_results <- separate(data = Axon_results, col = Image, into = c("Condition", "ID"), sep = "_", remove = F, extra = "merge")
  # Neurite_results <- separate(data = Neurite_results, col = Image, into = c("Condition", "ID"), sep = "_", remove = F, extra = "merge")
  # 
  
  ## save results
  write.table(Axon_results, file.path(resultsdir, "Axon_results.txt"), sep = "\t", row.names = F)
  write.table(Neurite_results, file.path(resultsdir, "Neurite_results.txt"), sep = "\t", row.names = F)
  write.table(Leftover, file.path(resultsdir, "Misclassified.txt"), sep = "\t", row.names = F)
  message("Success: All neurons have been analysed and Results have been saved\n")
  }
