## Summary Function
SummarizeNeurons <- function(folderdir) {
  
  # the magic function: count & measure length of individual processes
  SummarizeIndividual <- function(temp, ID, input) {
    
    
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
    Axon_results <- input[[1]]
    j = nrow(Axon_results) + 1 # always put the new results in the last row
    Axon_results[j, 1] <- ID                                                    # Image-Name
    Axon_results[j, 2] <- sum(Axon, Branch, na.rm = TRUE)                       # Total axon length
    Axon_results[j, 3] <- sum(Axon, na.rm = TRUE)                               # Primary Axon length
    Axon_results[j, 4] <- sum(Branch, na.rm = TRUE)                             # Total Branch length
    Axon_results[j, 5] <- sum(Filopodium, na.rm = TRUE)                         # Total filopodia length
    Axon_results[j, 6] <- max(Filopodium, na.rm = TRUE)                         # Maximal filopodium length
    Axon_results[j, 7] <- mean(Filopodium, na.rm = TRUE)                        # Mean filopodium length
    Axon_results[j, 8] <- length(na.omit(Branch))                               # Axon branch number
    Axon_results[j, 9] <- length(na.omit(Filopodium))                           # Axon filopodium number
    Axon_results[j, 10] <- length(na.omit(Branch_filopodium))                   # Branch filopodium number
    Axon_results[j, 11] <- Axon_results[j, 8] / Axon_results[j, 3]              # Branches / primary axon length
    Axon_results[j, 12] <- Axon_results[j, 9] / Axon_results[j, 3]              # Filopodia / primary axon length
    Axon_results[j, 13] <- Axon_results[j, 10] / Axon_results[j, 4]             # Branch-Filopodia / branch length
    Axon_results[j, 14] <- length(na.omit(Axon))                                # Number of Axons
    Axon_results[j, 15] <- sum(Axon_results[j, 9:10])                           # Total number of Filopodia
    Axon_results[j, 16] <- Axon_results[j, 15] / Axon_results[j, 2]             # Total Filopodia / total axon length
    
    Neurite_results <- input[[2]]
    Neurite_results[j, 1] <- ID                                                 # Image-Name
    Neurite_results[j, 2] <- sum(Neurite, Neurite_branch, na.rm = TRUE)         # Total Neurite length
    Neurite_results[j, 4] <- length(na.omit(Neurite))                           # Neurite number
    Neurite_results[j, 3] <- Neurite_results[j, 2] / Neurite_results[j, 4]      # Mean Neurite length (including branches)
    Neurite_results[j, 5] <- length(na.omit(Neurite_branch))                    # Total Neurite branch length
    Neurite_results[j, 6] <- length(na.omit(Neurite_filopodium))                # Total Neurite branch length
    Neurite_results[j, 7] <- Neurite_results[j, 5] / Neurite_results[j, 2]      # Branch number / Total Neurite length
    Neurite_results[j, 8] <- Neurite_results[j, 6] / Neurite_results[j, 2]      # Filopodia number / Total Neurite length
    Neurite_results[j, 9] <- Axon_results[j, 2] + Neurite_results[j, 2]         # Total neurite length (Total axon + Total Neurite)
    Neurite_results[j, 10] <- Axon_results[j, 2] / Neurite_results[j, 2]        # Axon to Neurite ratio (Total axon / Total Neurite)
    
    Leftover <- input[[3]]
    Leftover[j, 1] <- ID                                                        # Image-Name
    Leftover[j, 2] <- length(na.omit(Other))                                    # Number of non-/ mis-classified processes
    
    if(Leftover[i, 2] > 0) {
      warning(paste(ID, ": some tracings have an unfamiliar type. Consider revising the NeuronJ tracings before you continue. \nSupported types are: Axon, Primary, Secondary, Tertiary, Quartiary, Neurite & Neurite_primary\n"))
    }
    
    result <- list(Axon_results = Axon_results, Neurite_results = Neurite_results, Leftover = Leftover)
    return(result)
  }
  
  
  # Mainly error handling: using the function on the correct results-files & adapt to multiple cells per image
  
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
    Image = NA, 
    total_axon = NA,
    primary_axon = NA,
    total_branch_length = NA,
    total_filo_length = NA,
    max_filo_length = NA,
    mean_filo_length = NA,
    Number_branch = NA,
    Number_filo = NA,
    Number_branch_filo = NA, 
    branches_by_primary_axon = NA, 
    filopodia_by_primary_axon = NA, 
    branch_filopodia_by_branch_length = NA, 
    Number_axons = NA, 
    Total_filo = NA, 
    total_filopodia_by_total_length = NA 
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
  
  # combine dataframes to pass it into SummarizeIndividual function
  inputlist <- list(Axon_results = Axon_results, Neurite_results = Neurite_results, Leftover = Leftover)
  
  
  # open results-csvs per cell
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
    
    # Handling multiple cells at a time
    # Splitting neurons by cluster (if there are any)
    if(length(unique(temp$Cluster)) > 1) {
      list <- split(temp, temp$Cluster) 
      
      # go through every cell individually
      for (cluster in list){
        ID <- paste(folderlist[i], cluster$Cluster[1], sep = "_") # The Cluster gives the name
        inputlist <- SummarizeIndividual(cluster, ID, inputlist)
      }
    } else if (length(unique(temp$Cluster)) == 1) {
      ID <- paste(folderlist[i], cluster$Cluster[1], sep = "_") # The Cluster gives the name
      inputlist <- SummarizeIndividual(cluster, I, inputlist)
    }
        
  }
  

  ## save results
  write.table(inputlist$Axon_results, file.path(resultsdir, "Axon_results.txt"), sep = "\t", row.names = F)
  write.table(inputlist$Neurite_results, file.path(resultsdir, "Neurite_results.txt"), sep = "\t", row.names = F)
  write.table(inputlist$Leftover, file.path(resultsdir, "Misclassified.txt"), sep = "\t", row.names = F)
  message("Success: All neurons have been analysed and Results have been saved\n")
  }
