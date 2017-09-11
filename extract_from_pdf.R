
# Prepare workspace ----------------------------------------------------------
rm(list = ls()) # Remove Previous Workspace
gc(reset = TRUE) # Garbage Collection

# Install packageas
packages <- function(x, repos = "http://cran.r-project.org", ...) {
  x <- deparse(substitute(x))
  if (!require(x, character.only = TRUE)) {
    install.packages(pkgs = x, dependencies = TRUE, repos = repos, ...)
    library(x, character.only = TRUE)
  }
}

# Load packages
packages(data.table) # Data Frame Complement
packages(doParallel) # Parallel Computing
packages(foreach) # Parallel Computing
packages(jsonlite) # JSON Data
packages(reshape2) # Manipulate Datasets
packages(pdftools) # PDF to TXT Editor
packages(splitstackshape) # Stack and Reshape Datasets After Splitting Concatenated Values
packages(stringi) # Character/String Editor
packages(stringr) # Character/String Editor
packages(tm) # Text Mining
packages(plyr) # Splitting, applying, and combining data

# Functions -------------------------------------------------------------------
FindMode <- function(x) {
  # Computes the statistical mode (most frequent value) of a vector.
  # 
  # Args: 
  #   x: The vector whose mode is to be calculated.
  # 
  # Returns:
  #   The statistical mode of a vector.
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

ExtractTable <- function(x) {
  # Extract student names and majors from vector of strings.
  # 
  # Args: 
  #   x: Character vector containing student names and majors.
  # 
  # Returns:
  #   Data table with student names in the first column, majors in the second.
  
  # Remove page numbers and blanks.
  x <- x[grep("CLASS MARSHAL", x, invert = TRUE, ignore.case = TRUE)]
  x <- x[grep("\\d$", x, invert = TRUE, ignore.case = TRUE)]
  x <- x[x != ""]
  if (any(grepl("MEMORIAM|POSTHUMOUS", x, ignore.case = TRUE))) { 
    x <- x[1:grep("MEMORIAM|POSTHUMOUS", x, ignore.case = TRUE) - 1] }
  
  # Determine width of first column, adjusting for PDFs with tab formatting.
  if (any(str_detect(x, "\t"))) {
    col1.width <- str_extract(x, ".{0,50}\t\\s{0,20}(?=[A-Z])") %>% 
      str_length() %>% FindMode()
  } else {
    col1.width <- str_extract(x, ".{0,50}\\s\\s(?=[A-Z])") %>%
      str_length() %>% FindMode()
  }

  # Determine width of second column.
  col2.width <- str_extract(x, "(?<=\\s\\s)[A-Z].{0,100}\\s\\s(?=[A-Z])") %>% 
    str_length() %>% FindMode()

  # Create data table.
  col1 <- substr(x, 0, col1.width) %>% str_trim()
  col2 <- substr(x, col1.width, col1.width + col2.width) %>% str_trim()
  x <- as.data.table(cbind(col1, col2))
}

FixOverflow <- function(x) {
  # When majors are too long for one column in the PDF, the text overflows
  # onto a second line. This function re-attaches the overflow text.
  # 
  # Args: 
  #   x: Data table containing student names and majors.
  # 
  # Returns:
  #   Data table with overflow text re-attached.
  
  # Initialize vector specifying rows to be removed.
  empty.rows <- numeric()
  
  # Loop over each row in df and append overflow text to value in prior row.
  for (i in 1:nrow(x)) {
    
    # Control for rows that contain an empty string.
    if (any(x[i, ] == "")) {
      
      # Control for rows that contain only 1 empty string.
      if (x[i, 1] != x[i, 2]) {
        
        # If 1st column is blank, append overflow text in 2nd column.
        if (x[i, 1] == "") {
          x[i-1, 2] <- paste(x[i-1, 2], x[i, 2], sep = " ")
        }
        
        # If 2nd column is blank, append overflow text in 1st or 2nd column.
        else if (x[i, 2] == "") {
          if (t.flag) {
            x[i-1, 2] <- paste(x[i-1, 2], x[i, 1], sep = " ")
          } else {
            x[i-1, 1] <- paste(x[i-1, 1], x[i, 1], sep = " ")
          }
        }
      }
      
      # Add row identifier to rows-to-remove vector.
      empty.rows <- c(empty.rows, i)
    }
  }
  
  # Remove rows containing an empty string.
  x <- x[-empty.rows, ]
}

ExtractMajors <- function(x) {
  # Parent function to extract student names and majors; calls ExtractTable 
  # and FixOverflow functions.
  # 
  # Args: 
  #   x: Character vector containing student names and majors.
  # 
  # Returns:
  #   Data table with student names, majors, and commencement year.
  
  # Determine commencement year from program.
  commencement.year <- regmatches(x, regexpr("[1-2][9|0][0-9][0-9]", x))[1]
  
  # Subset only the pages containing student names and majors.
  pos1 <- grep("CLASS MARSHAL", x, ignore.case = TRUE)
  pos2 <- grep("HONORARY APPOINTMENTS", x, ignore.case = TRUE) - 1
  x <- x[pos1:pos2]

  # Set flag indicating if \t is present (formatting is different).
  if (any(str_detect(x, "\t"))) { t.flag <- TRUE } else { t.flag <- FALSE }
  
  # Split each line into separate strings.
  x <- str_split(x, pattern = "\n")
  
  # Extract student names and majors.
  x <- lapply(x, ExtractTable)
  x <- rbind.fill(x)

  # Re-attach overflow text from overlong majors.
  x <- FixOverflow(x)
  
  # Add column with the commencement year for that PDF.
  x <- cbind(x, commencement.year)
}

# Select files ----------------------------------------------------------------

# Extract text from Bowdoin commencement programs.
bowdoin.files <- list.files(path = "pdf", pattern = "Bowdoin") # Select files from directory
bowdoin.data <- lapply(bowdoin.files, pdf_text) # Extract text from all files

# Initialize flag indicating if \t is present.
t.flag <- TRUE

# Extract student names, majors, and commencement year into data table.
bowdoin.table <- lapply(bowdoin.data, ExtractMajors)
bowdoin.table <- rbind.fill(bowdoin.table)
colnames(bowdoin.table) <- c("Student Name", "Major/Minor", "Year")

# Write csv file with Bowdoin students and majors.
write.csv(bowdoin.table, "bowdoin_majors.csv", row.names = FALSE, quote = FALSE)
