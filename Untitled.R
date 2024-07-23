
# Load required libraries
library(RSelenium)
library(httr)

# Start the Safari driver
system("safaridriver -p 4444", wait = FALSE)

# Connect to the Safari driver
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "safari")
remDr$open()

# Navigate to the webpage
remDr$navigate("https://www.redape.dados.embrapa.br/dataset.xhtml?persistentId=doi:10.48432/EAOMKQ")

# Function to click on "Acessar Arquivo" and then "Formato original do arquivo" and return the download link
get_download_link <- function() {
  tryCatch({
    # Find and click "Acessar Arquivo" button
    file_access_button <- remDr$findElement(using = "xpath", "//a[contains(@class, 'btn-primary') and contains(text(), 'Acessar Arquivo')]")
    file_access_button$clickElement()
    
    Sys.sleep(2) # Wait for the modal to load
    
    # Find and get the "Formato original do arquivo" link
    original_format_link <- remDr$findElement(using = "xpath", "//a[contains(text(), 'Formato original do arquivo')]")
    download_link <- original_format_link$getElementAttribute("href")[[1]]
    
    # Close the modal (by navigating back)
    remDr$navigate("https://www.redape.dados.embrapa.br/dataset.xhtml?persistentId=doi:10.48432/EAOMKQ")
    
    Sys.sleep(2) # Wait for the page to reload
    
    return(download_link)
  }, error = function(e) {
    message("Error in get_download_link: ", e$message)
    return(NULL)
  })
}

# Get all "Acessar Arquivo" buttons
file_access_buttons <- remDr$findElements(using = "xpath", "//a[contains(@class, 'btn-primary') and contains(text(), 'Acessar Arquivo')]")

# Print number of buttons found for debugging
cat(sprintf("Found %d 'Acessar Arquivo' buttons.\n", length(file_access_buttons)))

# Extract download links for all files
download_links <- sapply(file_access_buttons, function(button) {
  tryCatch({
    button$clickElement()
    Sys.sleep(2)
    original_format_link <- remDr$findElement(using = "xpath", "//a[contains(text(), 'Formato original do arquivo')]")
    download_link <- original_format_link$getElementAttribute("href")[[1]]
    remDr$navigate("https://www.redape.dados.embrapa.br/dataset.xhtml?persistentId=doi:10.48432/EAOMKQ")
    Sys.sleep(2)
    return(download_link)
  }, error = function(e) {
    message("Error while extracting download link: ", e$message)
    return(NULL)
  })
})

# Filter out any NULLs from failed attempts
download_links <- download_links[!sapply(download_links, is.null)]

# Print the download links for debugging
cat("Download links:\n")
print(download_links)

# Function to download a file and save it to the specified folder
download_file <- function(file_url, dest_folder) {
  file_name <- basename(file_url)
  dest_file <- file.path(dest_folder, file_name)
  cat(sprintf("Downloading file to: %s\n", dest_file))
  GET(file_url, write_disk(dest_file, overwrite = TRUE))
}

# Destination folder
dest_folder <- "/Users/emilydoyle/Library/CloudStorage/OneDrive-UniversityofExeter/LIDAR/GEDI_A02_2021_LiDAR"
dir.create(dest_folder, showWarnings = FALSE, recursive = TRUE)

# Download files in batches of 2 to stay within the 100MB limit
batch_size <- 2
total_files <- length(download_links)
batch_count <- ceiling(total_files / batch_size)

for (i in 1:batch_count) {
  start_index <- (i - 1) * batch_size + 1
  end_index <- min(i * batch_size, total_files)
  
  cat(sprintf("Downloading files %d to %d\n", start_index, end_index))
  
  for (j in start_index:end_index) {
    cat(sprintf("Downloading: %s\n", download_links[j]))
    download_file(download_links[j], dest_folder)
  }
  
  # Optional: Introduce a pause to avoid overloading the server
  Sys.sleep(5)
}

cat("All files downloaded.\n")

# Close the RSelenium client and server
remDr$close()
