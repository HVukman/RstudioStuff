library("pdftools")
library("base")

# Enter path, glob adds all pdf names in variable files
files<-Sys.glob("*.pdf")

# function to write strings to txt
concat_strings <- function(..., sep='') {
  paste(..., sep=sep, collapse=sep)
}

# Loop over files 
for (fileName in files) {
  # read data:
  pdf.text<-pdftools::pdf_text(fileName)
  string_filename<-str(fileName)
  # write txt files 
  name_txt<-concat_strings(fileName,".txt")
  writeLines( pdf.text,name_txt) 
  
}



