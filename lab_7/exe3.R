# Load the necessary library for working with Excel files
library(readxl)

# Read the data from the first sheet of the Excel file
excel_data <- read_excel('/home/ibab/Downloads/pone.0148733.s001.xlsx', sheet = 1)

# Print the column names of the loaded data
print(colnames(excel_data))

# Print the dimensions (number of rows and columns) of the loaded data
print(dim(excel_data))
