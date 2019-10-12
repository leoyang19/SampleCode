# Running All Tests 
library('testthat')
functions <- dir('../code/functions')
lapply(paste0('../code/functions/', functions), source)
sink(file = 'test-output.txt')
test_dir('../code/tests')
sink()
