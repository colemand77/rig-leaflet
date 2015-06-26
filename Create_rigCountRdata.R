raw <- read.table("clipboard", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
save(raw, file = 'rigcount.RData')
