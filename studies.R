text1 <- scan("final//en_US//en_US.blogs.txt", character(0), sep = "\n") # separate each line

text1_chars = nchar(text1) # size (in characters) of each name
summary(text1_chars)

hate <- grep("hate",twitter) # grep lines where pattern happen
twitter[grep("biostat*",twitter)] # lines where pattern happen