library("openxlsx")
Sys.setenv(JAVA_HOME = "C:/Program Files (x86)/Java/jre-1.8/")

a <- read.csv("sorted_data/Combined_OPEN.csv")
b <- read.csv("data/4299WordNorms Moors et al.csv")

# merging the dataframe
c <- merge(x = a, y = b,
           by.x = c("lemma"),
           by.y = c("words"), all.x = TRUE)
