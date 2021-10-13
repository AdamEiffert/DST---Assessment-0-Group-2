# EDA of a URL data set.

# First make sure you've got the dplyr package.

library(dplyr)

# Now we get the data. Download the folder from the webpage
# https://www.unb.ca/cic/datasets/url-2016.html .
# Change your working directory to the folder "URL" which lies
# INSIDE the folder just downloaded. I moved the "URL" folder
# to my desktop.
setwd("/Users/willnunn/Desktop/URL")

benign <- read.csv(file = "Benign_list_big_final.csv")
# Add column name so we can row bind, add column of labels.
colnames(benign) <- c("URL")
benign["Phish"] = rep(0, nrow(benign))

phish <- read.csv(file = "phishing_dataset.csv")
colnames(phish) <- c("URL")
phish["Phish"] = rep(1, nrow(phish))

# We bind the data into a single labeled frame.
df <- rbind.data.frame(benign, phish)
# Check the following output is 0 1.
df[nrow(benign):(nrow(benign)+1), 2]
# Remove the redundant dataframes.
rm(benign, phish)

# I first wanted to visualise the lengths of the URLs between
# the two classes, so added a "Length" column with dplyr's very
# useful mutate function. 

df <- df %>% mutate(Length = nchar(URL))
# Check this has applied correctly.
head(df["Length"])

# Head lengths are all 83, were the URLs already sorted by
# length, or is this something else?

set.seed(1)

test <- sort(sample(1:30000, 3, replace = FALSE))
for(i in test){
  if(nchar(df[i,1]) == df[i,3]){print(c(i, df[i,3],1))}
  else{print(c(i, df[i,3],0))}
}

rm(i, test)

# Looks like the URLs were already sorted by length.

# We now produce a density plot.
d0 <- density(df$Length[df$Phish == 0])
d1 <- density(df$Length[df$Phish == 1])

plot(d0, xlim = c(0, 300), col = "blue", main = "Density Plot",
     xlab = "URL Length")
lines(d1, col = "red")
legend(x = "topright", legend = c("Benign", "Phish"),
       col = c("blue", "red"), lwd = 1)

rm(d0, d1)

# This plot is quite surprising. The two stand out features are:
# 1. The greater density of phish using URL lengths of 20-80.
# 2. The 'normally distributed bump in the phish URL length 
#    density around length 200.

# I would suspect there is an advantage having shorter
# phishing URLs- the shorter the link the more likely
# it is to be trusted and followed.
# As for the increased density around 200, that warrants
# more investigation.
phish_200 <- df[which(df$Phish == 1 & df$Length > 196
                   & df$Length < 204),]
rm(phish_200)
# After taking a look at some of the entries in the above
# dataframe I noticed hundreds were of the form
# http://appleid.apple.co.uk.cgi-bin ... and all around
# 200 characters in length, perhaps these were generated
# in an algorithmic way by the same bad actor.

# Next I wanted to approximate the Markov transition
# counts for the benign and phishing links. Where entry
# indexed by "a", "b" denotes the number of times letter 
# sequence "ab" occurs in the list of URLs.
# First instinct was to make a massive dataframe of all
# transitions then apply the table function.
# Realised I could neater, make a counts for each URL
# individually and combine as I went along.
# I made two functions to do this. 

# The first function counts the number of each
# transition.
transition_counts <- function(string) {
  broken <- unlist(strsplit(string, ""))
  counts <- data.frame(x = broken[1:length(broken)-1],
                       y = broken[2:length(broken)]) %>% 
    table() %>% 
    as.data.frame.matrix()
  rm(broken)
  return(counts)
}

# To illustrate the action of transition_counts():
string1 <- "ababbbabccabb"
string2 <- "abdddbbaabdddbab"
df1 <- transition_counts(string1)
df2 <- transition_counts(string2)

# The second function combines outputs of transition_counts
# into a single dataframe.

combine_counts <- function(df1, df2) {
  new_rows <- union(rownames(df1), rownames(df2))
  new_cols <- union(colnames(df1), colnames(df2))
  df <- data.frame(new_rows, new_cols) %>%
    table %>%
    as.data.frame.matrix()
  for(i in new_rows){
    for(j in new_cols){
      df[i,j] = max(na.exclude(df1[i, j]), 0) + 
        max(na.exclude(df2[i,j]), 0)
    }
  }
  rm(new_rows, new_cols)
  return(df)
}

# To illustrate the action of combine_counts() .

combine_counts(df1, df2)
rm(string1, string2, df1, df2)

# Now we use the functions to get the full counts.

seedf <- data.frame()
for(i in data.frame(df[which(df$Phish == 0), 1])){
  seedf <- combine_counts(seedf, transition_counts(i))
}
rm(i)

benign_counts <- seedf
rm(seedf)

seedf <- data.frame()
for(i in data.frame(df[which(df$Phish == 1), 1])){
  seedf <- combine_counts(seedf, transition_counts(i))
}
rm(i)

phish_counts <- seedf
rm(seedf)


# We'll keep just the transitions to and from lower case
# letters, upper case letters, numbers, and some symbols

lows <- "qwertyuiopasdfghjklzxcvbnm"
v1 <- unlist(strsplit(lows, ""))
ups <- toupper(lows)
v2 <- unlist(strsplit(ups, ""))
numbs <- "1234567890"
v3 <- unlist(strsplit(numbs, ""))
syms <- "/.:%+=?"
v4 <- unlist(strsplit(syms, ""))
keeps <- c(v1, v2, v3, v4)
rm(lows, v1, ups, v2, numbs, v3, syms, v4)

benign_counts_plot <- benign_counts[keeps, keeps]
phish_counts_plot <- phish_counts[keeps, keeps]

# Now let's visualise this hard earned data
# with a couple of heatmaps.

heatmap(as.matrix(benign_counts_plot),
        main = "Benign Markov Counts",
        labRow = F, labCol = F,
        Rowv = NA, Colv = NA)
heatmap(as.matrix(phish_counts_plot), 
        main = "Phish Markov Counts",
        labRow = F, labCol = F,
        Rowv = NA, Colv = NA)

# I didn't expect these maps to look as distinct.
# I'd now be very interested to see how well a Markov
# classifier performs on this data set.

# Counting the number of letters and symbols in each class

char_counts <- function(string) {
  broken <- unlist(strsplit(string, ""))
  counts <- data.frame(x = broken[1:length(broken)]) %>% 
    table() %>% 
    as.matrix() %>%
    as.data.frame()
  rm(broken)
  return(counts)
}

test1 <- "abnabdnar"
test2 <- "greatgre"

df1<- char_counts(test1)
df2<- char_counts(test2)
rm(test1, test2)

combine_counts(df1, df2)
rm(df1, df2)

seedf <- data.frame()
for(i in data.frame(df[which(df$Phish == 0), 1])){
  seedf <- combine_counts(seedf, char_counts(i))
}
rm(i)
benign_char <- as.matrix(seedf)

seedf <- data.frame()
for(i in data.frame(df[which(df$Phish == 1), 1])){
  seedf <- combine_counts(seedf, char_counts(i))
}
rm(i)
phish_char <- as.matrix(seedf)
rm(seedf)

benign_char <- as.matrix(benign_char)


benign_char <- benign_char[keeps,]

phish_char <- phish_char[keeps,]

ben_tot <- sum(benign_char)
phi_tot <- sum(phish_char)
benign_prop <- benign_char / ben_tot
phish_prop <- phish_char / phi_tot
rm(ben_tot, phi_tot)

par(mfrow= c(1,2))
barplot(benign_prop, ylab="Character",
        xlab ="Proportion", main="Benign Character Proportion",
        names.arg = keeps, horiz = T, col="blue", yaxt='n')
barplot(phish_prop, ylab="Character",
        xlab="Proportion", main="Phish Character Proportion",
        names.arg = keeps, horiz = T, col="red", yaxt='n')

ratio1 <- benign_prop/phish_prop
ratio2 <- phish_prop/benign_prop

sort(ratio1)
sort(ratio2)

# Run some sort of classifier using a machine learning book
# on the enriched dataset enriched by lex features.

lex_phish <- read.csv("/Users/willnunn/Desktop/URL_full/Phishing_BestFirst.csv")

