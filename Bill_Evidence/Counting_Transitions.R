test1 <- "ababcabba"
test2 <- "adabbaaddd"


transition_counts <- function(string) {
  broken <- unlist(strsplit(string, ""))
  counts <- data.frame(x = broken[1:length(broken)-1],
                       y = broken[2:length(broken)]) %>% 
    table() %>% 
    as.data.frame.matrix()
  rm(broken)
  return(counts)
}


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


transition_counts(test1)
transition_counts(test2)
combine_counts(transition_counts(test1), transition_counts(test2))

