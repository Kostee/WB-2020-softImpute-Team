source('./szkielet.r')

dfs <- read_all_imputed_datasets()

w <- lapply(dfs, function(x){list(x$imp_method, as.numeric(x$imp_time, units = 'secs'), x$id)})


df <- data.frame(matrix(unlist(w), nrow=length(w), byrow=T), stringsAsFactors = FALSE)
colnames(df) <- c("imputation", "time", "dataset_id")

df$time <- df$time %>% as.numeric()

library(ggplot2)
require(scales)

ggplot(data = df, aes(x = dataset_id, y = time, color = imputation)) + 
  geom_point()  +
  theme_gray() +
  scale_y_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) + 
  labs(x = "Dataset Id",
     y = "Imputation time [in seconds]",
     color = "Legend") 


ggplot(data = df, aes(x = imputation, y = time, color = imputation)) + 
  geom_boxplot()  +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x)))  + 
  labs(x = "Imputation method",
       y = "Imputation time [in seconds]",
       color = "Legend") 
