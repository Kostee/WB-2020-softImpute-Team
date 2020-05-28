library(dplyr)
final_results2 <- readRDS('./final_results_2.RDS')
f2 <- unlist(final_results2, recursive = FALSE)

times <- lapply(f2, function(x){list( x$dataset_id,
                                      #as.numeric(x$imputation_time, units = 'secs'), 
                                      as.character(x$imp_method), 
                                      as.character(x$mod_mehtod), 
                                      as.numeric(x$classification_report$f1), 
                                      as.numeric(x$classification_report$mcc))})

df <- do.call(rbind, times) %>% as.data.frame()

#colnames(df) <- c("time", "imp", "model", "f1", "mcc")
colnames(df) <- c("id","imp", "model", "f1", "mcc")

#df$time <- as.numeric(df$time)
df$id <- as.integer(df$id)
df$imp <- as.character(df$imp)
df$model <- as.character(df$model)
df$f1 <- as.numeric(df$f1)
df$mcc <- as.numeric(df$mcc)

rankings <- df %>% group_by(id, model) %>% mutate(r_f1 = rank(-f1), r_mcc = rank(-mcc))

melted <- reshape::melt(rankings[,c("imp", "r_f1", "r_mcc")] %>% as.data.frame)

library(ggplot2)

# wykres nie uzględnia NA w zbiorach, które nie zostały policzone
# nie ma dużych różnic między metryką mcc a f1
ggplot(melted, aes(x = imp, y = value, color = variable)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# uśrednione wyniki
rankings %>% group_by(imp) %>% transmute(m_r_f1 = mean(r_f1),m_r_mcc = mean(r_mcc)) %>% unique()

# dorzucmy NA tam, gdzie nie przeszły imputacje
# 14 zbiorow, 6 imp, 5 modeli. Każdy imp powinien mieć 14*5 obserwacji - jeśli nie ma - wypełniamy braki NA

library(tidyr)
cmp <- df %>% 
  complete(id, imp, model)

cmp <- replace_na(cmp, list(f1 = c(0), mcc = c(-1)))

#cmp$f1[is.na(cmp)] <- 0
#cmp$mcc[is.na(cmp)] <- -1

rankings <- cmp %>% group_by(id, model) %>% mutate(r_f1 = rank(-f1), r_mcc = rank(-mcc))
melted <- reshape::melt(rankings[,c("imp", "r_f1", "r_mcc")] %>% as.data.frame)

ggplot(melted, aes(x = imp, y = value, color = variable)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# uśrednione wyniki
rankings %>% group_by(imp) %>% transmute(m_r_f1 = mean(r_f1),m_r_mcc = mean(r_mcc)) %>% unique()
