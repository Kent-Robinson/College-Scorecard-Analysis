raw <- read.csv("Most-Recent-Cohorts-Institution.csv")
vars <- c("CONTROL", "ADM_RATE", "SAT_AVG", "COSTT4_A", "PCTPELL", "C150_4", "RET_FT4", "GRAD_DEBT_MDN", "MD_EARN_WNE_P10")
df <- raw[, vars]
df <- df[df$CONTROL %in% c(1, 2, 3), ]
df[vars] <- lapply(df[vars], function(x) as.numeric(as.character(x)))
df <- na.omit(df)
cat("Observations after cleaning:", nrow(df), "\n")
pca_vars <- c("ADM_RATE", "SAT_AVG", "COSTT4_A", "PCTPELL", "C150_4", "RET_FT4", "GRAD_DEBT_MDN", "MD_EARN_WNE_P10")
pca <- princomp(df[, pca_vars], cor = TRUE)
pc_scores <- pca$scores[, 1:2]
pc_scaled  <- scale(pc_scores)
fviz_nbclust(pc_scaled, kmeans, method = "wss") + labs(title = "Elbow Method", subtitle = "Optimal number of clusters")
set.seed(123)
km <- kmeans(pc_scaled, centers = 5, iter.max = 100, nstart = 25)
cat("Cluster sizes:\n")
print(km$size)
df_lda        <- df[, pca_vars]
df_lda$CONTROL <- as.factor(df$CONTROL)
lda.fit <- lda(CONTROL ~ ADM_RATE + SAT_AVG + COSTT4_A + PCTPELL +  C150_4 + RET_FT4 + GRAD_DEBT_MDN + MD_EARN_WNE_P10, data = df_lda)
lda.pred  <- predict(lda.fit)
lda.class <- lda.pred$class
cat("\nFull-sample confusion matrix:\n")
print(table(lda.class, df_lda$CONTROL))
cat("Full-sample accuracy:", mean(lda.class == df_lda$CONTROL), "\n")
set.seed(530)
train_idx <- sample(1:nrow(df_lda), 0.8 * nrow(df_lda))
lda.fit2 <- lda(CONTROL ~ ADM_RATE + SAT_AVG + COSTT4_A + PCTPELL + C150_4 + RET_FT4 + GRAD_DEBT_MDN + MD_EARN_WNE_P10,data = df_lda, subset = train_idx)
lda.pred_test  <- predict(lda.fit2, newdata = df_lda[-train_idx, ])
lda.class_test <- lda.pred_test$class
cat("\nTest-set confusion matrix:\n")
cat("Test accuracy:", mean(lda.class_test == df_lda[-train_idx, ]$CONTROL), "\n")
