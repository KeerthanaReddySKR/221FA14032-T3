#1Facebook_Economy
library(ggplot2)

df <- read.csv("C:/Users/keert/Downloads/news_popularity/Facebook_Economy.csv")

model <- lm(TS9 ~ TS1, data = df)

summary(model)

df$predicted <- predict(model)

ggplot(df, aes(x = TS1, y = TS9)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predicted), color = 'red') +
  labs(title = 'Linear Regression of TS9 vs TS1', x = 'TS1', y = 'TS9') +
  theme_minimal()

cat('Intercept:', coef(model)[1], '\n')
cat('Slope:', coef(model)[2], '\n')



#2Facebook_Microsoft

library(ggplot2)

df <- read.csv("C:/Users/keert/Downloads/news_popularity/Facebook_Microsoft.csv")

model <- lm(TS144 ~ TS100, data = df)

summary(model)

df$predicted <- predict(model)

ggplot(df, aes(x = TS100, y = TS144)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predicted), color = 'red') +
  labs(title = 'Linear Regression of TS144 vs TS100', x = 'TS100', y = 'TS144') +
  theme_minimal()

cat('Intercept:', coef(model)[1], '\n')
cat('Slope:', coef(model)[2], '\n')



#3.Facebook_Obama


library(ggplot2)

df <- read.csv("C:/Users/keert/Downloads/news_popularity/Facebook_Obama.csv")

best_r2 <- 0
best_x_col <- NULL
best_y_col <- NULL

for (i in 1:126) {
  for (j in (i + 1):126) {
    X <- df[, paste0("TS", i)]
    y <- df[, paste0("TS", j)]
    
    model <- lm(y ~ X)
    
    r2 <- summary(model)$r.squared
    
    if (r2 > best_r2) {
      best_r2 <- r2
      best_x_col <- paste0("TS", i)
      best_y_col <- paste0("TS", j)
    }
  }
}

print(paste("Best fit found with", best_x_col, "as X and", best_y_col, "as Y, R-squared:", best_r2))

X <- df[, best_x_col]
y <- df[, best_y_col]

model <- lm(y ~ X)

df$predicted <- predict(model)

ggplot(df, aes_string(x = best_x_col, y = best_y_col)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predicted), color = 'red') +
  labs(title = paste('Linear Regression of', best_y_col, 'vs', best_x_col), x = best_x_col, y = best_y_col) +
  theme_minimal()

cat('Intercept:', coef(model)[1], '\n')
cat('Slope:', coef(model)[2], '\n')


#4.LinkedIn_Economy


library(ggplot2)

df <- read.csv("C:/Users/keert/Downloads/news_popularity/LinkedIn_Economy.csv")

best_r2 <- 0
best_x_col <- NULL
best_y_col <- NULL

for (x_col in 1:126) {
  for (y_col in 1:126) {
    if (x_col != y_col) {
      X <- df[, paste0("TS", x_col)]
      y <- df[, paste0("TS", y_col)]
      
      model <- lm(y ~ X)
      
      r2 <- summary(model)$r.squared
      
      if (r2 > best_r2) {
        best_r2 <- r2
        best_x_col <- paste0("TS", x_col)
        best_y_col <- paste0("TS", y_col)
      }
    }
  }
}

print(paste('Best fit: X column:', best_x_col))
print(paste('Y column:', best_y_col))
print(paste('R-squared:', best_r2))

X <- df[, best_x_col]
y <- df[, best_y_col]

model <- lm(y ~ X)

df$predicted <- predict(model)

ggplot(df, aes_string(x = best_x_col, y = best_y_col)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predicted), color = 'red') +
  labs(title = paste('Linear Regression of', best_y_col, 'vs', best_x_col), x = best_x_col, y = best_y_col) +
  theme_minimal()

cat('Intercept:', coef(model)[1], '\n')
cat('Slope:', coef(model)[2], '\n')




#5.LinkedIn_Microsoft


library(ggplot2)

df <- read.csv("C:/Users/keert/Downloads/news_popularity/LinkedIn_Microsoft.csv")

model <- lm(TS100 ~ TS69, data = df)

summary(model)

df$predicted <- predict(model)

ggplot(df, aes(x = TS69, y = TS100)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predicted), color = 'red') +
  labs(title = 'Linear Regression of TS100 vs TS69', x = 'TS69', y = 'TS100') +
  theme_minimal()

ggsave("C:/Users/keert/Downloads/TS100_vs_TS69_plot.png", width = 7, height = 5)

cat('Intercept:', coef(model)[1], '\n')
cat('Slope:', coef(model)[2], '\n')



#6.LinkedIn_Obama

library(ggplot2)

df <- read.csv("C:/Users/keert/Downloads/news_popularity/LinkedIn_Obama.csv")

model <- lm(TS78 ~ TS32, data = df)

summary(model)

df$predicted <- predict(model)

ggplot(df, aes(x = TS32, y = TS78)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predicted), color = 'red') +
  labs(title = 'Linear Regression of TS78 vs TS32', x = 'TS32', y = 'TS78') +
  theme_minimal()

ggsave("C:/Users/keert/Downloads/TS78_vs_TS32_plot.png", width = 7, height = 5)

cat('Intercept:', coef(model)[1], '\n')
cat('Slope:', coef(model)[2], '\n')





#7News_Final

library(ggplot2)

df <- read.csv("C:/Users/keert/Downloads/news_popularity/News_Final.csv")

model <- lm(SentimentHeadline ~ SentimentTitle, data = df)

summary(model)

df$predicted <- predict(model)

ggplot(df, aes(x = SentimentTitle, y = SentimentHeadline)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predicted), color = 'red') +
  labs(title = 'Linear Regression of SentimentHeadline vs SentimentTitle', x = 'SentimentTitle', y = 'SentimentHeadline') +
  theme_minimal()

ggsave("C:/Users/keert/Downloads/SentimentHeadline_vs_SentimentTitle_plot.png", width = 7, height = 5)

cat('Intercept:', coef(model)[1], '\n')
cat('Slope:', coef(model)[2], '\n')



#8 Facebook_Palestine

library(ggplot2)

df <- read.csv("C:/Users/keert/Downloads/news_popularity/Facebook_Palestine.csv")

model <- lm(TS139 ~ TS135, data = df)

summary(model)

df$predicted <- predict(model)

ggplot(df, aes(x = TS135, y = TS139)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predicted), color = 'red') +
  labs(title = 'Linear Regression of TS139 vs TS135', x = 'TS135', y = 'TS139') +
  theme_minimal()

ggsave("C:/Users/keert/Downloads/TS139_vs_TS135_plot.png", width = 7, height = 5)

cat('Intercept:', coef(model)[1], '\n')
cat('Slope:', coef(model)[2], '\n')



#9.GooglePlus_Economy

library(ggplot2)

df <- read.csv("C:/Users/keert/Downloads/news_popularity/GooglePlus_Economy.csv")

model <- lm(TS90 ~ TS67, data = df)

summary(model)

df$predicted <- predict(model)

ggplot(df, aes(x = TS67, y = TS90)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predicted), color = 'red') +
  labs(title = 'Linear Regression of TS90 vs TS67', x = 'TS67', y = 'TS90') +
  theme_minimal()

ggsave("C:/Users/keert/Downloads/TS90_vs_TS67_plot.png", width = 7, height = 5)

cat('Intercept:', coef(model)[1], '\n')
cat('Slope:', coef(model)[2], '\n')


#10.GooglePlus_Microsoft

library(ggplot2)

df <- read.csv("C:/Users/keert/Downloads/news_popularity/GooglePlus_Microsoft.csv")

model <- lm(TS48 ~ TS85, data = df)

summary(model)

df$predicted <- predict(model)

ggplot(df, aes(x = TS85, y = TS48)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predicted), color = 'red') +
  labs(title = 'Linear Regression of TS48 vs TS85', x = 'TS85', y = 'TS48') +
  theme_minimal()

ggsave("C:/Users/keert/Downloads/TS48_vs_TS85_plot.png", width = 7, height = 5)

cat('Intercept:', coef(model)[1], '\n')
cat('Slope:', coef(model)[2], '\n')


#11.GooglePlus_Obama

library(ggplot2)

df <- read.csv("C:/Users/keert/Downloads/news_popularity/GooglePlus_Obama.csv")

model <- lm(TS46 ~ TS30, data = df)

summary(model)

df$predicted <- predict(model)

ggplot(df, aes(x = TS30, y = TS46)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predicted), color = 'red') +
  labs(title = 'Linear Regression of TS46 vs TS30', x = 'TS30', y = 'TS46') +
  theme_minimal()

ggsave("C:/Users/keert/Downloads/TS46_vs_TS30_plot.png", width = 7, height = 5)

cat('Intercept:', coef(model)[1], '\n')
cat('Slope:', coef(model)[2], '\n')



#12.GooglePlus_Palestine
library(ggplot2)

df <- read.csv("C:/Users/keert/Downloads/news_popularity/GooglePlus_Palestine.csv")

model <- lm(TS123 ~ TS76, data = df)

summary(model)

df$predicted <- predict(model)

ggplot(df, aes(x = TS76, y = TS123)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predicted), color = 'red') +
  labs(title = 'Linear Regression of TS123 vs TS76', x = 'TS76', y = 'TS123') +
  theme_minimal()

ggsave("C:/Users/keert/Downloads/TS123_vs_TS76_plot.png", width = 7, height = 5)

cat('Intercept:', coef(model)[1], '\n')
cat('Slope:', coef(model)[2], '\n')


#13.LinkedIn_Palestine

library(ggplot2)

df <- read.csv("C:/Users/keert/Downloads/news_popularity/LinkedIn_Palestine.csv")

model <- lm(TS89 ~ TS37, data = df)

summary(model)

df$predicted <- predict(model)

ggplot(df, aes(x = TS37, y = TS89)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predicted), color = 'red') +
  labs(title = 'Linear Regression of TS89 vs TS37', x = 'TS37', y = 'TS89') +
  theme_minimal()

ggsave("C:/Users/keert/Downloads/TS89_vs_TS37_plot.png", width = 7, height = 5)

cat('Intercept:', coef(model)[1], '\n')
cat('Slope:', coef(model)[2], '\n')

