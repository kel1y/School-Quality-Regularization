library(dplyr)

set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

schools %>% ggplot(aes(size, score)) + geom_point(alpha = 0.5) + geom_point(data=filter(schools, rank <= 10), col=2)


#regularizing schools
overall <- mean(sapply(scores, mean))
alpha <- 25
reguralize_school <- sapply(scores, function(x) {overall + sum(x-overall)/(length(x)+alpha)})
schools %>% mutate(reguralize_school = reguralize_school) %>% 
  top_n(10, reguralize_school) %>% arrange(desc(reguralize_school))

#finding best aplhas for regularization
overall <- mean(sapply(scores, mean))
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  reg_score <- sapply(scores, function(x) overall + sum(x-overall)/length(x)+alpha)
  sqrt(mean((reg_score - schools$quality)^2))
})