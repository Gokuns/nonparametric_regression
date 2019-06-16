data_set <-  read.csv("hw04_data_set.csv", header=TRUE)

train_set <- data_set[1:100,]

test_set <- data_set[101:133,]

x <- data_set$x
y <- data_set$y 

x_train <- train_set$x
y_train <- train_set$y

x_test <- test_set$x
y_test <- test_set$y

minimum_value <- 0
maximum_value <- 60
data_interval <- seq(from = minimum_value, to = maximum_value, by = 0.01)
origin <- 0
bin_width <- 3
left_borders <- seq(from = minimum_value, to = maximum_value - bin_width, by = bin_width)
right_borders <- seq(from = minimum_value + bin_width, to = maximum_value, by = bin_width)
bin_count <- length(left_borders)
N <- length(x)


plot_data_points <- function(){
  plot(x_train, y_train, type = "p", pch = 19, col = "blue",
       ylim = c(min(y), max(y)), xlim = c(minimum_value, maximum_value),
       ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
  points(x_test, y_test, type = "p", pch = 19, col = "red")
}





y_predicted <- sapply(1:bin_count, function(b) {
  same_bin <- (left_borders[b] < x_train & x_train <= right_borders[b])
  return(sum(same_bin * y_train) / sum (same_bin))
})

plot_data_points()
for (b in 1:bin_count) {
  lines(c(left_borders[b], right_borders[b]), c(y_predicted[b], y_predicted[b]), lwd = 2, col = "black")
  if (b < bin_count) {
    lines(c(right_borders[b], right_borders[b]), c(y_predicted[b], y_predicted[b + 1]), lwd = 2, col = "black") 
  }
}

rmse <- sqrt(mean(sapply(1:length(x_test), function(i){
  
  current_bin_index <- Position(function(b){left_borders[b] < x_test[i] & x_test[i] <= right_borders[b]}, 1:bin_count)
  
  return ((y_test[i] - y_predicted[ current_bin_index ])^2)
})))

print(sprintf("Regressogram => RMSE is %.4f when h is %g", rmse, bin_width))


weighted_learner_template <- function(algorithm_name, bin_width, weight_func){
  smoother <- function(x) {
    weight <- weight_func((x - x_train) / bin_width)
    return(sum(weight * y_train) / sum(weight))
  }
  
  p_head <- sapply(data_interval, smoother)
  
  plot_data_points()
  lines(data_interval, p_head, type = "l", lwd = 2, col = "black")
  
  rmse <- sqrt(mean((y_test - sapply(x_test, smoother))^2))
  print(sprintf("%s => RMSE is %.4f when h is %g", algorithm_name, rmse, bin_width))
}

# running mean smoother
bin_width <- 3
weighted_learner_template("Running Mean Smoother", bin_width, function(region){-0.5 <= region & region < 0.5})

# kernel smoother
bin_width <- 1
weighted_learner_template("Kernel Smoother", bin_width, function(region){exp(-(region^2)/2) / sqrt(2 * pi) })



