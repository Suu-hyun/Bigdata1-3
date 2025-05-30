y <- c(1,2,3,4,5,6)
name <- list(c('a1', 'a2'), c('b1', 'b2')) # 각 변수명마다 ''를 사용해줘야함함
array(y, dim=c(2,2,3), dimnames = name)

list1 <- list(c(1,2,3),"Hello",14, TRUE)
list1
list1$'Hello'

ex <- c(TRUE, FALSE)
str(ex)

seq(from = 1, to = 10, length.out = 5)
seq(to = 5)
v <- c(10, 20, 30, 40)
seq(along.with = v)
seq_len(5)
seq_along(v)
