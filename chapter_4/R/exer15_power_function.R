# (a)
Power = function() {
  print(2 ** 3)
}

Power()

# (b)
Power2 = function(x, a) {
  res = x ** a
  print(res)
}

Power2(3, 8)

# (c)
Power2(10, 3) # 1000
Power2(8, 17) # 2.2518e+15
Power2(131, 3) # 2248091

# (d)
Power3 = function(x, a) {
  res = x ** a
  return(res)
}

print(Power3(3, 8))

# (e)
x = 1:10
y = Power3(x, 2)
par(mfrow = c(2, 2))
plot(x, y)
plot(x, y, log = "x")
plot(x, y, log = "y")
plot(x, y, log = "xy")
par(mfrow = c(1, 1))

# (f) 
PlotPower = function(x_list, a) {
  y = Power3(x_list, a)
  plot(x_list, y, log = "y")
}
PlotPower(1:10, 3)
