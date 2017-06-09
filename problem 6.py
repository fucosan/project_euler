import math
x = 100
y = 0
z = 0
while x > 0:
    y += x
    z += math.pow(x,2)
    x -= 1

y = math.pow(y,2)
print(y," ", z, " ", y-z)



