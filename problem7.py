import math
x = 0
y = 1
k = 1
c = 0
p = 0;
while x <= 10001
        -3:
    y += 2
    c = 1
    while c <= int(math.sqrt(y)) and k == 1:
        #p+=1
        #print(p)
        c += 2
        if y % c == 0:
            k = 0
    if k == 1:
        x += 1
    k = 1
    print(x)
