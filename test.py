print("Hello Anusha!")
#variables
x, y, z = 5, 10, 15
'''how to make a 
multi-line comment'''

print(z-x)
#you cannot combine a string with a number

print(type(x))

a = str(x)

print("The value of x is " + a)

#to make Random numbers
import random
#print random number between 1 and 9

#random.randrange(start,stop+1)
print(random.randrange(1 , 10))
#random.randint(start,stop)
print(random.randint(1,10))

#random.choice(sequance or string)
x = [2,4,6,8,10,12,14,16]
print(random.choice(x))
y = "ANUSHA"
print(random.choice(y))
#random.choices(sequence,weights of each value, length of list)
print(random.choices(y,weights=[1,1,1,1,1,1],k=2))

def myfunction():
    return 0.1
#random.shuffle(sequence,varying value between 0.0-1.0)
random.shuffle(x, myfunction)
print(x)

b = "DECEMBER"
print(b[1])
print(b[3:5])
print(len(b))

c = "  DECEMBER  "
print(c.strip())
print(c.lower())
print(c.strip().lower())
print(c.replace("E","3"))

d = 22
e = "I am {} years old"
print(e.format(d))
f = 5
g = "I am {} years old and {} feet tall"
print(g.format(d,f))

for h in x:
    if h > 5:
        print(h)

x.append(100)
x.insert(3,44)
print(x)

i = x.copy()
j = list(x)
if i==j:
    print("True copy")
x.reverse()
print(x)
x.sort()
print(x)

k = 1
while k < 5:
    print(k)
    k+=1

def egg(style):
    print("I like my eggs " + style)

egg("boiled")

def five(x):
    return(x*5)

print(five(3))

def tri_rec(x):
    if x > 0:
        print(x)
        tri_rec(x-1)
    else:
        result = 0

tri_rec(3)

class CountN:
    def __iter__(self):
        self.a = 1
        return self

    def __next__(self):
        if self.a <= 20:
            x = self.a
            self.a += 1
            return x
        else:
            raise StopIteration

myiter = iter(CountN())
for x in myiter:
    print(x)

#open a random webpage
import webbrowser
pageurl = 'https://www.tumblr.com/dashboard'
#webbrowser.open(URL,new tab new window or same window)
webbrowser.open(pageurl,new=2)

print("Enter Name")
x = input()
print("Hi, " + x)