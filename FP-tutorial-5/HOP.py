alist = [i for i in range(10)]

def f(x):
    return True if x % 2 == 0 else False

evens = filter(f, alist)

