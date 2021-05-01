from random import shuffle
# defining a function:
# def function(param1: optionalType, param2: optionalType) -> optionalType:
#     do things
#     return value

# TASKS:
# 1. define functions that take a list of ints 
#    and return the same list with each element halved.
# a. do this with only a for-loop
# b. do this with a list comprehension
# c. do this with the "map" higher-order function

# 2. Write a function inRange(lowerBound, upperBound, l) to return all 
#    numbers in the input list within the range given by
#    the first two arguments (inclusive). 
#    For example, 
#        inRange(5, 10, list(range(1, 16))) == [5,6,7,8,9,10]
# a. do this with only a for-loop
# b. do this with only a list comprehension
# c. do this with the "filter" higher-order function

# 3. 
# a. Write a class Container that can hold an integer.
# b. Add methods to Container that allow you to set and access the internal integer.

# 4. 
# a. Write a class BinaryContainer that can hold two integers.
# b. Write four subclasses of BinaryContainer called
#    i.   AddExpr
#    ii.  SubExpr
#    iii. MultExpr
#    iv.  DivExpr
#    These subclasses must all have an eval() method that takes the two #    contained integers and performs the subclass's 
#    associated operation.
# c. Modify BinaryContainer such that it could contain itself.

#############################

def haf(x):
    return x/2

def haflup(array):
    arrayOut = []*len(array)
    for i in range(len(array)):
        arrayOut[i] = array[i]/2
    return arrayOut

def hafcmp(array): 
    return [haf(i) for i in array]

def hafmap(array):
    return list(map(haf, array))

#############################

def inRangeFor(lo, up, xs):
    out = []
    for i in xs:
        if i >= lo and i <= up:
            out.append(i)
    return out

def inRangeCmp(lo, up, xs):
    return [i for i in xs if i >= lo and i <= up]

def inRangeFil(lo, up, xs):
    return list(filter(lambda x: x >= lo and x <= up, xs))

#############################

class Container():
    def __init__(self):
        self.integ:int = 0

    def set(self, num):
        self.integ = num

    def access(self):
        return self.integ

#############################

# class BinaryContainer

# class AddExpr
# class SubExpr
# class MultExpr
# class DivExpr

if __name__ == "__main__":
    # ara = list(range(1, 16))
    # shuffle(ara)
    # print(ara)
    # a = 5
    # b = 10
    # print(inRangeFor(a, b, ara))
    # print(inRangeCmp(a, b, ara))
    # print(inRangeFil(a, b, ara))
    c = Container()
    c.set(10)
    print(c.access())
