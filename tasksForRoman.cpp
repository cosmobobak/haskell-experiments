#include <iostream>
#include <vector>
#include <algorithm>

template <typename T>
void print_vec(std::vector<T> v) {
    std::cout << "{ ";
    for (auto x : v) std::cout << x << ", ";
    std::cout << "}\n";
}

// TASKS:
// 1. define functions that take a vector of ints
//    and return the same vector with each element halved.
// a. do this with only a for-loop
// b. do this with an enhanced for-loop
// c. do this with the std::transform algorithm.

// 2. Write a function inRange(lowerBound, upperBound, l) to return all
//    numbers in the input list within the range given by
//    the first two arguments (inclusive).
//        For example, inRange(5, 10, {1, 6, 15}) == {6}
// a. do this with only a for-loop
// b. do this with the std::remove_if algorithm.

// 3.
// a. Write an abstract class Expr that gives type
//    signatures for the virtual show() and eval() methods.
// b. Write a class UnitaryExpr that inherits from Expr that can
//    hold an integer.
// c. Add methods to UnitaryExpr that allow you
//    to set and access the internal integer.

// 4.
// a. Write a class BinaryExpr that can hold two integers, and that defines the methods getLeft() and getRight().
// b. Write four subclasses of BinaryExpr called
//    i.   AddExpr
//    ii.  SubExpr
//    iii. MultExpr
//    iv.  DivExpr
//    These subclasses must all have an eval() method that takes
//    the two contained integers and performs the subclass's
//    associated operation.

/////////////////////////////////////////

auto haf(int x) -> int {
    return x / 2;
};

auto haflup(std::vector<int> xs) -> std::vector<int> {
    for (int i = 0; i < xs.size(); i++) {
        xs[i] = xs[i] / 2;
    };
    return xs;
};

auto hafelp(std::vector<int> xs) -> std::vector<int> {
    for (int &i : xs) {
        i = i / 2;
    }
    return xs;
}

auto hafmap(std::vector<int> xs) -> std::vector<int> {
    std::transform(xs.begin(), xs.end(), xs.begin(), haf);
    return xs;
};  

/////////////////////////////////////////

auto inRangeFor() -> void {}

// auto inRangeFil() -> void {}

/////////////////////////////////////////

// class Container

/////////////////////////////////////////

// class BinaryContainer

// class AddExpr
// class SubExpr
// class MultExpr
// class DivExpr

int main(int argc, char const *argv[]) {
    std::vector<int> ara;
    for (int i = 1; i < 16; i++) ara.push_back(i);
    int a = 5, b = 10;
    
    // print_vec(haflup(ara));
    // print_vec(hafelp(ara));
    // print_vec(hafmap(ara));

    // print_vec(inRangeFor(a, b, ara));
    // print_vec(inRangeFil(a, b, ara));

    return 0;
}
