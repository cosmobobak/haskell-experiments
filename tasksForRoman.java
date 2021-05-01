import java.util.Arrays;

// TASKS:
// 1. define functions that take an array of ints 
//    and return the same array with each element halved.
// a. do this with only a for-loop
// b. do this with an enhanced for-loop

// 2. Write a function inRange(lowerBound, upperBound, l) to return all 
//    numbers in the input list within the range given by
//    the first two arguments (inclusive). 
//        For example, inRange(5, 10, {1, 6, 15}) == {6}
// a. do this with only a for-loop
// b. do this with the "filter" higher-order function

class tasksForRoman {
    // everything has to be enclosed in a class in java

    /////////////////////////////////////////

    static int haf(int x) {
        return x / 2;
    }

    static int[] haflup(int[] xs) {
        for (int i = 0; i < xs.length; i++) {
            xs[i] = xs[i] / 2;
        }
        ;
        return xs;
    }

    static int[] hafelp(int[] xs) {
        for (int i : xs) {
            i = i / 2;
        }
        return xs;
    }

    static int[] hafmap(int[] xs) {
        return Arrays.stream(xs).map(e -> haf(e)).toArray();
    }

    /////////////////////////////////////////

    static int[] inRangeFor(int lb, int ub, int[] ltht) {
        int counter = 0;
        for (int i = 0; i < ltht.length; i++) {
            if (ltht[i] >= lb && ltht[i] <= ub) {
                counter++;
            }
        }
        int[] xs = new int[counter];
        for (int i = 0, x = 0; i < ltht.length; i++) {
            if (ltht[i] >= lb && ltht[i] <= ub) {
                xs[x] = ltht[i];
                x++;
            }
        }
        return xs;
    }

    static int[] inRangeFil(int lb, int ub, int[] ltht) {
        return Arrays.stream(ltht).filter(x -> x >= lb && x <= ub).toArray();
    }

    public static void main(String[] args) {
        int[] ara = { 2, 4, 6, 8, 6, 6, 130, -12, 45, 2, 4, 8, 7, 6, 5, 10 };
        int a = 5, b = 10;
        System.out.println(Arrays.toString(ara));
        System.out.println(Arrays.toString(inRangeFor(a, b, ara)));
        System.out.println(Arrays.toString(inRangeFil(a, b, ara)));
    };
};