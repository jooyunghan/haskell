package happynumber;

import java.util.*;
import java.util.stream.*;

public class Solution {
    private int digitsSquareSum(int n) {
        int sum = 0;
        while (n > 0) {
           int d = n % 10;
           sum += d * d;
           n /= 10;
       }
       return sum;
    }
    private int recurrent(IntStream ints) {
        Set<Integer> memo = new HashSet<>();
        PrimitiveIterator.OfInt it = ints.iterator();
        while (it.hasNext()) {
           int n = it.nextInt();
           if (memo.contains(n)) return n;
           memo.add(n);
        }
        throw new NoSuchElementException();
    }
    public boolean isHappy(int n) {
        IntStream series = IntStream.iterate(n, this::digitsSquareSum);
        return 1 == recurrent(series);
    }
    public static void main(String[] args) {
        Solution s = new Solution();
        for (int i=10; i<=20; i++) {
           System.out.println(s.isHappy(i));
       }
    }
}