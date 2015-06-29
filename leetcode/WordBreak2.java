
import java.util.*;
import java.util.stream.*;

public class WordBreak2 {
    public List<String> wordBreak(String s, Set<String> wordDict) {
        Map<String, List<List<String>>> memo = new HashMap<>();
        memo.put("", Arrays.asList(Collections.<String>emptyList()));

        return wordBreak(s, wordDict, memo).stream()
            .map(ss -> String.join(" ", ss)).collect(Collectors.toList());
    }

    private List<List<String>> wordBreak(String s, Set<String> dict, Map<String, List<List<String>>> memo) {
        if (memo.containsKey(s))
            return memo.get(s);
        List<List<String>> result = dict.stream()
            .filter(s::startsWith)
            .flatMap(w -> wordBreak(s.substring(w.length()), dict, memo).stream().map(sub -> cat(w, sub)))
            .collect(Collectors.toList());
        memo.put(s, result);
        return result;
    }

    private static <T> List<T> cat(T s, List<T> ss) {
        List<T> result = new ArrayList<>();
        result.add(s);
        result.addAll(ss);
        return result;
    }

    public static void main(String[] args) {
        Object o = new Solution().wordBreak("catsanddogs", new TreeSet<String>(Arrays.asList("cats", "and", "dogs", "cat", "sand")));
        System.out.println(o);
    }
}