package dataflow;

public class Simple {

    static String x;
    static int y;

    public static void main(String[] args) {
        x = "5";
        foo(x);
        if (args.length > 0) {
            foo(x + "7");
        } else {
            foo(x + "9");
        }
        y = x.indexOf('a');
        while (x.contains("1")) {
            y = x.length();
            x = x + x;
        }
    }

    static int foo(String x) {
        y = 3;
        x = Integer.toString(y);
        y = Integer.valueOf(x);
        return x.length();
    }
}
