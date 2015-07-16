package dataflow;

public class VerySimple {
    static String x;

    public static void main(String[] args) {
        x = "5";
        f(x);
    }

    static void f(String x) {}
}
