package dataflow;

public class Simple {

    static String x;
    static String y;
    String z;

    public static void main(String[] args) {
        x = "5";
        f(x + y);
    }

    static void f(String x) {
        y = x.toLowerCase().replace('0', x.charAt(x.indexOf(0)));
        new Simple().z = x + y;
        x = new Simple().z;
        System.out.println(x);
    }
}
