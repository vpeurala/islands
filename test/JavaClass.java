package test;

public class JavaClass implements Comparable<JavaClass> {
    public int compareTo(JavaClass o) {
	return Math.min(hashCode(), o.hashCode());
    }

    public Inner foo() {
	Inner i = new Inner();
	System.out.println(i.bar());
	return i;
    }

    class Inner {
	public String bar() {
	    return "hello";
	}
    }
}
