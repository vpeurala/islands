package test;

public class JavaClass extends Super implements Comparable<JavaClass> {
    private long a = 5;

    public int compareTo(JavaClass o) {
	return Math.min(hashCode(), o.hashCode());
    }

    public Inner foo() {
	Inner i = new Inner();
	System.out.println(i.bar());
	return i;
    }

    @Override
    void foobar() {
    }

    class Inner {
	public String bar() {
	    return "hello";
	}
    }
}

abstract class Super {
    abstract void foobar();
}
