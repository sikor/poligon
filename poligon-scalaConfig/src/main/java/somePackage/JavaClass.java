package somePackage;

public class JavaClass {

    private final String onlyArg;

    public static JavaClass javaFactory(String name) {
        return new JavaClass(name);
    }

    public JavaClass(String onlyArg) {
        this.onlyArg = onlyArg;
    }
}
