import java.util.Scanner;

public class NegativePoint {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        System.out.print("Enter x: ");
        int x = sc.nextInt();

        System.out.print("Enter y: ");
        int y = sc.nextInt();

        System.out.print("Enter p: ");
        int p = sc.nextInt();

        int negativeY = (p - y) % p;

        System.out.println("P = (" + x + ", " + y + ")");
        System.out.println("-P = (" + x + ", " + negativeY + ")");
    }
}