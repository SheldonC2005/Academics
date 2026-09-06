import java.math.BigInteger;
import java.util.Scanner;

public class PointDoubling {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        System.out.print("Enter x: ");
        BigInteger x = sc.nextBigInteger();

        System.out.print("Enter y: ");
        BigInteger y = sc.nextBigInteger();

        System.out.print("Enter a: ");
        BigInteger a = sc.nextBigInteger();

        System.out.print("Enter p: ");
        BigInteger p = sc.nextBigInteger();

        BigInteger numerator = x.multiply(x)
                .multiply(BigInteger.valueOf(3))
                .add(a);

        BigInteger denominator = y.multiply(BigInteger.TWO)
                .modInverse(p);

        BigInteger lambda = numerator.multiply(denominator).mod(p);

        BigInteger x3 = lambda.multiply(lambda)
                .subtract(x.multiply(BigInteger.TWO))
                .mod(p);

        BigInteger y3 = lambda.multiply(x.subtract(x3))
                .subtract(y)
                .mod(p);

        System.out.println("2P = (" + x3 + ", " + y3 + ")");
    }
}