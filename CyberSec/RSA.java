import java.math.BigInteger;
import java.util.Scanner;

public class RSA {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        System.out.print("Enter p: ");
        BigInteger p = sc.nextBigInteger();

        System.out.print("Enter q: ");
        BigInteger q = sc.nextBigInteger();

        System.out.print("Enter message: ");
        BigInteger m = sc.nextBigInteger();

        BigInteger n = p.multiply(q);
        BigInteger phi = p.subtract(BigInteger.ONE).multiply(q.subtract(BigInteger.ONE));
        BigInteger e = BigInteger.valueOf(3);

        while (!e.gcd(phi).equals(BigInteger.ONE)) {
            e = e.add(BigInteger.TWO);
        }

        BigInteger d = e.modInverse(phi);

        BigInteger encrypted = m.modPow(e, n);
        BigInteger decrypted = encrypted.modPow(d, n);

        System.out.println("n = " + n);
        System.out.println("e = " + e);
        System.out.println("d = " + d);
        System.out.println("Encrypted = " + encrypted);
        System.out.println("Decrypted = " + decrypted);
    }
}