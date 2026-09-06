import java.math.BigInteger;
import java.util.Scanner;

public class DigitalSignatureRSA {
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

        BigInteger signature = m.modPow(d, n);
        BigInteger verified = signature.modPow(e, n);

        System.out.println("Public Key = (" + e + ", " + n + ")");
        System.out.println("Private Key = (" + d + ", " + n + ")");
        System.out.println("Signature = " + signature);
        System.out.println("Verified Message = " + verified);

        if (m.equals(verified))
            System.out.println("Signature Valid");
        else
            System.out.println("Signature Invalid");
    }
}