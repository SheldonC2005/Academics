import java.math.BigInteger;
import java.util.Scanner;

public class ElGamal {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        System.out.print("Enter prime p: ");
        BigInteger p = sc.nextBigInteger();

        System.out.print("Enter generator g: ");
        BigInteger g = sc.nextBigInteger();

        System.out.print("Enter private key x: ");
        BigInteger x = sc.nextBigInteger();

        System.out.print("Enter message: ");
        BigInteger m = sc.nextBigInteger();

        System.out.print("Enter random k: ");
        BigInteger k = sc.nextBigInteger();

        BigInteger y = g.modPow(x, p);

        BigInteger c1 = g.modPow(k, p);
        BigInteger c2 = m.multiply(y.modPow(k, p)).mod(p);

        BigInteger s = c1.modPow(x, p);
        BigInteger decrypted = c2.multiply(s.modInverse(p)).mod(p);

        System.out.println("Public Key y = " + y);
        System.out.println("Encrypted = (" + c1 + ", " + c2 + ")");
        System.out.println("Decrypted = " + decrypted);
    }
}