import java.util.*;

class railfence
{
    public static void encrypt(String plain)
    {
        String cipher="";
        for(int i=0;i<plain.length();i+=2)
        {
            char ch=plain.charAt(i);
            cipher+=ch;
        }
        for(int i=1;i<plain.length();i+=2)
        {
            char ch=plain.charAt(i);
            cipher+=ch;
        }
        System.out.println("Cipher Text: " + cipher);
    }
    public static void decrypt(String cipher)
    {
        String plain="";
        for(int i=0;i<cipher.length()/2+1;i++)
        {
            plain+=cipher.charAt(i);
            if(i==cipher.length()/2)
                break;
            plain+=cipher.charAt(i+cipher.length()/2+cipher.length()%2);
        }
        System.out.println("Plain Text: " + plain);
    }
    public static void main(String args[])
    {
        Scanner sc=new Scanner(System.in);
        int ch=1;
        while(ch!=0)
        {
            System.out.println("****************************************");
            System.out.println("Enter choice:");
            System.out.println("0. Exit");
            System.out.println("1. Encrypt");
            System.out.println("2. Decrypt");
            System.out.println("****************************************");
            ch=sc.nextInt();
            switch(ch)
            {
                case 1:
                    System.out.print("Enter plain text: ");
                    String plain=sc.next();
                    encrypt(plain);
                    break;
                case 2:
                    System.out.print("Enter cipher text: ");
                    String cipher=sc.next();
                    decrypt(cipher);
                    break;
                case 0:
                    System.out.print("Exiting...");
                    break;
            }   
        }
        sc.close();
    }
}