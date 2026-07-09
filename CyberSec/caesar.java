import java.util.*;
class caesar
{
    public static void encrypt(String plain)
    {
        System.out.println("Cypher Text: ");
        for(int i=0;i<plain.length();i++)
        {
            char ch=plain.charAt(i);
            System.out.print((char)(ch+3));
        }
        System.out.println();
    }
    public static void decrypt(String cypher)
    {
        System.out.println("Plain Text: ");
        for(int i=0;i<cypher.length();i++)
        {
            char ch=cypher.charAt(i);
            System.out.print((char)(ch-3));
        }
        System.out.println();
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
                    System.out.print("Enter cypher text: ");
                    String cypher=sc.next();
                    decrypt(cypher);
                    break;
                case 0:
                    System.out.print("Exiting...");
                    break;
            }
        }
        sc.close();
    }
}