import java.util.*;
class vernam
{
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
                    plain=plain.toUpperCase();
                    System.out.print("Enter key: ");
                    String key=sc.next();
                    key=key.toUpperCase();
                    String cipher="";
                    for(int i=0;i<plain.length();i++)
                    {
                        char ch1=plain.charAt(i);
                        char ch2=key.charAt(i%key.length());
                        cipher+=(char)((ch1+ch2)%26+'A');
                    }
                    System.out.println("Cipher Text: " + cipher);
                    break;
                case 2:
                    System.out.print("Enter cipher text: ");
                    String cypher=sc.next();
                    cypher=cypher.toUpperCase();
                    System.out.print("Enter key: ");
                    String key1=sc.next();
                    key1=key1.toUpperCase();
                    String plain1="";
                    for(int i=0;i<cypher.length();i++)
                    {
                        char ch1=cypher.charAt(i);
                        char ch2=key1.charAt(i%key1.length());
                        plain1+=(char)((ch1-ch2+26)%26+'A');
                    }
                    System.out.println("Plain Text: " + plain1);
                    break;
                case 0:
                    System.out.print("Exiting...");
                    break;
            }
        }
        sc.close();
    }
}