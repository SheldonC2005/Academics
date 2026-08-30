import java.util.*;

class Fermat
{
    public static void main(String args[])
    {
        Scanner sc=new Scanner(System.in);

        System.out.print("Enter a: ");
        int a=sc.nextInt();

        System.out.print("Enter n: ");
        int n=sc.nextInt();

        System.out.print("Enter prime p: ");
        int p=sc.nextInt();

        int r=n%(p-1);
        int result=1;

        for(int i=1;i<=r;i++)
        {
            result=(result*a)%p;
        }

        System.out.println("a^n mod p = "+result);

        sc.close();
    }
}