import java.util.*;

class Euclidean
{
    public static void main(String args[])
    {
        Scanner sc=new Scanner(System.in);

        System.out.print("Enter two numbers: ");
        int a=sc.nextInt();
        int b=sc.nextInt();

        while(b!=0)
        {
            int r=a%b;
            a=b;
            b=r;
        }

        System.out.println("GCD = "+a);

        sc.close();
    }
}