import java.util.*;

class ExtendedEuclidean
{
    public static void main(String args[])
    {
        Scanner sc=new Scanner(System.in);

        System.out.print("Enter two numbers: ");
        int a=sc.nextInt();
        int b=sc.nextInt();

        int a1=a;
        int b1=b;

        int x=1;
        int y=0;
        int x1=0;
        int y1=1;

        while(b!=0)
        {
            int q=a/b;

            int t=a%b;
            a=b;
            b=t;

            t=x-q*x1;
            x=x1;
            x1=t;

            t=y-q*y1;
            y=y1;
            y1=t;
        }

        System.out.println("GCD = "+a);
        System.out.println("x = "+x);
        System.out.println("y = "+y);

        sc.close();
    }
}