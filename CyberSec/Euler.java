import java.util.*;

class Euler
{
    public static void main(String args[])
    {
        Scanner sc=new Scanner(System.in);

        System.out.print("Enter a: ");
        int a=sc.nextInt();

        System.out.print("Enter k: ");
        int k=sc.nextInt();

        System.out.print("Enter n: ");
        int n=sc.nextInt();

        int phi=0;

        for(int i=1;i<=n;i++)
        {
            int x=i;
            int y=n;

            while(y!=0)
            {
                int r=x%y;
                x=y;
                y=r;
            }

            if(x==1)
                phi++;
        }

        int r=k%phi;
        int result=1;

        for(int i=1;i<=r;i++)
        {
            result=(result*a)%n;
        }

        System.out.println("Phi(n) = "+phi);
        System.out.println("a^k mod n = "+result);

        sc.close();
    }
}