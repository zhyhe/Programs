#include<stdio.h>
int main(){
    int n,k,l,m;
    scanf("%d",&n);
    do{
        m=0;
        while(n>=3){
            k=n/3;
            m+=k;
            l=n%3;
            n=k+l;
        }
        if(n==2) m++;
        printf("%d\n",m);
        scanf("%d",&n);
    }while(n!=0);
}
