#include<stdio.h>
int main(){
    int i,N,m,b[1000];
    while(scanf("%d",&N)){
        for(i=0;i<1000;i++){
		b[i]=0;
		//printf("%d ",b[i]);
	}
        for(i=0;i<N;i++){
            scanf("%d",&m);
            (b[m-1])++;
		//printf("%d ",b[i]);
        }
        for(i=0;i<1000;i++){
		if(b[i]!=0) printf("%d\n",i+1);
	}
    }
}
