#include<stdio.h>
#include<stdlib.h>
int main(){
    int N,i,all,c3;
    int *p;
    int a[1000];
    while(scanf("%d",&N)){
        if(N>999) N=1000;
	if(N<1) continue;
        p=a;
        c3=0;
        do{
            all=0;
            for(p=a;p<a+N;p++){
                if(*p!=-1){
                    all++;
                    c3++;
        //printf("%d  ",*p);
                    if(c3%3==0){
                        c3=0;
                        *p=-1;
                    }
                }
            }
	    if(all==0){
		    printf("error\n");
		    break;
	    }
        //printf("%d\n",all);
        }while(all!=1);
	for(i=0;i<N;i++){
		if(a[i]!=-1) printf("%d\n",i);
		a[i]=i;
	}
    }
}
