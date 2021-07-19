#include<stdio.h>
#include<stdlib.h>
#include<math.h>
int main(){
	int i,j;
	int M,N;

	scanf("%d %d",&N,&M);
	printf("M=%d,N=%d\n",M,N);

	int *pb;
	pb=(int *)malloc(N*sizeof(int));
	if(pb==NULL){
		printf("内存不足\n");
		exit(1);
	}
	for(i=1;i<=N;i++){
		scanf("%d",&pb[i-1]);
		printf("p[%d]=%d ",i-1,pb[i-1]);
	}
	printf("\n");

	char c;
	int k,l,o;

	for(i=1;i<=M;i++){
		scanf("\n%c %d %d",&c,&k,&l);
		switch(c){
			case('Q'):
				o=pb[k-1];
				for(j=k;j<l;j++) if(pb[j]>o) o=pb[j];
				printf("%d到%d的最高成绩是%d\n",k,l,o);
				break;

			case('U'):
				pb[k-1]=l;
				for(o=1;o<=N;o++){
					printf("p[%d]=%d ",o-1,pb[o-1]);
				}
				printf("\n");
				break;
			default:
				printf("ERROR");
				exit(1);
		}
	}
}
