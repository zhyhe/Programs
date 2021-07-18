#include<stdio.h>
int main(){
	char alpha[52],n[52],i,j,k;
	char get[100];
	while(scanf("%s",get)!=EOF){
		for(i=0;i<52;i++){
			alpha[i]=0;
			n[i]=0;
		}
		i=0;
		k=0;
		//printf("%s",get);
		while(get[i]!='\0'){
			j=get[i]-'A';
			if(n[j]==0) n[j]=++k;
		//printf("%3d,%c,%3d\n",i,get[i],n[j]);
			i++;
		}
		//printf("%d\n",j);
		for(j=0;j<52;j++){
			k=n[j];
			if(k!=0) alpha[k]=j+'A';
		}
		for(j=0;j<52;j++){
			printf("%c",alpha[j]);
		}
		printf("\n");
	}
}
