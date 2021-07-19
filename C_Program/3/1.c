#include<stdio.h>
int main(){
    int i,j,N,M;
    unsigned int no,sc,sss;
    char ch;
    int score[30001];
    int sco;
    while(scanf("%d%d",&N,&M)!=EOF){
        for(i=1;i<=N;i++){
            scanf("%d",&score[i]);
	    printf("asssss%d,%d\n",score[i],M);
        }
        for(i=0;i<M;i++){
    		scanf("\n%c%d%d",&ch,&no,&sc);
		printf("%c\n",ch);
    		switch(ch){
    			case('Q'):{
					  sco=0;
					  if(no>sc){
						  sss=no;
						  no=sc;
						  sc=sss;
					  }

		  			  for(j=no;j<=sc;j++){
		      				  if(sco<score[j]) sco=score[j];
					  }
					  printf("%d\n",sco);
					  break;
				  }
			case('U'):{
					  score[no]=sc;
					  break;
				  }
			default:
				  printf("error\n");
				  break;
		}
	}
    }
}
