#include<stdio.h>
#include<stdlib.h>
struct num{
	int l;
	struct num *next;
};
struct num *CreateList(int n){
	struct num *head;
	struct num *rear;
	struct num *p;
	int i;
	head=NULL;
	for(i=0;i<n;i++){
		p=(struct num*)malloc(sizeof(struct num));
		p->l=i;
		if(head==NULL)	head=p;
		else rear->next=p;
		rear=p;
	}
	if(rear!=NULL) rear->next=head;
	return head;
}
void printList(struct num *hea){
	struct num *p=hea;
	do{
		printf("%d ",p->l);
		p=p->next;
	}while(p!=hea);
	printf("\n");
}
int main(){
    int n;
    scanf("%d",&n);
    struct num *p;
    p=CreateList(n);
    int i=1;
    while(p->next!=p){
	    if(i%2==0) p->next=p->next->next;
	    p=p->next;
	    i++;
    }
    printf("%d\n",p->l);
}


	
