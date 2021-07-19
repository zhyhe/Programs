#include<iostream>
#include<string>
using namespace std;
typedef struct file{
	string name;
	int no;
}FIL;
bool comp(FIL a,FIL b){
	char *cpa,*cpb;
	
	short as,bs;
	short al,bl;
	if( a.no != b.no ) return false;
	else{
		as=a.name.size(); bs=b.name.size();
		cpa=new char[as+1];cpb=new char[bs+1];
		al=a.name.find('\\');
		bl=b.name.find('\\');
		
		//cpa=a.name;
		//cpb=b.name;
		cout<<a.name<<bs<<al<<bl<<endl;



		//find_last_of('\\')
		return true;
	}
}
int main(){
	FIL t,u;
	int i;
	t={"E:hello\love.c",432}; u={"E:hello\love.c",432};
	cout << comp(t,u)<< endl;
	while(cin >> t.name >> t.no){
		cout<<t.name.find('\\')<<' '<<t.no <<endl;
	}

}
