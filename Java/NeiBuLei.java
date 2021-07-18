class Wai{
	public void myVoid(){
		int i=9;
		class Nei{
			public void myNeiVoid(){
				System.out.println("外部类的局部变量为:"+i);
			}
		}
		Nei n=new Nei();
		n.myNeiVoid();
		//System.out.println("局部内部类的成员变量为:"+n.i);
	}
}
public class NeiBuLei{
	public static void main(String args[]){
		Wai w=new Wai();
		w.myVoid();
	}
}
