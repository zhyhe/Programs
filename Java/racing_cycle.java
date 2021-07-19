class bike
{
	public String color;  //自行车的颜色
	public int size;      //自行车的大小，即型号

	public bike(){};
	
	public bike(String color, int size){
		this.color=color;
		this.size=size;
	}

	public String toString(){
		String emp=color+" "+size;
		return emp;
	}




}

public class racing_cycle extends bike{
	public int speed;  //公路赛车的速度
	public racing_cycle(int speed){
		this.speed = speed;
	}
	public void getMes(){
		System.out.println(speed);
	}
	public static void main(String args[])
	{


		bike b = new bike("yello",43);
		System.out.println(b);

		racing_cycle r = new racing_cycle(122);
		r.color="blue";
		r.size=23;
		System.out.println(r);
		r.getMes();
		int a[]=new int[10];
		a[0]=11;
	}
}
