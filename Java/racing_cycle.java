abstract class bike{
	public String name = "component of abstract class";

	public String getMessage(){
		return name;
	}

	abstract public String getMes();
}

public class racing_cycle extends bike{

	public String color;
	public String speed;

	public racing_cycle(){}
	
	public racing_cycle(String color, String speed){
		this.color=color;
		this.speed=speed;
	}

	public void move(){
		System.out.println("fast");
	}

	public String getMes(){
		return getMessage();
	}

	public String toString(){
		return "This object discribes racing cycle!";
	}

	public static void main(String[] args){
		racing_cycle rc=new racing_cycle();
		System.out.println(rc);
		System.out.println(rc.getMes());
	}
}


