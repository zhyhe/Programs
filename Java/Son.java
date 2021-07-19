class door{}

class wood_Door extends door{}

class math{
	static public door getMes(){
		return new door();
	}
}

public class Son extends math{
	static public wood_Door getMes(){
		return new wood_Door();
	}

	public static void main(String args[]){
		Son m=new Son();
		System.out.println(m.getMes());
	}
}
