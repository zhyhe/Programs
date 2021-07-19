import java.util.Date;

class fruit 
{
	static String color="yello";
	String size="big";

	static String getFruitColor(){
		return color;
	}
	String getFruitSize(){
		return size;
	}
}

public class apple extends fruit{
	static String appleColor="green";
	String appleSize="small";

	
	static String getFruitColor(){
		return appleColor;
	}
	String getFruitSize(){
		return appleSize;
	}

	public static void main(String args[]){
		fruit f=new apple();
		System.out.println(f.getFruitColor());
		System.out.println(f.getFruitSize());

	}
}

