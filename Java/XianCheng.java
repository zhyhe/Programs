class MyRunnable1 implements Runnable{
	public void run(){
		for (int i=0;i<100;i++){
			
			System.out.print("@");
		/*	try{
				Thread.sleep(50);
			}
			catch(InterruptedException e){
				e.printStackTrace();
			}
			*/
		}
	}
}
class MyRunnable2 implements Runnable{
	public void run(){
		for (int i=0;i<100;i++){
			
			System.out.print("$");
			/*try{
				Thread.sleep(50);
			}
			catch(InterruptedException e){
				e.printStackTrace();
			}*/
		}
	}
}

public class XianCheng{
	public static void main(String args[]){
		MyRunnable1 mr1=new MyRunnable1();
		MyRunnable2 mr2=new MyRunnable2();
		Thread t1=new Thread(mr1);
		Thread t2=new Thread(mr2);
		t1.start();
		t2.start();
		t1.setPriority(10);
		t2.setPriority(1);
	}
}
