public class MySet extends ArrayList<Object>{
	//This code does not work, but it illustrates the idea
	public boolean member(Object o){
		//Check if o is in set
		for(Object elem : this){
			if(elem.equals(o)){
				return true;
			}
		}
		return false;
	}

	public MySet insert(Object o){
		//Insert object o into set
		this.add(o);
	}

	public MySet remove(int n){
		//Remove object at index n from set
		this.remove(n);
	}		
}
