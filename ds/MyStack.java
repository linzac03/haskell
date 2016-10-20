public class MyStack {
	protected final int defCap = 100;
	protected Object[] stack;
	protected int numElements = 0;
	protected int front = 0;
	protected int rear = -1;

	public MyStack(int size){
		if (size == 0)
			stack = new Object[defCap];
		else {
			stack = new Object[size];
		}
		rear = size;
		front = size - 1;
	}

	public boolean isEmpty() {
		return (numElements == 0);
	}

	public boolean isFull() {
		return (numElements == stack.length);
	}

	public void push(Object element){
		if (isFull()) {
//			throw new Exception(
//					"Push attempted on a full stack");
			System.out.println("full tack cant poosh");			
		} else {
			rear = (rear - 1) % stack.length;
			stack[rear] = element;
			numElements = numElements + 1;
		}
	}

	public Object pop() {
		if (isEmpty()){
//			throw new Exception(
//					"pop attempted on empty queue.");
			System.out.println("popped atempt no good");
			return null;
		} else {
			Object toReturn = stack[front];
			stack[front] = null;
			front = (front - 1) % stack.length;
			numElements = numElements - 1;
			return toReturn;
		}
	}

	public void display() {
		for (Object ob : stack){
			System.out.println(ob);
		}
	}

	public static void main(String[] args) {
		MyStack test = new MyStack(15);
		test.display();
		String[] testString = new String[3];
		test.push(testString);
		test.display();
	}
}
