/**
 * 
 */
package eu.quanticol.carma.simulator.space;

import java.util.HashMap;

/**
 * @author loreti
 *
 */
public class Edge {

	
	private Node n1;
	private HashMap<String, Object> data;
	private Node n2;

	public Edge( Node n1 , HashMap<String,Object> data , Node n2 ) {
		this.n1 = n1;
		this.data = data;
		this.n2 = n2;
	}
	
	public Node getSource() {
		return n1;
	}
	
	public Node getTarget() {
		return n2;
	}
	
	public Object getValue( String label ) {
		return data.get(label);
	}

	public <T> T getValue( String label , Class<T> clazz ) {
		Object o = data.get(label);
		if (clazz.isInstance(o)) {
			return clazz.cast(o);
		}
		return null;
	}


	public HashMap<String, Object> getData() {
		return data;
	}
	
}
