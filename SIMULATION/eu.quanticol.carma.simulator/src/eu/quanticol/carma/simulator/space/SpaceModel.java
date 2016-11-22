/**
 * 
 */
package eu.quanticol.carma.simulator.space;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * @author loreti
 *
 */
public class SpaceModel {
	
	int nodeCounter = 0;
	
	private final HashMap<Location, Node> vertexes;
	
	private final HashMap<String,Set<Node>> labels;

	private HashMap<Set<String>, HashMap<Node,HashMap<Node, Double>>> distances;
	
	public SpaceModel() {
		this.vertexes = new HashMap<>();
		this.labels = new HashMap<>();
	}

	public boolean addVertex( String name , Tuple t ) {
		Node v = vertexes.get(t);
		if (v == null) {
			vertexes.put(new Location(name,t), new Node(this, name, t));
		}
		return false;
	}
	
	public boolean addVertex( Object ... values ) {
		return addVertex( new Tuple( values ) );
	}
	
	public void setArea( String label , Tuple ... values ) {
		HashSet<Node> set = new HashSet<>();
		for (Tuple t : values) {
			Node v = vertexes.get(t);
			if (v != null) {
				set.add(v);
			}
		}
		setArea( label , set );
	}

	public void setArea( String label , HashSet<Node> set ) {
		labels.put(label, set);
	}
	
	public void setArea( String label , Predicate<Location> p ) {
		setArea(label, getAll(p));
	}

	public boolean addEdge( Node n1 , HashMap<String,Object> data , Node n2 ) {
		if ((n1 != null)&&(n2 != null)) {
			n1.addToPoset( n2 , data );
			n2.addToPreset( n1 , data );
			return true;
		}
		return false;
	}

	public boolean addEdge( String name1 , Tuple t1 , HashMap<String,Object> data , String name2 , Tuple t2 ) {
		Node v1 = getVertex(name1, t1);
		Node v2 = getVertex(name2,t2);
		if ((v1 != null)&&(v2 != null)) {
			v1.addToPoset( v2 , data );
			v2.addToPreset( v1 , data );
			return true;
		}
		return false;
	}
	
	public Node getVertex(Tuple t) {
		return getVertex( null , t );
	}
	
	public Node getVertex( String name ) {
		return getVertex( name , new Tuple() );
	}
	
	public Node getVertex( String name , Tuple t ) {
		return vertexes.get(new Location(name,t));
	}

	public Set<Node> getLabel( String label ) {
		Set<Node> set = labels.get(label);
		if (set == null) {
			set = new HashSet<>();
		}
		return set;
	}
	
	public HashSet<Node> getAll( Predicate<Location> p ) {
		HashSet<Node> set = new HashSet<>();
		for (Entry<Location, Node> entry: vertexes.entrySet()) {
			if (p.test(entry.getKey())) {
				set.add(entry.getValue());
			}
		}
		return set;
	}
	
	public HashSet<Node> getAll() {
		HashSet<Node> result = new HashSet<>();
		result.addAll(vertexes.values());
		return result;
	}
		
}
