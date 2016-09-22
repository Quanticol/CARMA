/**
 * 
 */
package eu.quanticol.carma.simulator.space;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * @author loreti
 *
 */
public class SpaceModel {
	
	private int nodeCounter = 0;
	
	private final HashMap<NodeName, Node> vertexes;
	
	private final HashMap<String,Set<Node>> labels;
	
	public SpaceModel() {
		this.vertexes = new HashMap<>();
		this.labels = new HashMap<>();
	}

	public boolean addVertex( String name , Tuple t ) {
		Node v = vertexes.get(t);
		if (v == null) {
			vertexes.put(new NodeName(name,t), new Node(name, t));
		}
		return false;
	}
	
	public boolean addVertex( Object ... values ) {
		return addVertex( new Tuple( values ) );
	}
	
	public void setLabel( String label , Tuple ... values ) {
		HashSet<Node> set = new HashSet<>();
		for (Tuple t : values) {
			Node v = vertexes.get(t);
			if (v != null) {
				set.add(v);
			}
		}
		setLabel( label , set );
	}

	public void setLabel( String label , HashSet<Node> set ) {
		labels.put(label, set);
	}
	
	public void setLabel( String label , Predicate<NodeName> p ) {
		setLabel(label, getAll(p));
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
		return vertexes.get(new NodeName(name,t));
	}

	public Set<Node> getLabel( String label ) {
		Set<Node> set = labels.get(label);
		if (set == null) {
			set = new HashSet<>();
		}
		return set;
	}
	
	public HashSet<Node> getAll( Predicate<NodeName> p ) {
		HashSet<Node> set = new HashSet<>();
		for (Entry<NodeName, Node> entry: vertexes.entrySet()) {
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
	
	public class NodeName {
		
		private String name;
		
		private Tuple data;
		
		public NodeName( String name , Tuple data ) {
			this.name = name;
			this.data = (data==null?new Tuple():data);
		}

		public String getName() {
			return name;
		}

		public Tuple getData() {
			return data;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + getOuterType().hashCode();
			result = prime * result + ((data == null) ? 0 : data.hashCode());
			result = prime * result + ((name == null) ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			NodeName other = (NodeName) obj;
			if (!getOuterType().equals(other.getOuterType()))
				return false;
			if (data == null) {
				if (other.data != null)
					return false;
			} else if (!data.equals(other.data))
				return false;
			if (name == null) {
				if (other.name != null)
					return false;
			} else if (!name.equals(other.name))
				return false;
			return true;
		}

		private SpaceModel getOuterType() {
			return SpaceModel.this;
		}
		
		
		
		
	}
	
	public class Node {
		
		private final String name;
		
		private final Tuple data;
		
		private final int idx;
		
		private HashSet<Node> preset = new HashSet<>();
		private HashMap<Node,LinkedList<HashMap<String,Object>>> incomingData = new HashMap<>();
		private HashSet<Node> poset = new HashSet<>();
		private HashMap<Node,LinkedList<HashMap<String,Object>>> outgoingData = new HashMap<>();
		
		private Node( String name , Tuple data ) {
			this.name = name;
			this.data = data;
			this.idx = nodeCounter++;
		}

		public void addToPoset(Node l, HashMap<String,Object> data) {
			this.poset.add(l);
			LinkedList<HashMap<String,Object>> old = this.outgoingData.get(l);
			if (old == null) {
				old = new LinkedList<>();
				this.outgoingData.put(l, old);
			}
			old.add(data);
		}

		public void addToPreset(Node l, HashMap<String,Object> data) {
			this.preset.add(l);
			LinkedList<HashMap<String,Object>> old = this.outgoingData.get(l);
			if (old == null) {
				old = new LinkedList<>();
				this.incomingData.put(l, old);
			}
			old.add(data);
		}

		@Override
		public int hashCode() {
			return idx;
		}

		@Override
		public boolean equals(Object obj) {
			if (obj instanceof Node) {
				return this.idx==((Node) obj).idx;
			}
			return false;
		}

		@Override
		public String toString() {
			return (name != null?name:"")+data.toString();
		}
		
		public LinkedList<HashMap<String,Object>> getDataTo( Node l ) {
			return computeData(this.outgoingData.get(l));
		}
		
		public LinkedList<HashMap<String,Object>> getDataFrom( Node l ) {
			return computeData(this.incomingData.get(l));
		}
		
		private LinkedList<HashMap<String,Object>> computeData( LinkedList<HashMap<String,Object>> data ) {
			if (data == null) {
				return new LinkedList<>();
			}
			return data;
			
		}
		
		public HashSet<Node> getPreset() {
			return preset;
		}
		
		public HashSet<Node> getPoset() {
			return poset;
		}
		
		public <T> HashSet<T> getValuesTo( Node to , String label , Class<T> clazz ) {
			HashSet<T> result = new HashSet<T>();
			LinkedList<HashMap<String,Object>> edgesTo = outgoingData.get(to);
			if (edgesTo != null) {
				for (HashMap<String, Object> edge : edgesTo) {
					Object o = edge.get(label);
					if ((o != null)&&(clazz.isInstance(o))) {
						result.add(clazz.cast(o));
					}
				}
			}
			return result;
		}

		public boolean hasLabel( String label ) {
			return getLabel( label ).contains( this );
		}
		
		public String getName() {
			return name;
		}
	
		public Tuple getTuple() {
			return data;
		}
		
		public Object get( int i ) {
			return data.get(i);
		}
		
		public <T> T get( int i , Class<T> clazz) {
			return data.get(i, clazz);
		}
		
	}
	
	
}
