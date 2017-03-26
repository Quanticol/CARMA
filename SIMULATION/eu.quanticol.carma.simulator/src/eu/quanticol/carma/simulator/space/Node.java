package eu.quanticol.carma.simulator.space;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Predicate;

public class Node {
	
	/**
	 * 
	 */
	private final SpaceModel spaceModel;

	private final String name;
	
	private final Tuple data;
	
	private final int idx;
	
	private HashSet<Node> preset = new HashSet<>();
	private HashMap<Node,HashSet<Edge>> incomingData = new HashMap<>();
	private HashSet<Node> poset = new HashSet<>();
	private HashMap<Node,HashSet<Edge>> outgoingData = new HashMap<>();
	private HashSet<Edge> incomingEdges = new HashSet<>();
	private HashSet<Edge> outgoingEdges = new HashSet<>();
	
	Node( SpaceModel spaceModel, String name , Tuple data ) {
		this.spaceModel = spaceModel;
		this.name = name;
		this.data = data;
		this.idx = this.spaceModel.nodeCounter++;
	}

	public void addToPoset(Node l, HashMap<String,Object> data) {
		this.poset.add(l);
		HashSet<Edge> old = this.outgoingData.get(l);
		if (old == null) {
			old = new HashSet<>();
			this.outgoingData.put(l, old);
		}
		Edge e = new Edge(this, data, l); 
		old.add(e);
		outgoingEdges.add(e);
	}

	public void addToPreset(Node l, HashMap<String,Object> data) {
		this.preset.add(l);
		HashSet<Edge> old = this.incomingData.get(l);
		if (old == null) {
			old = new HashSet<>();
			this.incomingData.put(l, old);
		}
		Edge e = new Edge(l,data,this);
		old.add(e);
		incomingEdges.add(e);
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
	
	private LinkedList<HashMap<String,Object>> computeData( HashSet<Edge> hashSet ) {
		LinkedList<HashMap<String,Object>> toReturn = new LinkedList<>();
		if (hashSet != null) {
			for (Edge edge : hashSet) {
				toReturn.add(edge.getData());
			}
		}
		return toReturn;
		
	}
	
	public HashSet<Node> getPreset() {
		return preset;
	}
	
	public HashSet<Node> getPoset() {
		return poset;
	}
	
	public HashSet<Edge> getInEdges() {
		return incomingEdges;
	}
	
	public HashSet<Edge> getOutEdges() {
		return outgoingEdges;
	}
	
	public HashSet<Edge> getInEdges( Node from ) {
		return incomingEdges;
	}
	
	public HashSet<Edge> getOutEdges( Node to ) {
		return outgoingEdges;
	}
	
	public <T> HashSet<T> getValuesTo( Node to , String label , Class<T> clazz ) {
		HashSet<T> result = new HashSet<T>();
		HashSet<Edge> edgesTo = outgoingData.get(to);
		if (edgesTo != null) {
			for (Edge edge : edgesTo) {
				Object o = edge.getValue(label);
				if ((o != null)&&(clazz.isInstance(o))) {
					result.add(clazz.cast(o));
				}
			}
		}
		return result;
	}

	public boolean isInArea( String label ) {
		return this.spaceModel.getLabel( label ).contains( this );
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

	public int getIndex() {
		return idx;
	}
	
}