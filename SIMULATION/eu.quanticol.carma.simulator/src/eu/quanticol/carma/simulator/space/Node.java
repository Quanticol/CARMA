package eu.quanticol.carma.simulator.space;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

public class Node {
	
	/**
	 * 
	 */
	private final SpaceModel spaceModel;

	private final String name;
	
	private final Tuple data;
	
	private final int idx;
	
	private HashSet<Node> preset = new HashSet<>();
	private HashMap<Node,LinkedList<HashMap<String,Object>>> incomingData = new HashMap<>();
	private HashSet<Node> poset = new HashSet<>();
	private HashMap<Node,LinkedList<HashMap<String,Object>>> outgoingData = new HashMap<>();
	
	Node( SpaceModel spaceModel, String name , Tuple data ) {
		this.spaceModel = spaceModel;
		this.name = name;
		this.data = data;
		this.idx = this.spaceModel.nodeCounter++;
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
	
	public Double getMinDistance( Node n , Set<String> labels ) {
		double result = Double.MAX_VALUE;
		
		for (HashMap<String, Object> edges : this.outgoingData.get(n)) {
			
		}
		
		return result;
	}
	
}