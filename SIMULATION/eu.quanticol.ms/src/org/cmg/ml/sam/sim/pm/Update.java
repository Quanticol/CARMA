/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

/**
 * @author loreti
 *
 */
public class Update {

	private Map<Integer,Integer> update;
	private String name;
	
	public Update( String name ) {
		this.update = new HashMap<>();
		this.name = name;
	}
	
	public Set<Entry<Integer, Integer>> getUpdate() {
		return update.entrySet();
	}

	public synchronized void add(int idx , int c, int p) {
		if (c != p) {
			int drift = update.getOrDefault(idx,0)+p-c;
			if (drift!=0) {
				update.put(idx, drift);
			} else {
				update.remove(idx);
			}
		}
	}

	public int get(int i) {
		return update.getOrDefault(i,0);
	}

	public void consume(int idx, int c) {
		this.add(idx, c, 0);
	}

	public void produce(int idx, int p) {
		this.add(idx, 0, p);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return name+":"+update.toString();
	}

}
