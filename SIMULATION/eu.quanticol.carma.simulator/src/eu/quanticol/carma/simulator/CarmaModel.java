/**
 * 
 */
package eu.quanticol.carma.simulator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

import org.cmg.ml.sam.sim.SimulationFactory;
import org.cmg.ml.sam.sim.sampling.Measure;

/**
 * @author loreti
 *
 */
public abstract class CarmaModel {

	public abstract String[] getSystems();

	public abstract SimulationFactory<CarmaSystem> getFactory(String name);

	public abstract String[] getMeasures();

	public abstract String[] getMeasureParameters(String name);

	public abstract Map<String, Class<?>> getParametersType(String name);

	public abstract Measure<CarmaSystem> getMeasure(String name, Map<String, Object> parameters);

	public Measure<CarmaSystem> getMeasure(String name) {
		return getMeasure(name, new HashMap<String, Object>());
	}

	public boolean checkParameterType(String measure, Map<String, Object> parameters) {
		String[] names = getMeasureParameters(measure);
		if (names == null) {
			return false;
		}
		if (parameters.size() != names.length) {
			return false;
		}
		Map<String, Class<?>> types = getParametersType(measure);
		for (String n : names) {
			Class<?> t = types.get(n);
			Object v = parameters.get(n);
			if ((t == null) || (v == null) || (!t.isInstance(v))) {
				return false;
			}
		}
		return true;
	}

	public boolean carmaEquals(Object o1, Object o2) {
		if (o1 == o2) {
			return true;
		}
		if ((o1 == null) || (o2 == null)) {
			return false;
		}
		return o1.equals(o2);
	}

	public <T> LinkedList<T> getList(T... elements) {
		LinkedList<T> foo = new LinkedList<T>();
		for (int i = 0; i < elements.length; i++) {
			foo.add(elements[i]);
		}
		return foo;
	}

	public <T> T get(LinkedList<T> list, int idx) {
		if (list == null) {
			return null;
		}
		if ((idx < 0) || (idx >= list.size())) {
			return null;
		}
		return list.get(idx);
	}

	public <T> HashSet<T> getSet(T... elements) {
		HashSet<T> foo = new HashSet<T>();
		for (int i = 0; i < elements.length; i++) {
			foo.add(elements[i]);
		}
		return foo;
	}

	public <T> LinkedList<T> concatenate(LinkedList<T> l1, LinkedList<T> l2) {
		LinkedList<T> result = new LinkedList<T>();
		result.addAll(l1);
		result.addAll(l2);
		return result;
	}

	public <T> HashSet<T> union(HashSet<T> s1, HashSet<T> s2) {
		HashSet<T> result = new HashSet<T>();
		result.addAll(s1);
		result.addAll(s2);
		return result;
	}

	public <T> HashSet<T> intersection(HashSet<T> s1, HashSet<T> s2) {
		HashSet<T> result = new HashSet<T>();
		result.addAll(s1);
		result.retainAll(s2);
		return result;
	}

	public <T> HashSet<T> removeAll(HashSet<T> s1, HashSet<T> s2) {
		HashSet<T> result = new HashSet<T>();
		result.addAll(s1);
		result.removeAll(s2);
		return result;
	}

	public <T> LinkedList<T> removeAll(LinkedList<T> s1, LinkedList<T> s2) {
		LinkedList<T> result = new LinkedList<T>();
		result.addAll(s1);
		result.removeAll(s2);
		return result;
	}

	public <T, R> LinkedList<R> map(LinkedList<T> s1, Function<T, R> f) {
		LinkedList<R> result = new LinkedList<R>();
		for (T v : s1) {
			result.add(f.apply(v));
		}
		return result;
	}

	public <T> LinkedList<T> filter(LinkedList<T> s1, Predicate<T> f) {
		LinkedList<T> result = new LinkedList<T>();
		for (T v : s1) {
			if (f.test(v)) {
				result.add(v);
			}
		}
		return result;
	}

	public <T, R> HashSet<R> map(HashSet<T> s1, Function<T, R> f) {
		HashSet<R> result = new HashSet<R>();
		for (T v : s1) {
			result.add(f.apply(v));
		}
		return result;
	}

	public <T> HashSet<T> filter(HashSet<T> s1, Predicate<T> f) {
		HashSet<T> result = new HashSet<T>();
		for (T v : s1) {
			if (f.test(v)) {
				result.add(v);
			}
		}
		return result;
	}

	public <T> T pick(HashSet<T> s1) {
		return pick(s1, x -> true);
	}

	public <T> T pick(HashSet<T> s1, Predicate<T> f) {
		T result = null;
		for (T v : s1) {
			if ((result == null) && (f.test(v))) {
				result = v;
			}
		}
		if (result != null) {
			s1.remove(result);
		}
		return result;
	}

	public <T> T pick(LinkedList<T> s1, Predicate<T> f) {
		for (int i = 0; i < s1.size(); i++) {
			T v = s1.get(i);
			if (f.test(v)) {
				s1.remove(i);
				return v;
			}
		}
		return null;
	}

	public <T> boolean exist(Collection<T> c, Predicate<T> f) {
		for (T v : c) {
			if (f.test(v)) {
				return true;
			}
		}
		return false;
	}

	public <T> boolean forall(Collection<T> c, Predicate<T> f) {
		for (T v : c) {
			if (!f.test(v)) {
				return false;
			}
		}
		return true;
	}

	public <T> T head(LinkedList<T> l) {
		return l.peekFirst();
	}

	public <T> int computeSize(Collection<T> l) {
		return l.size();
	}
	
	public <T> LinkedList<T> tail( LinkedList<T> l ) {
		if (l.isEmpty()) {
			return new LinkedList<>();
		} else {
			LinkedList<T> newList = new LinkedList<>();
			newList.addAll(l);
			newList.remove();
			return newList;
		}
	}

	public LinkedList<Integer> generateIntervalList(int min, int max) {
		LinkedList<Integer> toReturn = new LinkedList<>();
		for (int i = min; i < max; i++) {
			toReturn.add(i);
		}
		return toReturn;
	}
	
	public <R,T> T reduce( Collection<R> collection , BiFunction<R, T, T> function , T base ) {
		T result = base;
		for (R r : collection) {
			result = function.apply(r, result);
		}		
		return result;
	}

}
