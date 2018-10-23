/**
 * 
 */
package eu.quanticol.carma.tools.monitoring.jsstl;

import java.util.LinkedList;
import java.util.function.BiFunction;

/**
 * @author loreti
 *
 */
public class SlidingWindow<D> {
	
	private final double size;
	private final double start;
	private final double end;
	private LinkedList<SignalPoint<D>> elements;
	private SignalPoint<D> next;
	private final BiFunction<D, D, D> operator;
	private double gap = -1;
	
	public SlidingWindow( double start, double end , BiFunction<D, D, D> operator ) {
		if (start>=end) {
			throw new IllegalArgumentException();
		}
		this.start = start;
		this.end = end;
		this.size = start-end;
		this.elements = new LinkedList<>();
		this.operator = operator;
	}
	
	public boolean isFull( ) {
		return this.gap == this.size;
	}
	
	public boolean canShift() {
		return isFull()&&(next != null);
	}
	
	public void shift() {
		if (!this.canShift()) {
			throw new IllegalStateException();
		}
		SignalPoint<D> first = elements.removeFirst();
		SignalPoint<D> second = elements.getFirst();
		SignalPoint<D> last = elements.getLast();
		double startGap = second.getTime()-first.getTime();
		double endGap = next.getTime()-last.getTime();
		if (startGap<endGap) {
			elements.addLast(new SignalPoint<D>(second.getTime()+size,last.getValue()));
		} else {
			elements.addLast(next);
			if (startGap>endGap) {
				elements.addFirst(new SignalPoint<D>(next.getTime()-size,first.getValue()));
			} else {
				gap = next.getTime()-second.getTime();
			}
			next = null;
		}			
	}
	
	public SignalPoint<D> getValue() {
		if (elements.isEmpty()) {
			throw new IllegalStateException();
		}
		SignalPoint<D> first = elements.getFirst();
		return new SignalPoint<D>( first.getTime()-this.start, first.getValue());
	}
	
	public void addValue( double time , D value ) {
		addValue( new SignalPoint<D>(time,value));
	}

	public void addValue(SignalPoint<D> point) {
		if (next != null) {
			throw new IllegalArgumentException();
		}
		if (elements.isEmpty()) {
			elements.add(point);
		} else {
			if (gap == size) {
				next = point;
			} else {
				SignalPoint<D> first = elements.getFirst();
				double time = point.getTime();
				if (time<=first.getTime()) {
					throw new IllegalArgumentException();
				}
				double newGap = time-first.getTime();
				if (newGap <= size) {
					elements.addLast(point);
					gap = newGap;
				} else {
					elements.addLast(new SignalPoint<D>(first.getTime()+size, point.getValue()));
					next = point;
					gap = size;
				}
			}
		}
	}
}
