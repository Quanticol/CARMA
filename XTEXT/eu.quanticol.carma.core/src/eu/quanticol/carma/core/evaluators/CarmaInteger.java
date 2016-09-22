/**
 * 
 */
package eu.quanticol.carma.core.evaluators;

/**
 * @author loreti
 *
 */
public class CarmaInteger implements CarmaValue {

	@Override
	public int hashCode() {
		return value;
	}

	@Override
	public boolean equals(Object obj) {
		if ((obj != null)&&(obj instanceof CarmaInteger)) {
			return value == ((CarmaInteger) obj).value;
		}
		return false;
	}

	@Override
	public String toString() {
		return value+"";
	}

	private int value;
	
	public CarmaInteger( int value ) {
		this.value = value;
	}

	@Override
	public boolean isBoolean() {
		return false;
	}

	@Override
	public boolean isInteger() {
		return true;
	}

	@Override
	public boolean isReal() {
		return false;
	}

	@Override
	public boolean isRecord() {
		return false;
	}

	@Override
	public boolean isEnum() {
		return false;
	}

	@Override
	public boolean isNone() {
		return false;
	}

	@Override
	public boolean getBooleanValue() {
		throw new IllegalStateException();
	}

	@Override
	public int getIntegerValue() {
		return value;
	}

	@Override
	public double getRealValue() {
		throw new IllegalStateException();
	}

	@Override
	public CarmaValue getFieldValue(String name) {
		throw new IllegalStateException();
	}

	@Override
	public boolean isTrue() {
		throw new IllegalStateException();
	}

	@Override
	public boolean isFalse() {
		throw new IllegalStateException();
	}

	@Override
	public CarmaValue and(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue or(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue not() {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue plus(CarmaValue v) {
		if (v.isInteger()) {
			return new CarmaInteger(this.value+v.getIntegerValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue minus(CarmaValue v) {
		if (v.isInteger()) {
			return new CarmaInteger(this.value-v.getIntegerValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue mul(CarmaValue v) {
		if (v.isInteger()) {
			return new CarmaInteger(this.value*v.getIntegerValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue div(CarmaValue v) {
		if (v.isInteger()) {
			return new CarmaInteger(this.value/v.getIntegerValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue lessThan(CarmaValue v) {
		if (v.isInteger()) {
			return new CarmaBoolean(this.value<v.getIntegerValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue lessOrEqualThan(CarmaValue v) {
		if (v.isInteger()) {
			return new CarmaBoolean(this.value<=v.getIntegerValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue equalTo(CarmaValue v) {
		if (v.isInteger()) {
			return new CarmaBoolean(this.value==v.getIntegerValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue notEqualTo(CarmaValue v) {
		if (v.isInteger()) {
			return new CarmaBoolean(this.value!=v.getIntegerValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue greaterThan(CarmaValue v) {
		if (v.isInteger()) {
			return new CarmaBoolean(this.value>v.getIntegerValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue greaterOrEqualThan(CarmaValue v) {
		if (v.isInteger()) {
			return new CarmaBoolean(this.value>=v.getIntegerValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue abs() {
		return new CarmaInteger(Math.abs(value));
	}

	@Override
	public CarmaValue cos() {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue acos() {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue asin() {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue atan() {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue atan2(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue cbrt() {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue ceil() {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue exp() {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue floor() {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue log() {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue log10() {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue max(CarmaValue v) {
		if (v.isInteger()) {
			return (this.value>v.getIntegerValue()?this:v);
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue min(CarmaValue v) {
		if (v.isInteger()) {
			return (this.value<v.getIntegerValue()?this:v);
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue pow(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue sin() {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue sqrt() {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue tan() {
		return CarmaNone.NONE;
	}
	
}
