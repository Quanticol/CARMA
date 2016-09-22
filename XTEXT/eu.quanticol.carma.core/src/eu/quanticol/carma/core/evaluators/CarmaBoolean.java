/**
 * 
 */
package eu.quanticol.carma.core.evaluators;

/**
 * @author loreti
 *
 */
public class CarmaBoolean implements CarmaValue {
	
	
	@Override
	public int hashCode() {
		return (value?1:0);
	}

	@Override
	public boolean equals(Object obj) {
		if ((obj != null)&&(obj instanceof CarmaBoolean)) {
			return this.value == ((CarmaBoolean) obj).value;
		}
		return false;
	}

	@Override
	public String toString() {
		return value+"";
	}

	private boolean value;
	
	public CarmaBoolean( boolean value ) {
		this.value = value;
	}

	@Override
	public boolean isBoolean() {
		return true;
	}

	@Override
	public boolean isInteger() {
		return false;
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
		return value;
	}

	@Override
	public int getIntegerValue() {
		throw new IllegalStateException();
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
		return value;
	}

	@Override
	public boolean isFalse() {
		return !value;
	}

	@Override
	public CarmaValue and(CarmaValue v) {
		if (v.isBoolean()) {
			return new CarmaBoolean(value&&v.getBooleanValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue or(CarmaValue v) {
		if (v.isBoolean()) {
			return new CarmaBoolean(value||v.getBooleanValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue not() {
		return new CarmaBoolean(!value);
	}

	@Override
	public CarmaValue plus(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue minus(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue mul(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue div(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue lessThan(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue lessOrEqualThan(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue equalTo(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue notEqualTo(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue greaterThan(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue greaterOrEqualThan(CarmaValue v) {
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue abs() {
		return CarmaNone.NONE;
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
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue min(CarmaValue v) {
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
