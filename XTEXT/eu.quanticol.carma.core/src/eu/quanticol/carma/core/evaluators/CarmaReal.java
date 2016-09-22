/**
 * 
 */
package eu.quanticol.carma.core.evaluators;

/**
 * @author loreti
 *
 */
public class CarmaReal implements CarmaValue {

	@Override
	public int hashCode() {
		return (int) value;
	}

	@Override
	public boolean equals(Object obj) {
		if ((obj != null)&&(obj instanceof CarmaReal)) {
			return value == ((CarmaReal) obj).value;
		}
		return false;
	}

	@Override
	public String toString() {
		return value+"";
	}

	private double value;
	
	public CarmaReal( double value ) {
		this.value = value;
	}

	@Override
	public boolean isBoolean() {
		return false;
	}

	@Override
	public boolean isInteger() {
		return false;
	}

	@Override
	public boolean isReal() {
		return true;
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
		throw new IllegalStateException();
	}

	@Override
	public double getRealValue() {
		return value;
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
		if (v.isReal()) {
			return new CarmaReal(this.value+v.getRealValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue minus(CarmaValue v) {
		if (v.isReal()) {
			return new CarmaReal(this.value-v.getRealValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue mul(CarmaValue v) {
		if (v.isReal()) {
			return new CarmaReal(this.value*v.getRealValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue div(CarmaValue v) {
		if (v.isReal()) {
			return new CarmaReal(this.value/v.getRealValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue lessThan(CarmaValue v) {
		if (v.isReal()) {
			return new CarmaBoolean(this.value<v.getRealValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue lessOrEqualThan(CarmaValue v) {
		if (v.isReal()) {
			return new CarmaBoolean(this.value<=v.getRealValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue equalTo(CarmaValue v) {
		if (v.isReal()) {
			return new CarmaBoolean(this.value==v.getRealValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue notEqualTo(CarmaValue v) {
		if (v.isReal()) {
			return new CarmaBoolean(this.value!=v.getRealValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue greaterThan(CarmaValue v) {
		if (v.isReal()) {
			return new CarmaBoolean(this.value>v.getRealValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue greaterOrEqualThan(CarmaValue v) {
		if (v.isReal()) {
			return new CarmaBoolean(this.value>=v.getRealValue());
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue abs() {
		return new CarmaReal(Math.abs(value));
	}

	@Override
	public CarmaValue cos() {
		return new CarmaReal(Math.cos(value));
	}

	@Override
	public CarmaValue acos() {
		return new CarmaReal(Math.acos(value));
	}

	@Override
	public CarmaValue asin() {
		return new CarmaReal(Math.asin(value));
	}

	@Override
	public CarmaValue atan() {
		return new CarmaReal(Math.atan(value));
	}

	@Override
	public CarmaValue atan2(CarmaValue v) {
		if (v.isReal()) {
			return new CarmaReal(Math.atan2(value,v.getRealValue()));	
		} 
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue cbrt() {
		return new CarmaReal(Math.cbrt(value));
	}

	@Override
	public CarmaValue ceil() {
		return new CarmaReal(Math.ceil(value));
	}

	@Override
	public CarmaValue exp() {
		return new CarmaReal(Math.exp(value));
	}

	@Override
	public CarmaValue floor() {
		return new CarmaReal(Math.floor(value));
	}

	@Override
	public CarmaValue log() {
		return new CarmaReal(Math.log(value));
	}

	@Override
	public CarmaValue log10() {
		return new CarmaReal(Math.log10(value));
	}

	@Override
	public CarmaValue max(CarmaValue v) {
		if (v.isReal()) {
			return (this.value>v.getRealValue()?this:v);
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue min(CarmaValue v) {
		if (v.isReal()) {
			return (this.value<v.getRealValue()?this:v);
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue pow(CarmaValue v) {
		if (v.isReal()) {
			return new CarmaReal(Math.pow(value,v.getRealValue()));
		}
		return CarmaNone.NONE;
	}

	@Override
	public CarmaValue sin() {
		return new CarmaReal(Math.sin(value));
	}

	@Override
	public CarmaValue sqrt() {
		return new CarmaReal(Math.sqrt(value));
	}

	@Override
	public CarmaValue tan() {
		return new CarmaReal(Math.tan(value));
	}
	
}
