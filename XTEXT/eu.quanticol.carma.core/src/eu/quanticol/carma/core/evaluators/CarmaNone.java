/**
 * 
 */
package eu.quanticol.carma.core.evaluators;

/**
 * @author loreti
 *
 */
public class CarmaNone implements CarmaValue {
	
	public static CarmaNone NONE = new CarmaNone();
	
	private CarmaNone() {		
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
		return true;
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
		throw new IllegalStateException();
	}

	@Override
	public CarmaValue getFieldValue(String name) {
		throw new IllegalStateException();
	}

	@Override
	public boolean isTrue() {
		return false;
	}

	@Override
	public boolean isFalse() {
		return false;
	}

	@Override
	public CarmaValue and(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue or(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue not() {
		return this;
	}

	@Override
	public CarmaValue plus(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue minus(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue mul(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue div(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue lessThan(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue lessOrEqualThan(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue equalTo(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue notEqualTo(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue greaterThan(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue greaterOrEqualThan(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue abs() {
		return this;
	}

	@Override
	public CarmaValue cos() {
		return this;
	}

	@Override
	public CarmaValue acos() {
		return this;
	}

	@Override
	public CarmaValue asin() {
		return this;
	}

	@Override
	public CarmaValue atan() {
		return this;
	}

	@Override
	public CarmaValue atan2(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue cbrt() {
		return this;
	}

	@Override
	public CarmaValue ceil() {
		return this;
	}

	@Override
	public CarmaValue exp() {
		return this;
	}

	@Override
	public CarmaValue floor() {
		return this;
	}

	@Override
	public CarmaValue log() {
		return this;
	}

	@Override
	public CarmaValue log10() {
		return this;
	}

	@Override
	public CarmaValue max(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue min(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue pow(CarmaValue v) {
		return this;
	}

	@Override
	public CarmaValue sin() {
		return this;
	}

	@Override
	public CarmaValue sqrt() {
		return this;
	}

	@Override
	public CarmaValue tan() {
		return this;
	}

}
