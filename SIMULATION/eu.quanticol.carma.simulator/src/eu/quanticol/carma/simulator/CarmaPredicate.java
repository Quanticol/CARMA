/**
 * 
 */
package eu.quanticol.carma.simulator;

/**
 * @author loreti
 *
 */
public interface CarmaPredicate {

	public boolean satisfy( CarmaStore store );

	public static final CarmaPredicate TRUE = new  CarmaPredicate() {

		@Override
		public boolean satisfy(CarmaStore store) {
			return true;
		}
		
	};
	
	public static final CarmaPredicate FALSE = new  CarmaPredicate() {

		@Override
		public boolean satisfy(CarmaStore store) {
			return false;
		}
		
	};
	
	
	public static class HasValue<S> implements CarmaPredicate {

		private String attribute;
		private Class<S> clazz;
		private S value;

		public HasValue( String attribute , Class<S> clazz , S value ) {
			this.attribute = attribute;
			this.clazz = clazz;
			this.value = value;
		}
		
		@Override
		public boolean satisfy(CarmaStore store) {
			return value.equals(store.get(attribute, clazz));
		}
		
	}
	
	public static class Conjunction implements CarmaPredicate {

		private CarmaPredicate[] predicates;

		public Conjunction( CarmaPredicate ... predicates ) {
			this.predicates = predicates;
		}
		
		@Override
		public boolean satisfy(CarmaStore store) {
			for (CarmaPredicate carmaPredicate : predicates) {
				if (!carmaPredicate.satisfy(store)) {
					return false;
				}
			}
			return true;
		}
		
	}
}
