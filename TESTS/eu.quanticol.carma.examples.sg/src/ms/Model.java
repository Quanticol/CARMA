package ms;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.*;		
import eu.quanticol.carma.simulator.*;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeSet;
import org.cmg.ml.sam.sim.sampling.*;


public class Model extends CarmaModel {
	
	public Model() {
		generateLearnerBehaviour( );
		generateComputingServerBehaviour( );
		setUpMeasures();
	}
	
	
	public static class __RECORD__LSet {
		
		public Double __FIELD__l10;
		public Double __FIELD__l13;
		public Double __FIELD__l14;
		public Double __FIELD__l20;
		public Double __FIELD__l23;
		public Double __FIELD__l24;
		public Double __FIELD__l03;
		public Double __FIELD__l04;
		
		public __RECORD__LSet( Double __FIELD__l10,Double __FIELD__l13,Double __FIELD__l14,Double __FIELD__l20,Double __FIELD__l23,Double __FIELD__l24,Double __FIELD__l03,Double __FIELD__l04) {
			this.__FIELD__l10 = __FIELD__l10;
			this.__FIELD__l13 = __FIELD__l13;
			this.__FIELD__l14 = __FIELD__l14;
			this.__FIELD__l20 = __FIELD__l20;
			this.__FIELD__l23 = __FIELD__l23;
			this.__FIELD__l24 = __FIELD__l24;
			this.__FIELD__l03 = __FIELD__l03;
			this.__FIELD__l04 = __FIELD__l04;
		}
		
		public String toString() {
			return "[ "+"l10="+__FIELD__l10+" , "+"l13="+__FIELD__l13+" , "+"l14="+__FIELD__l14+" , "+"l20="+__FIELD__l20+" , "+"l23="+__FIELD__l23+" , "+"l24="+__FIELD__l24+" , "+"l03="+__FIELD__l03+" , "+"l04="+__FIELD__l04+" ]";
		}
		
		public boolean equals( Object o ) {
			if (o instanceof __RECORD__LSet) {
				__RECORD__LSet other = (__RECORD__LSet) o;
				return 
				this.__FIELD__l10.equals( other.__FIELD__l10 )					&&
				this.__FIELD__l13.equals( other.__FIELD__l13 )					&&
				this.__FIELD__l14.equals( other.__FIELD__l14 )					&&
				this.__FIELD__l20.equals( other.__FIELD__l20 )					&&
				this.__FIELD__l23.equals( other.__FIELD__l23 )					&&
				this.__FIELD__l24.equals( other.__FIELD__l24 )					&&
				this.__FIELD__l03.equals( other.__FIELD__l03 )					&&
				this.__FIELD__l04.equals( other.__FIELD__l04 )					
						;	
			}	
			return false;
		}
		
	}
	public static class __RECORD__MG {
		
		public Double __FIELD__m1;
		public Double __FIELD__m2;
		public Double __FIELD__m3;
		public Double __FIELD__m4;
		
		public __RECORD__MG( Double __FIELD__m1,Double __FIELD__m2,Double __FIELD__m3,Double __FIELD__m4) {
			this.__FIELD__m1 = __FIELD__m1;
			this.__FIELD__m2 = __FIELD__m2;
			this.__FIELD__m3 = __FIELD__m3;
			this.__FIELD__m4 = __FIELD__m4;
		}
		
		public String toString() {
			return "[ "+"m1="+__FIELD__m1+" , "+"m2="+__FIELD__m2+" , "+"m3="+__FIELD__m3+" , "+"m4="+__FIELD__m4+" ]";
		}
		
		public boolean equals( Object o ) {
			if (o instanceof __RECORD__MG) {
				__RECORD__MG other = (__RECORD__MG) o;
				return 
				this.__FIELD__m1.equals( other.__FIELD__m1 )					&&
				this.__FIELD__m2.equals( other.__FIELD__m2 )					&&
				this.__FIELD__m3.equals( other.__FIELD__m3 )					&&
				this.__FIELD__m4.equals( other.__FIELD__m4 )					
						;	
			}	
			return false;
		}
		
	}


	public static Double __FUN__ReturnVal ( 
		__RECORD__LSet __VARIABLE__vector,Integer __VARIABLE__idx
	) {
		{
			//
			Double __VARIABLE__value =0.0;
			//
			//
			if (( __VARIABLE__idx )==( 0 )) {
				//
				__VARIABLE__value =__VARIABLE__vector.__FIELD__l10;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 1 )) {
				//
				__VARIABLE__value =__VARIABLE__vector.__FIELD__l13;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 2 )) {
				//
				__VARIABLE__value =__VARIABLE__vector.__FIELD__l14;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 3 )) {
				//
				__VARIABLE__value =__VARIABLE__vector.__FIELD__l20;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 4 )) {
				//
				__VARIABLE__value =__VARIABLE__vector.__FIELD__l23;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 5 )) {
				//
				__VARIABLE__value =__VARIABLE__vector.__FIELD__l24;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 6 )) {
				//
				__VARIABLE__value =__VARIABLE__vector.__FIELD__l03;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 7 )) {
				//
				__VARIABLE__value =__VARIABLE__vector.__FIELD__l04;
				//
			}
			//
			//
			return __VARIABLE__value;
			//
		}
	}
	public static Integer __FUN__UpdateVal ( 
		__RECORD__LSet __VARIABLE__vector,Integer __VARIABLE__idx,Double __VARIABLE__p
	) {
		{
			//
			if (( __VARIABLE__idx )==( 0 )) {
				//
				__VARIABLE__vector.__FIELD__l10 =__VARIABLE__p;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 1 )) {
				//
				__VARIABLE__vector.__FIELD__l13 =__VARIABLE__p;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 2 )) {
				//
				__VARIABLE__vector.__FIELD__l14 =__VARIABLE__p;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 3 )) {
				//
				__VARIABLE__vector.__FIELD__l20 =__VARIABLE__p;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 4 )) {
				//
				__VARIABLE__vector.__FIELD__l23 =__VARIABLE__p;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 5 )) {
				//
				__VARIABLE__vector.__FIELD__l24 =__VARIABLE__p;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 6 )) {
				//
				__VARIABLE__vector.__FIELD__l03 =__VARIABLE__p;
				//
			}
			//
			//
			if (( __VARIABLE__idx )==( 7 )) {
				//
				__VARIABLE__vector.__FIELD__l04 =__VARIABLE__p;
				//
			}
			//
			//
			return 1;
			//
		}
	}
	public static __RECORD__LSet __FUN__PowerLoss ( 
		__RECORD__LSet __VARIABLE__newpowersvect
	) {
		{
			//
			__RECORD__LSet __VARIABLE__pL =new __RECORD__LSet( Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 )
			 );
			//
			//
			__RECORD__LSet __VARIABLE__resistors =__FUN__GetResistors( 
					);
			//
			//
			__RECORD__LSet __VARIABLE__voltages =__FUN__GetVoltages( 
					);
			//
			//
			__VARIABLE__pL.__FIELD__l10 =( __VARIABLE__resistors.__FIELD__l10 )*( Math.pow( ( __VARIABLE__newpowersvect.__FIELD__l10 )/( __VARIABLE__voltages.__FIELD__l10 ) , 2 ) );
			//
			//
			__VARIABLE__pL.__FIELD__l13 =( __VARIABLE__resistors.__FIELD__l13 )*( Math.pow( ( __VARIABLE__newpowersvect.__FIELD__l13 )/( __VARIABLE__voltages.__FIELD__l13 ) , 2 ) );
			//
			//
			__VARIABLE__pL.__FIELD__l14 =( __VARIABLE__resistors.__FIELD__l14 )*( Math.pow( ( __VARIABLE__newpowersvect.__FIELD__l14 )/( __VARIABLE__voltages.__FIELD__l14 ) , 2 ) );
			//
			//
			__VARIABLE__pL.__FIELD__l20 =( __VARIABLE__resistors.__FIELD__l20 )*( Math.pow( ( __VARIABLE__newpowersvect.__FIELD__l20 )/( __VARIABLE__voltages.__FIELD__l20 ) , 2 ) );
			//
			//
			__VARIABLE__pL.__FIELD__l23 =( __VARIABLE__resistors.__FIELD__l23 )*( Math.pow( ( __VARIABLE__newpowersvect.__FIELD__l23 )/( __VARIABLE__voltages.__FIELD__l23 ) , 2 ) );
			//
			//
			__VARIABLE__pL.__FIELD__l24 =( __VARIABLE__resistors.__FIELD__l24 )*( Math.pow( ( __VARIABLE__newpowersvect.__FIELD__l24 )/( __VARIABLE__voltages.__FIELD__l24 ) , 2 ) );
			//
			//
			__VARIABLE__pL.__FIELD__l03 =( __VARIABLE__resistors.__FIELD__l03 )*( Math.pow( ( __VARIABLE__newpowersvect.__FIELD__l03 )/( __VARIABLE__voltages.__FIELD__l03 ) , 2 ) );
			//
			//
			__VARIABLE__pL.__FIELD__l04 =( __VARIABLE__resistors.__FIELD__l04 )*( Math.pow( ( __VARIABLE__newpowersvect.__FIELD__l04 )/( __VARIABLE__voltages.__FIELD__l04 ) , 2 ) );
			//
			//
			return __VARIABLE__pL;
			//
		}
	}
	public static __RECORD__LSet __FUN__CalcCValues ( 
		__RECORD__LSet __VARIABLE__newpowersvect
	) {
		{
			//
			__RECORD__LSet __VARIABLE__cvalues =new __RECORD__LSet( Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 )
			 );
			//
			//
			__RECORD__LSet __VARIABLE__pL =new __RECORD__LSet( Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 )
			 );
			//
			//
			__VARIABLE__pL =__FUN__PowerLoss( 
						__VARIABLE__newpowersvect
					);
			//
			//
			__RECORD__MG __VARIABLE__mgs =__FUN__GetMicroGrid( 
					);
			//
			//
			__VARIABLE__cvalues.__FIELD__l10 =( __VARIABLE__mgs.__FIELD__m1 )-( ( __VARIABLE__newpowersvect.__FIELD__l13 )+( __VARIABLE__newpowersvect.__FIELD__l14 ) );
			//
			//
			__VARIABLE__cvalues.__FIELD__l20 =( __VARIABLE__mgs.__FIELD__m2 )-( ( __VARIABLE__newpowersvect.__FIELD__l23 )+( __VARIABLE__newpowersvect.__FIELD__l24 ) );
			//
			//
			__VARIABLE__cvalues.__FIELD__l03 =( ( ( ( ( -(1.0) )*( __VARIABLE__mgs.__FIELD__m3 ) )-( ( __VARIABLE__newpowersvect.__FIELD__l13 )+( __VARIABLE__newpowersvect.__FIELD__l23 ) ) )+( __VARIABLE__pL.__FIELD__l03 ) )+( __VARIABLE__pL.__FIELD__l13 ) )+( __VARIABLE__pL.__FIELD__l23 );
			//
			//
			__VARIABLE__cvalues.__FIELD__l04 =( ( ( ( ( -(1.0) )*( __VARIABLE__mgs.__FIELD__m4 ) )-( ( __VARIABLE__newpowersvect.__FIELD__l14 )+( __VARIABLE__newpowersvect.__FIELD__l24 ) ) )+( __VARIABLE__pL.__FIELD__l04 ) )+( __VARIABLE__pL.__FIELD__l14 ) )+( __VARIABLE__pL.__FIELD__l24 );
			//
			//
			__VARIABLE__cvalues.__FIELD__l13 =( ( 0.5 )*( ( ( ( ( ( __VARIABLE__mgs.__FIELD__m1 )-( __VARIABLE__mgs.__FIELD__m3 ) )-( __VARIABLE__newpowersvect.__FIELD__l14 ) )-( __VARIABLE__newpowersvect.__FIELD__l23 ) )-( __VARIABLE__newpowersvect.__FIELD__l10 ) )-( __VARIABLE__newpowersvect.__FIELD__l03 ) ) )+( ( 0.5 )*( ( ( __VARIABLE__pL.__FIELD__l03 )+( __VARIABLE__pL.__FIELD__l13 ) )+( __VARIABLE__pL.__FIELD__l23 ) ) );
			//
			//
			__VARIABLE__cvalues.__FIELD__l23 =( ( 0.5 )*( ( ( ( ( ( __VARIABLE__mgs.__FIELD__m2 )-( __VARIABLE__mgs.__FIELD__m3 ) )-( __VARIABLE__newpowersvect.__FIELD__l24 ) )-( __VARIABLE__newpowersvect.__FIELD__l13 ) )-( __VARIABLE__newpowersvect.__FIELD__l20 ) )-( __VARIABLE__newpowersvect.__FIELD__l03 ) ) )+( ( 0.5 )*( ( ( __VARIABLE__pL.__FIELD__l03 )+( __VARIABLE__pL.__FIELD__l13 ) )+( __VARIABLE__pL.__FIELD__l23 ) ) );
			//
			//
			__VARIABLE__cvalues.__FIELD__l14 =( ( 0.5 )*( ( ( ( ( ( __VARIABLE__mgs.__FIELD__m1 )-( __VARIABLE__mgs.__FIELD__m4 ) )-( __VARIABLE__newpowersvect.__FIELD__l13 ) )-( __VARIABLE__newpowersvect.__FIELD__l24 ) )-( __VARIABLE__newpowersvect.__FIELD__l10 ) )-( __VARIABLE__newpowersvect.__FIELD__l04 ) ) )+( ( 0.5 )*( ( ( __VARIABLE__pL.__FIELD__l04 )+( __VARIABLE__pL.__FIELD__l14 ) )+( __VARIABLE__pL.__FIELD__l24 ) ) );
			//
			//
			__VARIABLE__cvalues.__FIELD__l24 =( ( 0.5 )*( ( ( ( ( ( __VARIABLE__mgs.__FIELD__m2 )-( __VARIABLE__mgs.__FIELD__m4 ) )-( __VARIABLE__newpowersvect.__FIELD__l23 ) )-( __VARIABLE__newpowersvect.__FIELD__l14 ) )-( __VARIABLE__newpowersvect.__FIELD__l20 ) )-( __VARIABLE__newpowersvect.__FIELD__l04 ) ) )+( ( 0.5 )*( ( ( __VARIABLE__pL.__FIELD__l04 )+( __VARIABLE__pL.__FIELD__l14 ) )+( __VARIABLE__pL.__FIELD__l24 ) ) );
			//
			//
			return __VARIABLE__cvalues;
			//
		}
	}
	public static __RECORD__LSet __FUN__GetVoltages ( 
	) {
		{
			//
			return new __RECORD__LSet( Double.valueOf( 20.0 ),
			Double.valueOf( 10.0 ),
			Double.valueOf( 10.0 ),
			Double.valueOf( 20.0 ),
			Double.valueOf( 10.0 ),
			Double.valueOf( 10.0 ),
			Double.valueOf( 20.0 ),
			Double.valueOf( 20.0 )
			 );
			//
		}
	}
	public static Double __FUN__Alpha ( 
	) {
		{
			//
			return 50.0;
			//
		}
	}
	public static __RECORD__LSet __FUN__GetResistors ( 
	) {
		{
			//
			return new __RECORD__LSet( Double.valueOf( 10.0 ),
			Double.valueOf( 1.0 ),
			Double.valueOf( 1.0 ),
			Double.valueOf( 10.0 ),
			Double.valueOf( 1.0 ),
			Double.valueOf( 1.0 ),
			Double.valueOf( 10.0 ),
			Double.valueOf( 10.0 )
			 );
			//
		}
	}
	public static __RECORD__LSet __FUN__GetMaxPowers ( 
	) {
		{
			//
			return new __RECORD__LSet( Double.valueOf( 300.0 ),
			Double.valueOf( 300.0 ),
			Double.valueOf( 300.0 ),
			Double.valueOf( 200.0 ),
			Double.valueOf( 200.0 ),
			Double.valueOf( 200.0 ),
			Double.valueOf( 375.0 ),
			Double.valueOf( 450.0 )
			 );
			//
		}
	}
	public static __RECORD__LSet __FUN__GetDeltaPowers ( 
	) {
		{
			//
			Double __VARIABLE__st =5.0;
			//
			//
			return new __RECORD__LSet( Double.valueOf( ( 300.0 )/( __VARIABLE__st ) ),
			Double.valueOf( ( 300.0 )/( __VARIABLE__st ) ),
			Double.valueOf( ( 300.0 )/( __VARIABLE__st ) ),
			Double.valueOf( ( 200.0 )/( __VARIABLE__st ) ),
			Double.valueOf( ( 200.0 )/( __VARIABLE__st ) ),
			Double.valueOf( ( 200.0 )/( __VARIABLE__st ) ),
			Double.valueOf( ( 375.0 )/( __VARIABLE__st ) ),
			Double.valueOf( ( 450.0 )/( __VARIABLE__st ) )
			 );
			//
		}
	}
	public static __RECORD__MG __FUN__GetMicroGrid ( 
	) {
		{
			//
			return new __RECORD__MG( Double.valueOf( 300.0 ),
			Double.valueOf( 200.0 ),
			Double.valueOf( -(250.0) ),
			Double.valueOf( -(300.0) )
			 );
			//
		}
	}
	public static Double __FUN__StepFunc ( 
		Double __VARIABLE__val
	) {
		{
			//
			Double __VARIABLE__sf =0.0;
			//
			//
			if (( __VARIABLE__val )>=( 0.0 )) {
				//
				__VARIABLE__sf =1.0;
				//
			}
			//
			//
			return __VARIABLE__sf;
			//
		}
	}
	public static __RECORD__LSet __FUN__CalcPayoffs ( 
		__RECORD__LSet __VARIABLE__newpowersvect
	) {
		{
			//
			Double __VARIABLE__tol =10.0;
			//
			//
			Double __VARIABLE__alpha =__FUN__Alpha( 
					);
			//
			//
			__RECORD__LSet __VARIABLE__cvalues =new __RECORD__LSet( Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 ),
			Double.valueOf( 0.0 )
			 );
			//
			//
			__RECORD__LSet __VARIABLE__payoffs =new __RECORD__LSet( Double.valueOf( ( -(1.0) )*( __VARIABLE__alpha ) ),
			Double.valueOf( ( -(1.0) )*( __VARIABLE__alpha ) ),
			Double.valueOf( ( -(1.0) )*( __VARIABLE__alpha ) ),
			Double.valueOf( ( -(1.0) )*( __VARIABLE__alpha ) ),
			Double.valueOf( ( -(1.0) )*( __VARIABLE__alpha ) ),
			Double.valueOf( ( -(1.0) )*( __VARIABLE__alpha ) ),
			Double.valueOf( ( -(1.0) )*( __VARIABLE__alpha ) ),
			Double.valueOf( ( -(1.0) )*( __VARIABLE__alpha ) )
			 );
			//
			//
			__VARIABLE__cvalues =__FUN__CalcCValues( 
						__VARIABLE__newpowersvect
					);
			//
			//
			__RECORD__LSet __VARIABLE__maxpowers =__FUN__GetMaxPowers( 
					);
			//
			//
			if (( ( __VARIABLE__cvalues.__FIELD__l10 )>=( 0 ) )&&( ( __VARIABLE__cvalues.__FIELD__l10 )<=( __VARIABLE__maxpowers.__FIELD__l10 ) )) {
				//
				__VARIABLE__payoffs.__FIELD__l10 =( ( -(1.0) )*( Math.pow( Math.abs( ( __VARIABLE__newpowersvect.__FIELD__l10 )-( __VARIABLE__cvalues.__FIELD__l10 ) ) , 0.5 ) ) )+( ( __VARIABLE__alpha )*( (double) ( __FUN__StepFunc( 
							Double.valueOf( ( __VARIABLE__newpowersvect.__FIELD__l10 )-( __VARIABLE__cvalues.__FIELD__l10 ) )
						) ) ) );
				//
				//
				if (( ( __VARIABLE__payoffs.__FIELD__l10 )>=( ( __VARIABLE__alpha )-( __VARIABLE__tol ) ) )||( ( ( __VARIABLE__payoffs.__FIELD__l10 )<( 0 ) )&&( ( __VARIABLE__payoffs.__FIELD__l10 )>=( ( -(1) )*( __VARIABLE__tol ) ) ) )) {
					//
					__VARIABLE__payoffs.__FIELD__l10 =__VARIABLE__alpha;
					//
				}
				//
			}
			else {
				//
				__VARIABLE__payoffs.__FIELD__l10 =( -(1.0) )*( __VARIABLE__alpha );
				//
			}
			//
			//
			if (( ( __VARIABLE__cvalues.__FIELD__l13 )>=( 0 ) )&&( ( __VARIABLE__cvalues.__FIELD__l13 )<=( __VARIABLE__maxpowers.__FIELD__l13 ) )) {
				//
				__VARIABLE__payoffs.__FIELD__l13 =( ( -(1.0) )*( Math.pow( Math.abs( ( __VARIABLE__newpowersvect.__FIELD__l13 )-( __VARIABLE__cvalues.__FIELD__l13 ) ) , 0.5 ) ) )+( ( __VARIABLE__alpha )*( (double) ( __FUN__StepFunc( 
							Double.valueOf( ( __VARIABLE__newpowersvect.__FIELD__l13 )-( __VARIABLE__cvalues.__FIELD__l13 ) )
						) ) ) );
				//
				//
				if (( ( __VARIABLE__payoffs.__FIELD__l13 )>=( ( __VARIABLE__alpha )-( __VARIABLE__tol ) ) )||( ( ( __VARIABLE__payoffs.__FIELD__l13 )<( 0 ) )&&( ( __VARIABLE__payoffs.__FIELD__l13 )>=( ( -(1) )*( __VARIABLE__tol ) ) ) )) {
					//
					__VARIABLE__payoffs.__FIELD__l13 =__VARIABLE__alpha;
					//
				}
				//
			}
			else {
				//
				__VARIABLE__payoffs.__FIELD__l13 =( -(1.0) )*( __VARIABLE__alpha );
				//
			}
			//
			//
			if (( ( __VARIABLE__cvalues.__FIELD__l14 )>=( 0 ) )&&( ( __VARIABLE__cvalues.__FIELD__l14 )<=( __VARIABLE__maxpowers.__FIELD__l14 ) )) {
				//
				__VARIABLE__payoffs.__FIELD__l14 =( ( -(1.0) )*( Math.pow( Math.abs( ( __VARIABLE__newpowersvect.__FIELD__l14 )-( __VARIABLE__cvalues.__FIELD__l14 ) ) , 0.5 ) ) )+( ( __VARIABLE__alpha )*( (double) ( __FUN__StepFunc( 
							Double.valueOf( ( __VARIABLE__newpowersvect.__FIELD__l14 )-( __VARIABLE__cvalues.__FIELD__l14 ) )
						) ) ) );
				//
				//
				if (( ( __VARIABLE__payoffs.__FIELD__l14 )>=( ( __VARIABLE__alpha )-( __VARIABLE__tol ) ) )||( ( ( __VARIABLE__payoffs.__FIELD__l14 )<( 0 ) )&&( ( __VARIABLE__payoffs.__FIELD__l14 )>=( ( -(1) )*( __VARIABLE__tol ) ) ) )) {
					//
					__VARIABLE__payoffs.__FIELD__l14 =__VARIABLE__alpha;
					//
				}
				//
			}
			else {
				//
				__VARIABLE__payoffs.__FIELD__l14 =( -(1.0) )*( __VARIABLE__alpha );
				//
			}
			//
			//
			if (( ( __VARIABLE__cvalues.__FIELD__l20 )>=( 0 ) )&&( ( __VARIABLE__cvalues.__FIELD__l20 )<=( __VARIABLE__maxpowers.__FIELD__l20 ) )) {
				//
				__VARIABLE__payoffs.__FIELD__l20 =( ( -(1.0) )*( Math.pow( Math.abs( ( __VARIABLE__newpowersvect.__FIELD__l20 )-( __VARIABLE__cvalues.__FIELD__l20 ) ) , 0.5 ) ) )+( ( __VARIABLE__alpha )*( (double) ( __FUN__StepFunc( 
							Double.valueOf( ( __VARIABLE__newpowersvect.__FIELD__l20 )-( __VARIABLE__cvalues.__FIELD__l20 ) )
						) ) ) );
				//
				//
				if (( ( __VARIABLE__payoffs.__FIELD__l20 )>=( ( __VARIABLE__alpha )-( __VARIABLE__tol ) ) )||( ( ( __VARIABLE__payoffs.__FIELD__l20 )<( 0 ) )&&( ( __VARIABLE__payoffs.__FIELD__l20 )>=( ( -(1) )*( __VARIABLE__tol ) ) ) )) {
					//
					__VARIABLE__payoffs.__FIELD__l20 =__VARIABLE__alpha;
					//
				}
				//
			}
			else {
				//
				__VARIABLE__payoffs.__FIELD__l20 =( -(1.0) )*( __VARIABLE__alpha );
				//
			}
			//
			//
			if (( ( __VARIABLE__cvalues.__FIELD__l23 )>=( 0 ) )&&( ( __VARIABLE__cvalues.__FIELD__l23 )<=( __VARIABLE__maxpowers.__FIELD__l23 ) )) {
				//
				__VARIABLE__payoffs.__FIELD__l23 =( ( -(1.0) )*( Math.pow( Math.abs( ( __VARIABLE__newpowersvect.__FIELD__l23 )-( __VARIABLE__cvalues.__FIELD__l23 ) ) , 0.5 ) ) )+( ( __VARIABLE__alpha )*( (double) ( __FUN__StepFunc( 
							Double.valueOf( ( __VARIABLE__newpowersvect.__FIELD__l23 )-( __VARIABLE__cvalues.__FIELD__l23 ) )
						) ) ) );
				//
				//
				if (( ( __VARIABLE__payoffs.__FIELD__l23 )>=( ( __VARIABLE__alpha )-( __VARIABLE__tol ) ) )||( ( ( __VARIABLE__payoffs.__FIELD__l23 )<( 0 ) )&&( ( __VARIABLE__payoffs.__FIELD__l23 )>=( ( -(1) )*( __VARIABLE__tol ) ) ) )) {
					//
					__VARIABLE__payoffs.__FIELD__l23 =__VARIABLE__alpha;
					//
				}
				//
			}
			else {
				//
				__VARIABLE__payoffs.__FIELD__l23 =( -(1.0) )*( __VARIABLE__alpha );
				//
			}
			//
			//
			if (( ( __VARIABLE__cvalues.__FIELD__l24 )>=( 0 ) )&&( ( __VARIABLE__cvalues.__FIELD__l24 )<=( __VARIABLE__maxpowers.__FIELD__l24 ) )) {
				//
				__VARIABLE__payoffs.__FIELD__l24 =( ( -(1.0) )*( Math.pow( Math.abs( ( __VARIABLE__newpowersvect.__FIELD__l24 )-( __VARIABLE__cvalues.__FIELD__l24 ) ) , 0.5 ) ) )+( ( __VARIABLE__alpha )*( (double) ( __FUN__StepFunc( 
							Double.valueOf( ( __VARIABLE__newpowersvect.__FIELD__l24 )-( __VARIABLE__cvalues.__FIELD__l24 ) )
						) ) ) );
				//
				//
				if (( ( __VARIABLE__payoffs.__FIELD__l24 )>=( ( __VARIABLE__alpha )-( __VARIABLE__tol ) ) )||( ( ( __VARIABLE__payoffs.__FIELD__l24 )<( 0 ) )&&( ( __VARIABLE__payoffs.__FIELD__l24 )>=( ( -(1) )*( __VARIABLE__tol ) ) ) )) {
					//
					__VARIABLE__payoffs.__FIELD__l24 =__VARIABLE__alpha;
					//
				}
				//
			}
			else {
				//
				__VARIABLE__payoffs.__FIELD__l24 =( -(1.0) )*( __VARIABLE__alpha );
				//
			}
			//
			//
			if (( ( __VARIABLE__cvalues.__FIELD__l03 )>=( 0 ) )&&( ( __VARIABLE__cvalues.__FIELD__l03 )<=( __VARIABLE__maxpowers.__FIELD__l03 ) )) {
				//
				__VARIABLE__payoffs.__FIELD__l03 =( ( -(1.0) )*( Math.pow( Math.abs( ( __VARIABLE__newpowersvect.__FIELD__l03 )-( __VARIABLE__cvalues.__FIELD__l03 ) ) , 0.5 ) ) )+( ( __VARIABLE__alpha )*( (double) ( __FUN__StepFunc( 
							Double.valueOf( ( __VARIABLE__newpowersvect.__FIELD__l03 )-( __VARIABLE__cvalues.__FIELD__l03 ) )
						) ) ) );
				//
				//
				if (( ( __VARIABLE__payoffs.__FIELD__l03 )>=( ( __VARIABLE__alpha )-( __VARIABLE__tol ) ) )||( ( ( __VARIABLE__payoffs.__FIELD__l03 )<( 0 ) )&&( ( __VARIABLE__payoffs.__FIELD__l03 )>=( ( -(1) )*( __VARIABLE__tol ) ) ) )) {
					//
					__VARIABLE__payoffs.__FIELD__l03 =__VARIABLE__alpha;
					//
				}
				//
			}
			else {
				//
				__VARIABLE__payoffs.__FIELD__l03 =( -(1.0) )*( __VARIABLE__alpha );
				//
			}
			//
			//
			if (( ( __VARIABLE__cvalues.__FIELD__l04 )>=( 0 ) )&&( ( __VARIABLE__cvalues.__FIELD__l04 )<=( __VARIABLE__maxpowers.__FIELD__l04 ) )) {
				//
				__VARIABLE__payoffs.__FIELD__l04 =( ( -(1.0) )*( Math.pow( Math.abs( ( __VARIABLE__newpowersvect.__FIELD__l04 )-( __VARIABLE__cvalues.__FIELD__l04 ) ) , 0.5 ) ) )+( ( __VARIABLE__alpha )*( (double) ( __FUN__StepFunc( 
							Double.valueOf( ( __VARIABLE__newpowersvect.__FIELD__l04 )-( __VARIABLE__cvalues.__FIELD__l04 ) )
						) ) ) );
				//
				//
				if (( ( __VARIABLE__payoffs.__FIELD__l04 )>=( ( __VARIABLE__alpha )-( __VARIABLE__tol ) ) )||( ( ( __VARIABLE__payoffs.__FIELD__l04 )<( 0 ) )&&( ( __VARIABLE__payoffs.__FIELD__l04 )>=( ( -(1) )*( __VARIABLE__tol ) ) ) )) {
					//
					__VARIABLE__payoffs.__FIELD__l04 =__VARIABLE__alpha;
					//
				}
				//
			}
			else {
				//
				__VARIABLE__payoffs.__FIELD__l04 =( -(1.0) )*( __VARIABLE__alpha );
				//
			}
			//
			//
			return __VARIABLE__payoffs;
			//
		}
	}
	public static Integer __FUN__Greater ( 
		__RECORD__LSet __VARIABLE__vect1,__RECORD__LSet __VARIABLE__vect2
	) {
		{
			//
			Integer __VARIABLE__gt =0;
			//
			//
			if (( ( ( ( ( ( ( ( __VARIABLE__vect1.__FIELD__l10 )>=( __VARIABLE__vect2.__FIELD__l10 ) )&&( ( __VARIABLE__vect1.__FIELD__l13 )>=( __VARIABLE__vect2.__FIELD__l13 ) ) )&&( ( __VARIABLE__vect1.__FIELD__l14 )>=( __VARIABLE__vect2.__FIELD__l14 ) ) )&&( ( __VARIABLE__vect1.__FIELD__l20 )>=( __VARIABLE__vect2.__FIELD__l20 ) ) )&&( ( __VARIABLE__vect1.__FIELD__l23 )>=( __VARIABLE__vect2.__FIELD__l23 ) ) )&&( ( __VARIABLE__vect1.__FIELD__l24 )>=( __VARIABLE__vect2.__FIELD__l24 ) ) )&&( ( __VARIABLE__vect1.__FIELD__l03 )>=( __VARIABLE__vect2.__FIELD__l03 ) ) )&&( ( __VARIABLE__vect1.__FIELD__l04 )>=( __VARIABLE__vect2.__FIELD__l04 ) )) {
				//
				__VARIABLE__gt =1;
				//
			}
			//
			//
			return __VARIABLE__gt;
			//
		}
	}
	public static Double __FUN__DecreaseP ( 
		Integer __VARIABLE__index,Double __VARIABLE__p
	) {
		{
			//
			Double __VARIABLE__deltap =__FUN__ReturnVal( 
						__FUN__GetDeltaPowers( 
								),
						Integer.valueOf( __VARIABLE__index )
					);
			//
			//
			Double __VARIABLE__newp =( ( ( __VARIABLE__p )-( 1.0 ) )-( __VARIABLE__deltap ) )+( ( __VARIABLE__deltap )*( RandomGeneratorRegistry.rnd() ) );
			//
			//
			if (( __VARIABLE__newp )<( 0 )) {
				//
				__VARIABLE__newp =0.0;
				//
			}
			//
			//
			return __VARIABLE__newp;
			//
		}
	}
	public static Double __FUN__IncreaseP ( 
		Integer __VARIABLE__index,Double __VARIABLE__p
	) {
		{
			//
			Double __VARIABLE__deltap =__FUN__ReturnVal( 
						__FUN__GetDeltaPowers( 
								),
						Integer.valueOf( __VARIABLE__index )
					);
			//
			//
			Double __VARIABLE__mp =__FUN__ReturnVal( 
						__FUN__GetMaxPowers( 
								),
						Integer.valueOf( __VARIABLE__index )
					);
			//
			//
			Double __VARIABLE__newp =( ( __VARIABLE__p )+( 1.0 ) )+( ( __VARIABLE__deltap )*( RandomGeneratorRegistry.rnd() ) );
			//
			//
			if (( __VARIABLE__newp )>( __VARIABLE__mp )) {
				//
				__VARIABLE__newp =__VARIABLE__mp;
				//
			}
			//
			//
			return __VARIABLE__newp;
			//
		}
	}
	
	
	/* START COMPONENT: Learner         */
	
	/* DEFINITIONS OF PROCESSES */
	public final CarmaProcessAutomaton _COMP_Learner = new CarmaProcessAutomaton("Learner");
	
	public final CarmaProcessAutomaton.State __STATE___Learner_RPP = _COMP_Learner.newState("RPP");		
	public final CarmaProcessAutomaton.State __STATE___Learner_GR = _COMP_Learner.newState("GR");		
	public final CarmaProcessAutomaton.State __STATE___Learner_DU = _COMP_Learner.newState("DU");		
	public final CarmaProcessAutomaton.State __STATE___Learner_UP = _COMP_Learner.newState("UP");		
	public final CarmaProcessAutomaton.State __STATE___Learner_SP = _COMP_Learner.newState("SP");		
	
	private void generateLearnerBehaviour( ) {
		
		
		{
			CarmaAction action = new CarmaInput( 
				__ACT__updatepower , true  		
			) {
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys, final Object value,double now) {
					
					LinkedList<Object> message = (LinkedList<Object>) value;
					final __RECORD__LSet __VARIABLE__powers = (__RECORD__LSet) message.get(0);
					final __RECORD__LSet __VARIABLE__payoffs_attrib = (__RECORD__LSet) message.get(1);
					return new CarmaStoreUpdate() {
						
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
							final Integer __MY__i = store.get( "i" , Integer.class );
							store.set( "pf", __FUN__ReturnVal( 
										__VARIABLE__payoffs_attrib,
										Integer.valueOf( __MY__i )
									) );
							store.set( "p", __FUN__ReturnVal( 
										__VARIABLE__powers,
										Integer.valueOf( __MY__i )
									) );
						}
					};
								
				}	
				
				@Override
				protected CarmaPredicate getPredicate(CarmaSystem sys, CarmaStore myStore, Object value) {
					return CarmaPredicate.TRUE;
					
				}
							
			};		
			
			_COMP_Learner.addTransition( __STATE___Learner_RPP , action , __STATE___Learner_GR );			
		}
		{
			CarmaAction action = new CarmaOutput(
				__ACT__generaterandom , true  		
			) {
				
				@Override
				protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
					LinkedList<Object> toReturn = new LinkedList<Object>();
					return toReturn;
				}
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
					return new CarmaStoreUpdate() {
						
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
							store.set( "rng", RandomGeneratorRegistry.rnd() );
						}
					};
				}
				
				@Override
				protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
					return CarmaPredicate.FALSE;
					
				}
			};		
			
			_COMP_Learner.addTransition( __STATE___Learner_GR , action , __STATE___Learner_DU );			
		}
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Double __ATTR__rng = store.get( "rng" , Double.class );
					final Double __ATTR__lambda = store.get( "lambda" , Double.class );
					return ( __ATTR__rng )<( __ATTR__lambda );
				}
					
			};
			{
				CarmaAction action = new CarmaOutput(
					__ACT__samepower , true  		
				) {
					
					@Override
					protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
						LinkedList<Object> toReturn = new LinkedList<Object>();
						return toReturn;
					}
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
						return new CarmaStoreUpdate() {					
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
							}
						};
					}
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
						return CarmaPredicate.FALSE;
						
					}
				};		
				
				_COMP_Learner.addTransition( __STATE___Learner_DU , new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , action , __STATE___Learner_SP );			
			}
		}
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Double __ATTR__rng = store.get( "rng" , Double.class );
					final Double __ATTR__lambda = store.get( "lambda" , Double.class );
					return ( __ATTR__rng )>=( __ATTR__lambda );
				}
					
			};
			{
				CarmaAction action = new CarmaOutput(
					__ACT__changeupdate , true  		
				) {
					
					@Override
					protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
						LinkedList<Object> toReturn = new LinkedList<Object>();
						return toReturn;
					}
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
						return new CarmaStoreUpdate() {					
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
							}
						};
					}
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
						return CarmaPredicate.FALSE;
						
					}
				};		
				
				_COMP_Learner.addTransition( __STATE___Learner_DU , new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , action , __STATE___Learner_UP );			
			}
		}
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Double __ATTR__pf = store.get( "pf" , Double.class );
					return ( ( __ATTR__pf )>=( 0 ) )&&( ( __ATTR__pf )<( __FUN__Alpha( 
							) ) );
				}
					
			};
			{
				CarmaAction action = new CarmaOutput(
					__ACT__decreasepower , true  		
				) {
					
					@Override
					protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
						LinkedList<Object> toReturn = new LinkedList<Object>();
						return toReturn;
					}
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
						return new CarmaStoreUpdate() {
							
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
								final Integer __MY__i = store.get( "i" , Integer.class );
								final Double __MY__p = store.get( "p" , Double.class );
								store.set( "p", __FUN__DecreaseP( 
											Integer.valueOf( __MY__i ),
											Double.valueOf( __MY__p )
										) );
							}
						};
					}
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
						return CarmaPredicate.FALSE;
						
					}
				};		
				
				_COMP_Learner.addTransition( __STATE___Learner_UP , new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , action , __STATE___Learner_SP );			
			}
		}
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Double __ATTR__pf = store.get( "pf" , Double.class );
					return ( ( __ATTR__pf )<( 0 ) )&&( ( __ATTR__pf )>( ( -(1) )*( __FUN__Alpha( 
							) ) ) );
				}
					
			};
			{
				CarmaAction action = new CarmaOutput(
					__ACT__increasepower , true  		
				) {
					
					@Override
					protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
						LinkedList<Object> toReturn = new LinkedList<Object>();
						return toReturn;
					}
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
						return new CarmaStoreUpdate() {
							
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
								final Integer __MY__i = store.get( "i" , Integer.class );
								final Double __MY__p = store.get( "p" , Double.class );
								store.set( "p", __FUN__IncreaseP( 
											Integer.valueOf( __MY__i ),
											Double.valueOf( __MY__p )
										) );
							}
						};
					}
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
						return CarmaPredicate.FALSE;
						
					}
				};		
				
				_COMP_Learner.addTransition( __STATE___Learner_UP , new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , action , __STATE___Learner_SP );			
			}
		}
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Double __ATTR__pf = store.get( "pf" , Double.class );
					return ( ( __ATTR__pf )==( __FUN__Alpha( 
							) ) )||( ( __ATTR__pf )==( ( -(1) )*( __FUN__Alpha( 
							) ) ) );
				}
					
			};
			{
				CarmaAction action = new CarmaOutput(
					__ACT__samepower , true  		
				) {
					
					@Override
					protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
						LinkedList<Object> toReturn = new LinkedList<Object>();
						return toReturn;
					}
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
						return new CarmaStoreUpdate() {					
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
							}
						};
					}
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
						return CarmaPredicate.FALSE;
						
					}
				};		
				
				_COMP_Learner.addTransition( __STATE___Learner_UP , new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , action , __STATE___Learner_SP );			
			}
		}
		{
			CarmaAction action = new CarmaOutput(
				__ACT__sendpower , false  		
			) {
				
				@Override
				protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
					LinkedList<Object> toReturn = new LinkedList<Object>();
					final Integer __MY__i = store.get( "i" , Integer.class );
					final Double __MY__p = store.get( "p" , Double.class );
					toReturn.add( __MY__i );
					toReturn.add( __MY__p );
					return toReturn;
				}
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
					return new CarmaStoreUpdate() {					
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
						}
					};
				}
				
				@Override
				protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
					return CarmaPredicate.TRUE;
					
				}
			};		
			
			_COMP_Learner.addTransition( __STATE___Learner_SP , action , __STATE___Learner_RPP );			
		}
		
	}
	
	public CarmaComponent createComponentLearner( 
		Integer __VARIABLE__index, Double __VARIABLE__power, Double __VARIABLE__rnd  
	) {
		CarmaComponent c = new CarmaComponent();
		c.set( "i" ,  __VARIABLE__index );
		c.set( "p" ,  __VARIABLE__power );
		c.set( "lambda" ,  __VARIABLE__rnd );
		c.set( "pf" ,  ( -(1.0) )*( __FUN__Alpha( 
				) ) );
		c.set( "rng" ,  0.0 );
		c.addAgent( new CarmaSequentialProcess( c , _COMP_Learner , __STATE___Learner_SP ));
		return c;
	}	
	
	/* END COMPONENT: Learner */
		
	
	/* START COMPONENT: ComputingServer         */
	
	/* DEFINITIONS OF PROCESSES */
	public final CarmaProcessAutomaton _COMP_ComputingServer = new CarmaProcessAutomaton("ComputingServer");
	
	public final CarmaProcessAutomaton.State __STATE___ComputingServer_RP = _COMP_ComputingServer.newState("RP");		
	public final CarmaProcessAutomaton.State __STATE___ComputingServer_CP = _COMP_ComputingServer.newState("CP");		
	public final CarmaProcessAutomaton.State __STATE___ComputingServer_NewPA = _COMP_ComputingServer.newState("NewPA");		
	public final CarmaProcessAutomaton.State __STATE___ComputingServer_FS = _COMP_ComputingServer.newState("FS");		
	public final CarmaProcessAutomaton.State __STATE___ComputingServer_NewPP = _COMP_ComputingServer.newState("NewPP");		
	public final CarmaProcessAutomaton.State __STATE___ComputingServer_DC = _COMP_ComputingServer.newState("DC");		
	
	private void generateComputingServerBehaviour( ) {
		
		
		{
			CarmaAction action = new CarmaInput( 
				__ACT__sendpower , false  		
			) {
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys, final Object value, double now) {
					
					LinkedList<Object> message = (LinkedList<Object>) value;
					final Integer __VARIABLE__i = (Integer) message.get(0);
					final Double __VARIABLE__p = (Double) message.get(1);
					return new CarmaStoreUpdate() {
						
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
							final Integer __ATTR__rcvnum = store.get( "rcvnum" , Integer.class );
							final __RECORD__LSet __MY__newpowers = store.get( "newpowers" , __RECORD__LSet.class );
							store.set( "a", __FUN__UpdateVal( 
										__MY__newpowers,
										Integer.valueOf( __VARIABLE__i ),
										Double.valueOf( __VARIABLE__p )
									) );
							store.set( "rcvnum", ( __ATTR__rcvnum )+( 1 ) );
						}
					};
								
				}	
				
				@Override
				protected CarmaPredicate getPredicate(CarmaSystem sys, CarmaStore myStore, Object value) {
					return CarmaPredicate.TRUE;
					
				}
							
			};		
			
			_COMP_ComputingServer.addTransition( __STATE___ComputingServer_RP , action , __STATE___ComputingServer_CP );			
		}
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Integer __MY__rcvnum = store.get( "rcvnum" , Integer.class );
					return ( __MY__rcvnum )<( 8 );
				}
					
			};
			{
				CarmaAction action = new CarmaOutput(
					__ACT__waitalllearners , true  		
				) {
					
					@Override
					protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
						LinkedList<Object> toReturn = new LinkedList<Object>();
						return toReturn;
					}
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
						return new CarmaStoreUpdate() {					
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
							}
						};
					}
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
						return CarmaPredicate.FALSE;
						
					}
				};		
				
				_COMP_ComputingServer.addTransition( __STATE___ComputingServer_CP , new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , action , __STATE___ComputingServer_RP );			
			}
		}
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Integer __MY__rcvnum = store.get( "rcvnum" , Integer.class );
					return ( __MY__rcvnum )==( 8 );
				}
					
			};
			{
				CarmaAction action = new CarmaOutput(
					__ACT__calcpayoffs , true  		
				) {
					
					@Override
					protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
						LinkedList<Object> toReturn = new LinkedList<Object>();
						return toReturn;
					}
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
						return new CarmaStoreUpdate() {
							
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
								final __RECORD__LSet __MY__newpowers = store.get( "newpowers" , __RECORD__LSet.class );
								store.set( "newpayoffs", __FUN__CalcPayoffs( 
											__MY__newpowers
										) );
								store.set( "rcvnum", 0 );
							}
						};
					}
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
						return CarmaPredicate.FALSE;
						
					}
				};		
				
				_COMP_ComputingServer.addTransition( __STATE___ComputingServer_CP , new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , action , __STATE___ComputingServer_NewPA );			
			}
		}
		{
			CarmaAction action = new CarmaOutput(
				__ACT__newpayoffAlphas , true  		
			) {
				
				@Override
				protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
					LinkedList<Object> toReturn = new LinkedList<Object>();
					return toReturn;
				}
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
					return new CarmaStoreUpdate() {
						
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
							final __RECORD__LSet __ATTR__alphas = store.get( "alphas" , __RECORD__LSet.class );
							final __RECORD__LSet __MY__newpayoffs = store.get( "newpayoffs" , __RECORD__LSet.class );
							store.set( "a", __FUN__Greater( 
										__MY__newpayoffs,
										__ATTR__alphas
									) );
						}
					};
				}
				
				@Override
				protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
					return CarmaPredicate.FALSE;
					
				}
			};		
			
			_COMP_ComputingServer.addTransition( __STATE___ComputingServer_NewPA , action , __STATE___ComputingServer_FS );			
		}
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Integer __ATTR__a = store.get( "a" , Integer.class );
					return ( __ATTR__a )==( 1 );
				}
					
			};
			{
				CarmaAction action = new CarmaOutput(
					__ACT__stablestate , true  		
				) {
					
					@Override
					protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
						LinkedList<Object> toReturn = new LinkedList<Object>();
						return toReturn;
					}
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
						return new CarmaStoreUpdate() {
							
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
								final __RECORD__LSet __ATTR__newpayoffs = store.get( "newpayoffs" , __RECORD__LSet.class );
								final __RECORD__LSet __ATTR__newpowers = store.get( "newpowers" , __RECORD__LSet.class );
								store.set( "payoffs", __ATTR__newpayoffs );
								store.set( "powers", __ATTR__newpowers );
							}
						};
					}
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
						return CarmaPredicate.FALSE;
						
					}
				};		
				
				_COMP_ComputingServer.addTransition( __STATE___ComputingServer_FS , new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , action , null );			
			}
		}
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Integer __ATTR__a = store.get( "a" , Integer.class );
					return ( __ATTR__a )==( 0 );
				}
					
			};
			{
				CarmaAction action = new CarmaOutput(
					__ACT__continue , true  		
				) {
					
					@Override
					protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
						LinkedList<Object> toReturn = new LinkedList<Object>();
						return toReturn;
					}
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
						return new CarmaStoreUpdate() {					
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
							}
						};
					}
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
						return CarmaPredicate.FALSE;
						
					}
				};		
				
				_COMP_ComputingServer.addTransition( __STATE___ComputingServer_FS , new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , action , __STATE___ComputingServer_NewPP );			
			}
		}
		{
			CarmaAction action = new CarmaOutput(
				__ACT__newpayoffpayoff , true  		
			) {
				
				@Override
				protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
					LinkedList<Object> toReturn = new LinkedList<Object>();
					return toReturn;
				}
				
				@Override
				protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
					return new CarmaStoreUpdate() {
						
						//@Override
						public void update(RandomGenerator r, CarmaStore store) {
							final __RECORD__LSet __ATTR__payoffs = store.get( "payoffs" , __RECORD__LSet.class );
							final __RECORD__LSet __MY__newpayoffs = store.get( "newpayoffs" , __RECORD__LSet.class );
							store.set( "a", __FUN__Greater( 
										__MY__newpayoffs,
										__ATTR__payoffs
									) );
						}
					};
				}
				
				@Override
				protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
					return CarmaPredicate.FALSE;
					
				}
			};		
			
			_COMP_ComputingServer.addTransition( __STATE___ComputingServer_NewPP , action , __STATE___ComputingServer_DC );			
		}
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Integer __ATTR__a = store.get( "a" , Integer.class );
					return ( __ATTR__a )==( 1 );
				}
					
			};
			{
				CarmaAction action = new CarmaOutput(
					__ACT__updatepower , true  		
				) {
					
					@Override
					protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
						LinkedList<Object> toReturn = new LinkedList<Object>();
						final __RECORD__LSet __ATTR__newpowers = store.get( "newpowers" , __RECORD__LSet.class );
						final __RECORD__LSet __ATTR__newpayoffs = store.get( "newpayoffs" , __RECORD__LSet.class );
						toReturn.add( __ATTR__newpowers );
						toReturn.add( __ATTR__newpayoffs );
						return toReturn;
					}
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
						return new CarmaStoreUpdate() {
							
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
								final __RECORD__LSet __ATTR__newpayoffs = store.get( "newpayoffs" , __RECORD__LSet.class );
								final __RECORD__LSet __ATTR__newpowers = store.get( "newpowers" , __RECORD__LSet.class );
								store.set( "payoffs", __ATTR__newpayoffs );
								store.set( "powers", __ATTR__newpowers );
								store.set( "newpayoffs", new __RECORD__LSet( Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 )
								 ) );
								store.set( "newpowers", new __RECORD__LSet( Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 ),
								Double.valueOf( 0.0 )
								 ) );
							}
						};
					}
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
						return CarmaPredicate.TRUE;
						
					}
				};		
				
				_COMP_ComputingServer.addTransition( __STATE___ComputingServer_DC , new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , action , __STATE___ComputingServer_RP );			
			}
		}
		{
			CarmaPredicate _FOO_predicate0 = new CarmaPredicate() {
		
				//@Override
				public boolean satisfy(double now,CarmaStore store) {
					final Integer __ATTR__a = store.get( "a" , Integer.class );
					return ( __ATTR__a )==( 0 );
				}
					
			};
			{
				CarmaAction action = new CarmaOutput(
					__ACT__updatepower , true  		
				) {
					
					@Override
					protected Object getValue(CarmaSystem sys, CarmaStore store,double now) {
						LinkedList<Object> toReturn = new LinkedList<Object>();
						final __RECORD__LSet __ATTR__powers = store.get( "powers" , __RECORD__LSet.class );
						final __RECORD__LSet __ATTR__payoffs = store.get( "payoffs" , __RECORD__LSet.class );
						toReturn.add( __ATTR__powers );
						toReturn.add( __ATTR__payoffs );
						return toReturn;
					}
					
					@Override
					protected CarmaStoreUpdate getUpdate(CarmaSystem sys, double now) {
						return new CarmaStoreUpdate() {					
							//@Override
							public void update(RandomGenerator r, CarmaStore store) {
							}
						};
					}
					
					@Override
					protected CarmaPredicate getPredicate(CarmaSystem sys, final CarmaStore myStore) {
						return CarmaPredicate.TRUE;
						
					}
				};		
				
				_COMP_ComputingServer.addTransition( __STATE___ComputingServer_DC , new CarmaPredicate.Conjunction(  _FOO_predicate0  ) , action , __STATE___ComputingServer_RP );			
			}
		}
		
	}
	
	public CarmaComponent createComponentComputingServer( 
	) {
		CarmaComponent c = new CarmaComponent();
		c.set( "a" ,  0 );
		c.set( "rcvnum" ,  0 );
		c.set( "alphas" ,  new __RECORD__LSet( Double.valueOf( __FUN__Alpha( 
				) ),
		Double.valueOf( __FUN__Alpha( 
				) ),
		Double.valueOf( __FUN__Alpha( 
				) ),
		Double.valueOf( __FUN__Alpha( 
				) ),
		Double.valueOf( __FUN__Alpha( 
				) ),
		Double.valueOf( __FUN__Alpha( 
				) ),
		Double.valueOf( __FUN__Alpha( 
				) ),
		Double.valueOf( __FUN__Alpha( 
				) )
		 ) );
		c.set( "newpowers" ,  new __RECORD__LSet( Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 )
		 ) );
		c.set( "powers" ,  new __RECORD__LSet( Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 )
		 ) );
		c.set( "newpayoffs" ,  new __RECORD__LSet( Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 ),
		Double.valueOf( 0.0 )
		 ) );
		c.set( "payoffs" ,  new __RECORD__LSet( Double.valueOf( -(__FUN__Alpha( 
				)) ),
		Double.valueOf( -(__FUN__Alpha( 
				)) ),
		Double.valueOf( -(__FUN__Alpha( 
				)) ),
		Double.valueOf( -(__FUN__Alpha( 
				)) ),
		Double.valueOf( -(__FUN__Alpha( 
				)) ),
		Double.valueOf( -(__FUN__Alpha( 
				)) ),
		Double.valueOf( -(__FUN__Alpha( 
				)) ),
		Double.valueOf( -(__FUN__Alpha( 
				)) )
		 ) );
		c.addAgent( new CarmaSequentialProcess( c , _COMP_ComputingServer , __STATE___ComputingServer_RP ));
		return c;
	}	
	
	/* END COMPONENT: ComputingServer */
		
	
	public static final int __ACT__updatepower = 0;	
	public static final int __ACT__generaterandom = 1;	
	public static final int __ACT__samepower = 2;	
	public static final int __ACT__changeupdate = 3;	
	public static final int __ACT__decreasepower = 4;	
	public static final int __ACT__increasepower = 5;	
	public static final int __ACT__sendpower = 6;	
	public static final int __ACT__waitalllearners = 7;	
	public static final int __ACT__calcpayoffs = 8;	
	public static final int __ACT__newpayoffAlphas = 9;	
	public static final int __ACT__stablestate = 10;	
	public static final int __ACT__continue = 11;	
	public static final int __ACT__newpayoffpayoff = 12;	
	
	
	public String[] getSystems() {
		return new String[] {
			"Simple"
		};	
	}
	
	public SimulationFactory<CarmaSystem> getFactory( String name ) {
		if ("Simple".equals( name )) {
			return getFactorySystemSimple();
		}
		return null;
	}
			
	
	public class __SYSTEM__Simple extends CarmaSystem {
		
		public __SYSTEM__Simple() {
			super();
			CarmaSystem system = this;
			for ( int i0 = 0 ; 
				i0 <= 7 ; 
				 i0++  ) {					
				system.addComponent( 
					createComponentLearner(					
						Integer.valueOf( i0 ),
						Double.valueOf( 0.0 ),
						Double.valueOf( 0.83 )
					) 
				);
			}
			system.addComponent( 
				createComponentComputingServer(					
				) 
			);
		}
		
		@Override
		public double unicastProbability(
			final CarmaStore sender, 
			final CarmaStore receiver,
			int action) {
			final CarmaSystem system = this;
			final CarmaStore global = this.global;
			return 1.0;
		}
		
		@Override
		public double broadcastProbability(
			final CarmaStore sender, 
			final CarmaStore receiver,
			int action) {
			final CarmaSystem system = this;
			final CarmaStore global = this.global;
			return 1.0;
		}
		
		@Override
		public double broadcastRate(final CarmaStore sender, int action) {
			final CarmaSystem system = this;
			final CarmaStore global = this.global;
			return 1.0;
		}
		
		@Override
		public double unicastRate(final CarmaStore sender, int action) {
			final CarmaSystem system = this;
			final CarmaStore global = this.global;
			return 1.0;
		}
		
		@Override
		public void broadcastUpdate( RandomGenerator random , CarmaStore sender , int action , Object value ) {					
		}
	
		@Override
		public void unicastUpdate( RandomGenerator random , CarmaStore sender , CarmaStore receiver, int action , Object value ) {
		}			
	}
	
	
	public SimulationFactory<CarmaSystem> getFactorySystemSimple() {
		return new SimulationFactory<CarmaSystem>() {
	
			//@Override
			public CarmaSystem getModel() {
				return new __SYSTEM__Simple();
			}
		
			//@Override
			public Measure<CarmaSystem> getMeasure(String name) {
				// TODO Auto-generated method stub
				//FIXME!!!!
				return null;
			}
		
		};
		
	}
	
	
	private HashMap<String,Measure<CarmaSystem>> measures;
			
	public String[] getMeasures() {
		TreeSet<String> sortedSet = new TreeSet<String>( measures.keySet() );
		return sortedSet.toArray( new String[ sortedSet.size() ] );
	}
	
	public Measure<CarmaSystem> getMeasure( String name ) {
		return measures.get( name );
	}
	
	protected void setUpMeasures() {
		measures = new HashMap<String,Measure<CarmaSystem>>();
		buildMeasuredebug( );
	}	
	
	
	private void buildMeasuredebug( ) {
		measures.put( 
						"debug" ,
						getMeasuredebug( )
					);
	}
	
	
	private Measure<CarmaSystem> getMeasuredebug( 
	) {
	
		return new Measure<CarmaSystem>() {
		
			//@Override
			public double measure(final CarmaSystem system) {
				final CarmaStore global = system.getGlobalStore();
				return system.measure( 
					new BasicComponentPredicate(
						new CarmaPredicate() {
							
							//Here we assume that the following "final" references are available (if needed):
							//- global: reference to the global store;
							//- sender: reference to the store of sender;
							//- receiver: reference to the store of the receiver;				
							//@Override
							public boolean satisfy(double now,CarmaStore store) {
								final Integer __MY__a = store.get( "a" , Integer.class );
								try{
									return ( __MY__a )==( 1 );
								} catch (NullPointerException e) {
									return false;
								}
							}
						
							
						}
						, new CarmaProcessPredicate() {
					
							//@Override
							public boolean eval(CarmaProcess p) {
								if (p instanceof CarmaSequentialProcess) {
									CarmaSequentialProcess csp = (CarmaSequentialProcess) p;
									try{
										return csp.getName().equals("ComputingServer")&&csp.getState().getName().equals("FS");
									} catch (NullPointerException e) {
										return false;
									}
								}
								return false;
							}
										
						}
						)
				)
				;
			}
	
			//@Override
			public String getName() {
				return "debug";
			}
		
		};
		
	}
	@Override
	public String[] getMeasureParameters(String name) {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	public Map<String, Class<?>> getParametersType(String name) {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	public Measure<CarmaSystem> getMeasure(String name, Map<String, Object> parameters) {
		// TODO Auto-generated method stub
		return null;
	}
	
	
	
}
