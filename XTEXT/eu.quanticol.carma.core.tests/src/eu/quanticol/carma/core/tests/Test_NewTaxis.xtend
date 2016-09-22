package eu.quanticol.carma.core.tests

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Model
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper
import org.junit.Test
import org.junit.runner.RunWith
import static extension org.junit.Assert.*
import eu.quanticol.carma.simulator.CarmaModel

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_NewTaxis {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''const SIZE = 3;
	const K = 5;
	
	const MAX_USER = int(1.5*(SIZE*SIZE)*K);	
	
	const R_T = 5.0;
	const R_C = 0.25;
	const R_A = 2.0;
	const R_STEP = 1.0;
	
	const P_LOST = 0.125; //0.2;
	const SWITCH_TIME = 100;
	
	space grid( int width , int height ) {
		universe <int x,int y>
	   nodes {
	   	for r from 0 to width {
	   		for c from 0 to height {
	   			[ r , c ];	
	   		}	
	   	}
	   }
	   connections {
	      [?x,?y]: x<width-1 <-> [x+1,y] : w=1.0;
	      [?x,?y]: y<height-1 <-> [x,y+1]: w=1.0;
	      [?x,?y]: x<width-1 && y<height-1 <-> [x+1,x+1]: w=1.0;
	   }
	   areas {
	      centre {
	      	[?x,?y]: (x==width/2)&&(y==height/2);
	      }
	      border {
	       [?x,?y]: x==0||x==width-1||y==0||y==height-1;
	      }
		} 
	}
	
	fun real Mrate( node l1, node l2){
		real t := real( abs(l1.x - l2.x) + abs(l1.y - l2.y) );
		real r := 0.0;
		if(t > 1.0){
			r := R_STEP / t;
		} else{
			r := R_STEP;
		}
		return r;
	}
	
	fun real Arate(real time, node l1){
		real r := 0.0;
		if (l1.centre) {
			if(time < SWITCH_TIME)
				return 0.5*R_A; 
			else{
				return 1.5*R_A;  
			}
		}else{
			if(time < SWITCH_TIME)
				return R_A;
			else{
				return 0.4*R_A;
			}
		}
	}
	
	fun real Takeprob( int taxisAtLoc ){
		real x_:= 0.0;
		if (taxisAtLoc == 0){
			x_ := 0.0;
		}
		else{
			x_ := 1.0; 
		}
		return x_;
	}
	
	component User(node userDestination){   
		store{
			dest = userDestination;
		}
		behaviour{
			Wait = call*[true]<loc>.Wait + take[my.loc == loc]<my.dest>.kill;    
		}
	    init{
	        Wait
	    }
	}
	
	component Taxi( ){
		store{
			node dest = none;
			free = true;
		}
	    behaviour{
			F = [free] take[true](x){
					dest = x; free = false;
				}.G 
				+ call*[(my.loc != pos)](pos){
					dest = pos;
				}.G;
	        G = move*[false]<>{
	        		loc = dest; 
	        		dest = none; 
	        		free = true;
	        }.F;
	    }
	    
	    init{
	        F
	    }
	}
	
	component Arrival(){	
		store{}	
		behaviour{
			A = arrival*[false]<>.A;
		}	
		init{ A }
	}
	
	collective TaxiCollective {	
		for l in locations {
			new Taxi()@l< K >;
			new Arrival()@l;
		}	
	} 
	
	
	measure WaitingUser( int i , int j ) = #{User[Wait] | my.loc.x == i && my.loc.y == j };
	measure Travelling = real( #{ Taxi[G] | my.free } )/(K*SIZE*SIZE);
	
	system Scenario1 {
	
		space grid( SIZE , SIZE )
		
	    collective TaxiCollective
	
	    environment{
		store {
			active_users = 0;
		}
	    	
	    	
	    	prob{
	              call* { return 1-P_LOST; }
	              default { return 1.0; }
	    	}
	    	
	    	weight{
	              take { return Takeprob(#{Taxi[F] | my.loc == sender.loc }); }
	              default { return 1.0; }
	    	}
	    	
	        rate{
	        	take { 
	        		return R_T;
	        	}
	    		call* { return R_C; }
	    		move* { return Mrate(sender.loc,sender.dest); }
	    		arrival* { return R_A; 
	    		}
	    		default { return 0.0; }
	        }
	        
	        update{
	        	arrival* {
	        		if (active_users<MAX_USER) {
		        		if (sender.loc.centre) {
			        		new User( U( locations - {: [1,1] :}) )@sender.loc;
		        		} else {
		        			new User([1,1])@sender.loc;
		        		}
		        		active_users = active_users +1;
	        		}
	        	}
	        	take {
	        		active_users = active_users -1;
	        	}
	        }
	    }
	    
	}

	'''
	
	@Test
	def void test_Parser(){
		code.parse.assertNoErrors
	}

	@Test
	def void test_Compiler(){
		class.classLoader.setJavaCompilerClassPath
		code.compile[ 
					var o = getCompiledClass.newInstance 
			assertNotNull( o )
			assertTrue( o instanceof CarmaModel )
			var m = o as CarmaModel
			assertEquals( 1 , m.systems.length )
			assertEquals( 2 , m.measures.length )					

	]
	}
	
}