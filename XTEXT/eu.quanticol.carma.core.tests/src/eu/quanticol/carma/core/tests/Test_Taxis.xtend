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
class Test_Taxis {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
const SIZE = 3;
const K = 5;

const R_T = 12.0;
const R_C = 6.0;
const R_A = 1.0;
const R_STEP = 1.0;

const P_LOST = 0.2;

record Position =  [ int x , int y ];

fun Position Roving(){
    int pos_x := U(0,1,2);
	int pos_y := 0;
	if (pos_x == 1) {
		pos_y := U(0,2);
	} else {
		pos_y := U(0,1,2);
	}
    return [ x := pos_x , y:= pos_y ];
}

fun Position Dest_loc(real time, Position g){
	Position q := [ x := 0 , y := 0 ];
    if( g.x == 1 && g.y == 1){
    	q := Roving(); 
   } else{
    	q := [ x := 1 , y := 1 ];
    }
    return q;
}

fun real Mrate( Position l1, Position l2){
	real t := real( abs(l1.x - l2.x) + abs(l1.y - l2.y) );
	real r := 0.0;
	if(t > 1.0){
		r := R_STEP / t;
	} else{
		r := R_STEP;
	}
	return r;
}

fun real Arate(real time, Position l1){
	real r := 0.0;
	if ((l1.x == 1)&&(l1.y==1)) {
		if(time < 20)
			r := R_A / 4.0;
		else{
			r := 3.0 * R_A / 4.0;
		}
	}
	else{
		if(time < 20)
			r := 3.0 * R_A / 4.0;
		else{
			r := R_A / 4.0; 
		}
	}
	return r;
}

fun real Takeprob( int taxisAt_loc ){
	real x_:= 0.0;
	if (taxisAt_loc == 0){
		x_ := 0.0;
	}
	else{
		x_ := 1.0/real(taxisAt_loc); 
	}
	return x_;
}

component User(Position g, Position h, process Z){
    
    store{
        attrib _loc := g;
        attrib dest := h;
    }

    behaviour{
        Wait = call*[true]<my._loc.x,my._loc.y>.Wait + 
        take[_loc.x == my._loc.x && _loc.y == my._loc.y]<my.dest.x,my.dest.y>.kill;    
	}
	
    init{
        Z
    }
}

component Taxi(int a, int b, int c, int d, int e, process Z){
    
    store{
        attrib _loc := [ x:= a , y:= b];
        attrib dest := [ x:=c , y:=d];
        attrib occupancy := e;
    }

    behaviour{
        F = take[true](posx,posy){dest := [x:=posx,y:=posy]; occupancy := 1;}.G + 
        	call*[(my._loc.x != posx)&&(my._loc.y !=posy)](posx,posy){dest := [ x:=posx,y:=posy]; }.G;
        G = move*[false]<>{_loc := dest; dest := [x:=3,y:=3]; occupancy := 0;}.F;
    }
    
    init{
        Z
    }
}

component Arrival(int a, int b){
	
	store{
		attrib _loc := [ x:=a , y:= b];
	}
	
	behaviour{
		A = arrival*[false]<>.A;
	}
	
	init{
		A
	}
}

	measure WaitingUser( int i , int j ) = #{User[Wait] | my._loc.x == i && my._loc.y == j };
	measure FreeTaxi( int i , int j ) = #{Taxi[F] | my._loc.x == 0 && my._loc.y == 0 };
	measure All_User = #{User[*] | true };

system Scenario1{

    collective{
    	for( i ; i<K ; i+1 ) {
          new Taxi(0:SIZE-1,0:SIZE-1,3,3,0,F);
        }
	    new Arrival(0:SIZE-1,0:SIZE-1);
    }

    environment{
    	
    	
    	prob{
              call* { return 1-P_LOST; }
              default { return 1.0; }
    	}
    	
    	weight{
              take { return Takeprob(#{Taxi[F] | my._loc == sender._loc }); }
              default { return 1.0; }
    	}
    	
        rate{
        	take { return R_T; }
    		call* { return R_C; }
    		move* { return Mrate(sender._loc,sender.dest); }
    		arrival* { return R_A * (1.0 / real( SIZE * SIZE )) ; }
    		default { return 0.0; }
        }
        
        update{
        	arrival* {
        		new User(sender._loc,Dest_loc(now,sender._loc), Wait);
        	}
        }
    }
    
}

system Scenario2{

    collective{
    	for( i ; i<K ; i+1 ) {
          new Taxi(0:SIZE-1,0:SIZE-1,3,3,0,F);
        }
	    new Arrival(0:SIZE-1,0:SIZE-1);
    }

    environment{
    	
    	prob{
              call* { return 1-P_LOST; }
              default { return 1.0; }
    	}

    	weight{
              take { return Takeprob(#{Taxi[F] | my._loc == sender._loc }); }
              default { return 1.0; }
    	}
    	
        rate{
        	take { return R_T; }
    		call* { return R_C; }
    		move* { return Mrate(sender._loc,sender.dest); }
    		arrival* { return Arate(now,sender._loc); }
    		default { return 0.0; }
        }
        
        update{
        	arrival* {
        		new User(sender._loc,Dest_loc(now,sender._loc), Wait);
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
			assertEquals( 2 , m.systems.length )
			assertEquals( 3 , m.measures.length )					

	]
	}
	
}