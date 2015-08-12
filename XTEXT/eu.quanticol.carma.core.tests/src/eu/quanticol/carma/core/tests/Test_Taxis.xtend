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

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class Test_Taxis {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
//This is the taxi model implemented according to the senario 2 in the paper.
//There are some slightly change in the fun Mrate and Takeprob

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

fun Position DestLoc(real time, Position g){
	Position q := [ x := 0 , y := 0 ];
    if( g.x == 1 && g.y == 1){
    	q := Roving(); 
   } else{
    	q := [ x := 1 , y := 1 ];
    }
    return q;
}

fun int Mrate(real time, Position l1, Position l2, int r_step){
	int t := abs(l1.x - l2.x) + abs(l1.y - l2.y);
	int r := 0;
	if(t > 1){
		r := r_step / t;
	} else{
		r := r_step;
	}
	return r;
}

fun int Atime(real time, Position l1, int r_a){
	int r := 0;
	if ((l1.x == 1)&&(l1.y==1)) {
		if(time < 20)
			r := r_a / 4;
		else{
			r := 3 * r_a / 4;
		}
	}
	else{
		if(time < 20)
			r := 3 * r_a / 4;
		else{
			r := r_a / 4; 
		}
	}
	return r;
}

fun real Takeprob( real taxisAtLoc ){
	// taxisAtLoc := #{Taxi[F] | my.loc == l1 }; 
	// This x wants to count how many taxis are in the grid from which the user sent the message.
	// But it seems that the x is alway equal to 0,because after remove the if-esle structure and
	// return 1/x directly, there will be some error when the programme is executed.
	real x_:= 0.0;
	if (taxisAtLoc == 0){
		x_ := 0.8;
	}
	else{
		x_ := 1.0/taxisAtLoc; 
	}
	return x_;
}

fun real Mtime( real time , Position senderLoc , Position destinationLoc , int r_a ) {
	return 1.0;
}

component User(Position g, Position h, process Z){
    
    store{
        attrib loc := g;
        attrib dest := h;
    }

    behaviour{
        Wait = call*[true]<my.loc.x,my.loc.y>.Wait + take[loc.x == my.loc.x && loc.y == my.loc.y]<my.dest.x,my.dest.y>.kill;    
	}
	
    init{
        Z
    }
}

component Taxi(int a, int b, int c, int d, int e, process Z){
    
    store{
        attrib loc := [ x:= a , y:= b];
        attrib dest := [ x:=c , y:=d];
        attrib occupancy := e;
    }

    behaviour{
        F = take[true](posx,posy){dest := [x:=posx,y:=posy], occupancy := 1}.G + 
        	call*[(my.loc.x != posx)&&(my.loc.y !=posy)](posx,posy){dest := [ x:=posx,y:=posy] }.G;
        G = move*[false]<>{loc := dest, dest := [x:=3,y:=3], occupancy := 0}.F;
    }
    
    init{
        Z
    }
}

component Arrival(int a, int b){
	
	store{
		attrib loc := [ x:=a , y:= b];
	}
	
	behaviour{
		A = arrival*[false]<>.A;
	}
	
	init{
		A
	}
}

	measure WaitingUser_00[ i := 0 ] = #{User[Wait] | my.loc.x == 0 && my.loc.y == 0 };
	measure WaitingUser_02[ i := 0 ] = #{User[Wait] | my.loc.x == 0 && my.loc.y == 2 };
	measure WaitingUser_11[ i := 0 ] = #{User[Wait] | my.loc.x == 1 && my.loc.y == 1 };
	measure FreeTaxi_00[ i := 0 ] = #{Taxi[F] | my.loc.x == 0 && my.loc.y == 0 };
	measure FreeTaxi_02[ i := 0 ] = #{Taxi[F] | my.loc.x == 0 && my.loc.y == 2 };
	measure FreeTaxi_11[ i := 0 ] = #{Taxi[F] | my.loc.x == 1 && my.loc.y == 1 };
	measure Taxi_Relocating[ i := 1 ] = #{Taxi[G] | my.occupancy == 0};
	measure All_User[ i := 1 ] = #{User[*] | true };



system Taxi_Dispatch{

    collective{
         new Arrival(0:2,0:2);
         new Taxi(0:2,0:2,3,3,0,F);
    }

    environment{
    	
    	store{
    		attrib p_lost := 0.2;
    		attrib r_c := 6;
    		attrib r_t := 12;
    	}
    	
    	prob{
    		[true] take : Takeprob(real(#{Taxi[F] | (my.loc.x == sender.loc.x)&&(my.loc.y == sender.loc.y) }));
      		[true] call* : global.p_lost;
    		default : 1.0;
    	}
    	
        rate{
        	[true] take : global.r_t;
    		[true] call* : global.r_c;
    		[true] move* : Mtime(now,sender.loc,sender.dest,6);
    		[true] arrival* : Atime(now,sender.loc,1);
    		default : 0.0;
        }
        
        update{
        	[true] arrival* : new User(sender.loc,DestLoc(now,sender.loc), Wait);
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
		code.compile[ getCompiledClass.newInstance ]
	}
	
}