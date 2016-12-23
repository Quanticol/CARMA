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
class Test_Meadows_m {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
	
	CharSequence code = 	'''
fun int DistanceSquared(location a, location b){
	return a.x*b.x + a.y*b.y;
}

//fun int GetCongestion(location a, location b){
//	return #{Pedestrian[B] | my.loc==a && my.next == b} + #{Bike[B] | my.loc==a && my.next == b}; 
//}

fun location GetNextClosestToDestination(location current, location destination){
	location choice := GetNextRandomChoice(current);
	for possibleNext in  current.post {
		if(DistanceSquared(possibleNext, destination) < DistanceSquared(choice, destination)){
			choice = possibleNext;
		}
	}
	return choice;
}



fun location GetNextRandomChoice(location current){
	location choice := U(current.post);
	return choice;
}

//fun location GetNextSmallestCongestion(location current){
//	location choice := U(current.post);
//	for possibleNext in  current.post{
//		if(GetCongestion(possibleNext, current) < GetCongestion(choice, current)){
//			choice = possibleNext;
//		}
//	}
//	return choice;
//}



space Grid (int width, int height) { 
	universe <int x, int y>
        nodes {
            A[6,7];
            A[6,4];
            A[1,2];
            A[3,1];
            A[6,0];
            A[9,1];
            A[12,2];
        }
        connections {
        	A[6,7]  <-> A[6,4]  {two_lane=1};
        	A[1,2]  <-> A[6,4]  {two_lane=1};
        	A[12,2] <-> A[6,4]  {two_lane=1};
        	A[6,0]  <-> A[6,4]  {two_lane=1};
        	A[1,2]  <-> A[6,0]  {two_lane=1};
        	A[6,0]  <-> A[9,1]  {two_lane=1};
        	A[9,1]  <-> A[12,2] {two_lane=1};
        	A[6,4]  <-> A[9,1]  {two_lane=0};
        	A[6,4]  <-> A[3,1]  {two_lane=0};
        	
        }
        areas {
        	
        }
}

component Pedestrian(location start, location destination, process Z){
    store{
        attrib current := start;
        attrib destination := destination;
        attrib next := GetNextRandomChoice(current);
    }
    behaviour{
        B = chooseNextMove*<>{my.next:=GetNextRandomChoice(my.current);}.C;
        C = move*<>{my.current:=my.next;}.D;
        D = [my.current == my.destination]arrive*<>.F + [my.current != my.destination]continue*<>.B;
        F = idle*<>.F;
    }    
    init{Z}
}

component Bike(location start, location destination, process Z){
    store{
        attrib current := start;
        attrib destination := destination;
        attrib next := GetNextRandomChoice(current);
    }
    behaviour{
        B = chooseNextMove*<>{my.next:=GetNextRandomChoice(my.current);}.C;
        C = move*<>{my.current:=my.next;}.D;
        D = [my.current == my.destination]arrive*<>.F + [my.current != my.destination]continue*<>.B;
        F = idle*<>.F;
    }    
    init{Z}
}

//component Generator( ){
//	store{
//
//    }
//    behaviour{
//        G = spawnBike*<>{new Bike(GetStart(time),GetDestination(time), B)}.G;
//        H = spawnPedestrian*<>{new Pedestrian(GetStart(time),GetDestination(time), B)}.H;
//    }    
//    init{G | H}
//}

measure pedestrians = #{Pedestrian[B] | true};
measure pedestrians2 = #{ * | true};
measure pedestrians3 = #{ Pedestrian[F] | true};

system Meadows{
	space Grid (12, 12)

    collective{
    		new Pedestrian(A[6,7], A[12,2], B);       
			new Pedestrian(A[6,7], A[6,0], B);
			new Pedestrian(A[12,2], A[1,2], B);       
			new Pedestrian(A[12,2], A[1,2], B);  
			new Pedestrian(A[6,7], A[3,1], B); 
//        for (i; i<100; 1) {
//			new Pedestrian(A[6,7], A[12,2], B);       
//			new Pedestrian(A[6,7], A[6,0], B);
//			new Pedestrian(A[12,2], A[1,2], B);       
//			new Pedestrian(A[12,2], A[1,2], B);  
//			new Pedestrian(A[6,7], A[3,1], B);  
//         }
    }
    environment{
       	prob{    
              default { return 1.0;}
    	}
        weight{
              default {return 1.0;}        	
        }    	
        rate{
        
    		default { return 0.0;}
        }
        update{

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
			var factory = m.getFactory("Meadows")
			var measure = m.getMeasure("pedestrians")			
			var measure2 = m.getMeasure("pedestrians2")			
			var measure3 = m.getMeasure("pedestrians3")			
			var sys = factory.model
			assertEquals( 5 , measure.measure( sys ) , 0.000001 )
			assertEquals( 5 , measure2.measure( sys ) , 0.000001 )
			assertEquals( 0 , measure3.measure( sys ) , 0.000001 )

	]
	}
	
}