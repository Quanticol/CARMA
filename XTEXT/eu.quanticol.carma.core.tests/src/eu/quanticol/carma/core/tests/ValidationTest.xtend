package eu.quanticol.carma.core.tests
//
import com.google.inject.Inject
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Test
import org.junit.runner.RunWith
import eu.quanticol.carma.core.CARMAInjectorProvider
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.validation.CARMAValidator
import eu.quanticol.carma.core.carma.CarmaPackage

//
@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProvider))
class ValidationTest {
	
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper
	
	
	/**
	 * 		
	    fun integer Test(integer v){
			return v + 1;
		}
		
		fun integer Test2(integer v){
			return v + 1;
		}
				
		component Comp1(){
			store{
				enum a := 0;
				record b := {x := 1, y :=1}; 
			}
				
			behaviour{
				P = [b == {x := 1, y:= 1}] nothing*<a,b,b>{a := a + 1}.P;
			}
			
			init{
				P;
			}
		}
		
		component Comp2(Z){
			store{
				enum a := 0;
				enum e := 0;
			}
				
			behaviour{
				P =  nothing*(b,c).P;
			}
			
			init{
				Z;
			}
		}
		
		Q = Q;
				
		system Bleh{
			collective{
				new Comp1();
				new Comp2(P);
			}
			environment{
				store{
					record loc := {x := 1, y :=1}; 
				}
			}
		}
	 */
	
	
	@Test
	def void test_ERROR_ActionStub_reference(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        Produce = produce*<>{my.product := my.product + 1}.Produce;
		    }
		
		    init{
		        Produce;
		    }
		
		}
		
		system SimpleMove{
		
		    collective{
		        new Producer();
		    }
		
		    environment{
		        rate{
		        //problem here...
		        [True] produce*() := 1;
		        }
		    }
		}
		'''.parse.assertError(CarmaPackage::eINSTANCE.actionStub,
			CARMAValidator::ERROR_ActionStub_reference,
			CARMAValidator::ERROR_ActionStub_reference)
	}
	
	@Test
	def void test_ERROR_VariableDeclaration_type(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        Produce = produce*.Produce;
		    }
		
		    init{
		        Produce;
		    }
		
		}
		
		component ProducerT(){
		    store{
		        record product := {a := 0};
		    }
		
		    behaviour{
		        Produce = produce*.Produce;
		    }
		
		    init{
		        Produce;
		    }
		
		}
		
		system SimpleMove{
		
		    collective{
		        new Producer();
		    }
		
		    environment{
		        rate{
		        [True] produce*() := 1;
		        }
		    }
		}
		'''.parse.assertError(CarmaPackage::eINSTANCE.actionStub,
			CARMAValidator::ERROR_ActionStub_reference,
			CARMAValidator::ERROR_ActionStub_reference)
	}
	
	@Test
	def void test_ERROR_BooleanExpression_expression_boolean_type(){
		'''
		fun double MTime(record location, record destination){
			//TODO 
			double step := 0.0;
			return step;
		}
		
		fun double ATime(record location){
			//TODO
			double arrivalRate := 0.0;
			return arrivalRate * (1/9);
		}
		
		fun record DestLoc(record location){
			//TODO
			if((location.x == 1) && (location.y == 1)){
				return 0;
			} else {
				record newLocation := (x := 1, y := 1);
				return newLocation;
			};
		}
		
		component Taxi(record loc){
			store{
				record location := loc;
				enum occupancy := 1;
				record destination := ( x := 1, y := 1);
			}
			
			behaviour{
				F = take[location == this.location](d){destination := d, occupancy := 1}.G +
					call*[l != this.location](d,l){destination := d}.G;
				G = move*{location := destination, occupancy := 0}.F;
			}
			
			init{
				F;
			}
		}
		
		component User(record loc, record dest){
			store{
				record location := loc;
				record destination := dest;
			}
			
			behaviour{
				W = call*<location>.W +
					take[location == this.location]<destination>.kill;
			}
			
			init{
				W;
			}
		}
		
		component Arrivals(record loc){
			store{
				record location := loc;
			}
			
			behaviour{
				A = arrival*.A;
			}
			
			init{
				A;
			}
		}
			
		measures{
			measure Waiting = #{User[*]  | location == (x := 1..3, y:= 1..3) };
		}
			
		system SmartTaxi {	
			
			collective{
				new Taxi((x := 1..3, y:= 1..3));
				new Arrivals((x := 1..3, y:= 1..3));
			}
		
			environment{
				
				store{
					
				}
				
				prob{
					[True] take<> := 1/#{Taxi[F] | location == sender.location};
				}
				
				rate{
					[True] take<> 		:= 0.01;
					[True] call* 		:= 0.01;
					[True] move* 		:= MTime(sender.location,sender.destination);
					[True] arrival* 	:= ATime(location);
					default := 0.0;
				}
				
				update{
					[True] arrival* := new User(sender.loc, DestLoc(sender.loc));
				}
			}
		}
		'''.parse.assertError(CarmaPackage::eINSTANCE.booleanExpression,
			CARMAValidator::ERROR_BooleanExpression_expression_boolean_type,
			CARMAValidator::ERROR_BooleanExpression_expression_boolean_type)
	}
	
	@Test
	def void test_WARN_ComponentBlockDefinition_unused(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        Produce = produce*{product := product + 1}.Send;
		        Send = send<1>{product := product - 1}.Produce;
		    }
		
		    init{
		        Produce;
		    }
		
		}
		
		component Consumer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        Receive = send(z){product := product + z}.Consume;
		        Consume = consume*{product := product + 1}.Receive;
		    }
		
		    init{
		        Produce;
		    }
		
		}
		
		system Simple{
		
		    collective{
		        new Producer();
		    }
		
		    environment{
		        rate{
		        [True] produce* := 1;
		        }
		    }
		}'''.parse.assertWarning(CarmaPackage::eINSTANCE.componentBlockDefinition,
			CARMAValidator::WARN_ComponentBlockDefinition_unused,
			CARMAValidator::WARN_ComponentBlockDefinition_unused)
	}
	
	@Test
	def void test_ERROR_MacroExpressionReference_noAccess(){
	'''
	component Producer(){
	    store{
	        enum product := 0;
	    }
	
	    behaviour{
	        Produce = produce*{product := product + 1}.Produce;
	        Send = send<>{product := product - 1}.Send;
	    }
	
	    init{
	        Produce | Send;
	    }
	
	}
	
	component Consumer(){
	    store{
	        enum product := 0;
	    }
	
	    behaviour{
	        Receive = send(){product := product + 1}.Receive;
	        Consume = consume*{product := product - 1}.Consume;
	    }
	
	    init{
	        Produce | Consume;
	    }
	
	}
	
	system Simple{
	
	    collective{
	        new Producer();
	        new Consumer();
	    }
	
	    environment{
	        rate{
	        [True] produce* := 1;
	        }
	    }
	}
	'''.parse.assertError(CarmaPackage::eINSTANCE.macroExpressionReference,
			CARMAValidator::ERROR_MacroExpressionReference_noAccess,
			CARMAValidator::ERROR_MacroExpressionReference_noAccess)
	}
	
	@Test
	def void test_ERROR_ActionName_type_unique(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		       P = move*.Q;
		       Q = move<>.R;
		       R = move().P;
		    }
		
		    init{
		        P;
		    }
		
		}
		
		system SimpleMove{
		
		    collective{
		        new Producer();
		    }
		
		    environment{
		    }
		}
		'''.parse.assertError(CarmaPackage::eINSTANCE.actionName,
			CARMAValidator::ERROR_ActionName_type_unique,
			CARMAValidator::ERROR_ActionName_type_unique)
	}
	
	@Test
	def void test_ERROR_CBND_reference(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        Produce = [my.product > 0] produce*{product := product + 1}.Produce + [my.product == 0] produceDouble*{product := product + 2}.Produce;
		        Send = [my.product > 0] send*{product := product - 1}.Send;
		    }
		
		    init{
		        Produce | Send;
		    }
		
		}
		
		
		system Simple{
		
		    collective{
		        new Producer();
		        //heh? validation rule missing... There can be no declaration without a definition
		        new Consumer();
		    }
		
		    environment{
		        rate{
		        [True] produce* := 1;
		        [True] produceDouble* := 1;
		        [True] send* := 1;
		        }
		    }
		}'''.parse.assertError(CarmaPackage::eINSTANCE.CBND,
			CARMAValidator::ERROR_CBND_reference,
			CARMAValidator::ERROR_CBND_reference)
	}
	
	@Test
	def void test_ERROR_CBND_matching(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        Produce = [my.product > 0] produce*{product := product + 1}.Produce + [my.product == 0] produceDouble*{product := product + 2}.Produce;
		        Send = [my.product > 0] send*{product := product - 1}.Send;
		    }
		
		    init{
		        Produce | Send;
		    }
		
		}
		
		
		system Simple{
		
		    collective{
		        new Producer(1);
		    }
		
		    environment{
		        rate{
		        [True] produce* := 1;
		        [True] produceDouble* := 1;
		        [True] send* := 1;
		        }
		    }
		}'''.parse.assertError(CarmaPackage::eINSTANCE.CBND,
			CARMAValidator::ERROR_CBND_matching,
			CARMAValidator::ERROR_CBND_matching)
	}
	
	@Test
	def void test_ERROR_ProcessExpressionGuard_following_action(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        P = [True] P;
		    }
		
		    init{
		        P;
		    }
		
		}
		
		
		system Simple{
		
		    collective{
		        new Producer();
		    }
		
		    environment{
		    }
		}
		'''.parse.assertError(CarmaPackage::eINSTANCE.processExpressionGuard,
			CARMAValidator::ERROR_ProcessExpressionGuard_following_action,
			CARMAValidator::ERROR_ProcessExpressionGuard_following_action)
		
	}
}