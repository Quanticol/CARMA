package eu.quanticol.carma.core.generator.ms.environment

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.ProbabilityBlock
import eu.quanticol.carma.core.generator.ms.expression.ExpressionHandler
import eu.quanticol.carma.core.utils.Util
import eu.quanticol.carma.core.carma.Probability
import eu.quanticol.carma.core.carma.Expression
import eu.quanticol.carma.core.carma.RateBlock
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.UpdateBlock
import eu.quanticol.carma.core.carma.EnvironmentUpdate
import eu.quanticol.carma.core.generator.ms.collective.CollectiveHandler
import eu.quanticol.carma.core.generator.ms.attribute.AttributeHandler
import eu.quanticol.carma.core.utils.ReferenceContext
import static extension org.eclipse.xtext.EcoreUtil2.*

class EnvironmentHandler {
	
	@Inject extension ExpressionHandler
	@Inject extension Util
	@Inject extension CollectiveHandler
	@Inject extension AttributeHandler
	
	def handleProbabilityBlock( ProbabilityBlock block ) {
		'''
		@Override
		public double unicastProbability(
			final CarmaStore sender, 
			final CarmaStore receiver,
			int action) {
			final CarmaSystem system = this;
			final CarmaStore global = this.global;
			«block.probabilities.filter[ !it.stub.isIsBroadcast ].probabilityBody(block.value)»
		}

		@Override
		public double broadcastProbability(
			final CarmaStore sender, 
			final CarmaStore receiver,
			int action) {
			final CarmaSystem system = this;
			final CarmaStore global = this.global;
			«block.probabilities.filter[ it.stub.isIsBroadcast ].probabilityBody(block.value)»
		}
		'''
	}
	
	def handleRateBlock( RateBlock block ) {
		'''
		@Override
		public double broadcastRate(final CarmaStore sender, int action) {
			final CarmaSystem system = this;
			final CarmaStore global = this.global;
			«block.rates.filter[ it.stub.isIsBroadcast ].rateBody( block.value )»
		}

		@Override
		public double unicastRate(final CarmaStore sender, int action) {
			final CarmaSystem system = this;
			final CarmaStore global = this.global;
			«block.rates.filter[ !it.stub.isIsBroadcast ].rateBody( block.value )»
		}
		'''		
	}
	
	def handleUpdateBlock( UpdateBlock block ) {
		'''
		@Override
		public void broadcastUpdate( 
			final RandomGenerator random , 
			final CarmaStore sender , 
			final int action , 
			final Object value ) {
			final CarmaSystem system = this;
			final CarmaStore global = this.global;
			«block.updates.filter[ it.stub.isBroadcast ].updateBody»	
		}

		@Override
		public void unicastUpdate( 
			final RandomGenerator random , 
			final CarmaStore sender , 
			final CarmaStore receiver, 
			int action , 
			final Object value ) {
			final CarmaSystem system = this;
			final CarmaStore global = this.global;
			«block.updates.filter[ !it.stub.isBroadcast ].updateBody»	
		}		
		'''
	}
	
	def probabilityBody( Iterable<Probability> probabilities , Expression defaultValue ) {
		'''
		«FOR a:(probabilities.map[it.expression]+probabilities.filter[it.guard != null].map[it.guard.booleanExpression]).globalAttributes»
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
		«ENDFOR»
		«FOR a:(probabilities.map[it.expression]+probabilities.filter[it.guard != null].map[it.guard.booleanExpression]).senderAttributes»
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::SENDER,"sender")»
		«ENDFOR»
		«FOR a:(probabilities.map[it.expression]+probabilities.filter[it.guard != null].map[it.guard.booleanExpression]).receiverAttributes»
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::RECEIVER,"receiver")»
		«ENDFOR»
		«FOR p:probabilities»
		if ((action==«p.stub.activity.name.actionName»)
			«IF p.guard != null»
			&&
			(«p.guard.booleanExpression.expressionToJava»)
			«ENDIF»			
			) {
				return «p.expression.expressionToJava»;				
		}
		«ENDFOR»
		«IF defaultValue != null»
		return «defaultValue.expressionToJava»;
		«ELSE»
		return 1.0;
		«ENDIF»			
		'''
	}

	def rateBody( Iterable<Rate> rates , Expression defaultValue ) {
		'''
		«FOR a:(rates.map[it.expression]+rates.filter[it.guard != null].map[it.guard.booleanExpression]).globalAttributes»
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
		«ENDFOR»
		«FOR a:(rates.map[it.expression]+rates.filter[it.guard != null].map[it.guard.booleanExpression]).senderAttributes»
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::SENDER,"sender")»
		«ENDFOR»
		«FOR p:rates»
		if ((action==«p.stub.activity.name.actionName»)
			&&
			«IF p.guard != null»			
			(«p.guard.booleanExpression.expressionToJava»)
			«ENDIF»
			) {
				return «p.expression.expressionToJava»;				
		}
		«ENDFOR»
		«IF defaultValue != null»
		return «defaultValue.expressionToJava»;
		«ELSE»
		return 1.0;
		«ENDIF»			
		'''
	}
	
	def updateBody( Iterable<EnvironmentUpdate> updates ) {
		'''
		«FOR a:updates.map[it.getAllContentsOfType(typeof(Expression))].flatten.globalAttributes»
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::GLOBAL,"global")»
		«ENDFOR»
		«FOR a:updates.map[it.getAllContentsOfType(typeof(Expression))].flatten.senderAttributes»
		«a.attributeTemporaryVariableDeclaration(ReferenceContext::SENDER,"sender")»
		«ENDFOR»
		«FOR u:updates»
		if ((action==«u.stub.activity.name.actionName»)
			&&
			«IF u.guard != null»			
			(«u.guard.booleanExpression.expressionToJava»)
			«ENDIF»
			) {
			«FOR a:u.update»
			global.set( "«a.reference.name»" , «a.expression.expressionToJava» );
			«ENDFOR»
			«IF u.spawn != null»
			«FOR c:u.spawn.comp»
			«c.instantiationCode»
			«ENDFOR»	
			«ENDIF»		
			return ;				
		}
		«ENDFOR»
		'''
	}
	
	
}