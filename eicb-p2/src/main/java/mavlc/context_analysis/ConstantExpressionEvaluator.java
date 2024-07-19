/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 *
 * All rights reserved.
 *
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.context_analysis;

import mavlc.errors.NonConstantExpressionError;
import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeBaseVisitor;
import mavlc.syntax.expression.*;
import mavlc.type.Type;

/* TODO enter group information
 *
 * EiCB group number: 090
 * Names and matriculation numbers of all group members:
 * Thien Nam Vu tv40nusa
 * Quoc Dang Huy Nguyen qn97cuhi
 * Dieu Linh Nguyen dn23qace
 */

public class ConstantExpressionEvaluator extends AstNodeBaseVisitor<Integer, Void> {
	@Override
	protected Integer defaultOperation(AstNode node, Void obj) {
		if(node instanceof Expression) {
			throw new NonConstantExpressionError((Expression) node);
		} else {
			throw new RuntimeException("Internal compiler error: should not try to constant-evaluate non-expressions");
		}
	}
	
	@Override
	public Integer visitIntValue(IntValue intValue, Void __) {
		return intValue.value;
	}
	
	// TODO implement (exercise 2.3)


	@Override
	public Integer visitUnaryMinus(UnaryMinus unaryMinus, Void obj) {
		return -unaryMinus.operand.accept(this);
	}

	@Override
	public Integer visitAddition(Addition addition, Void obj) {
		return addition.leftOperand.accept(this)+addition.rightOperand.accept(this);
	}

	@Override
	public Integer visitSubtraction(Subtraction subtraction, Void __) {
		return subtraction.leftOperand.accept(this)-subtraction.rightOperand.accept(this);
	}

	@Override
	public Integer visitMultiplication(Multiplication multiplication, Void __) {
		return multiplication.leftOperand.accept(this)*multiplication.rightOperand.accept(this);	}

	@Override
	public Integer visitDivision(Division division, Void __) {
		return division.leftOperand.accept(this)/division.rightOperand.accept(this);
	}

	@Override
	public Integer visitExponentiation(Exponentiation exponentiation, Void __) {
		return (int) Math.pow(exponentiation.leftOperand.accept(this),exponentiation.rightOperand.accept(this));
	}

	@Override
	public Integer visitBoolValue(BoolValue boolValue, Void __) {
		return 0;
	}

	@Override
	public Integer visitFloatValue(FloatValue floatValue, Void __) {
		return (int) floatValue.value;
	}

	@Override
	public Integer visitStringValue(StringValue stringValue, Void __) {
		return 0;
	}
}
