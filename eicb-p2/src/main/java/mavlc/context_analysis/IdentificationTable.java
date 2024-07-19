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

import mavlc.errors.OverwritingDeclarationError;
import mavlc.syntax.statement.Declaration;

/* TODO enter group information
 *
 * EiCB group number: 090
 * Names and matriculation numbers of all group members:
 * Thien Nam Vu tv40nusa
 * Quoc Dang Huy Nguyen qn97cuhi
 * Dieu Linh Nguyen dn23qace
 */

/**
 * A table for identifiers used inside a function.
 */
public class IdentificationTable {

	private Scope scope;
	
	/**
	 * Declares the given identifier in the current scope.
	 *
	 * @param name the identifier to declare
	 * @param declaration the reference to the identifier's declaration site
	 */
	public void addIdentifier(String name, Declaration declaration) {
		// TODO implement (task 2.1)
		this.scope.addIdentifier(name, declaration);
	}
	
	/**
	 * Looks up the innermost declaration of the given identifier.
	 *
	 * @param name the identifier to look up
	 * @return the identifier's innermost declaration site
	 */
	public Declaration getDeclaration(String name) {
		// TODO implement (task 2.1)
		return this.scope.getDeclaration(name);
	}
	
	/**
	 * Opens a new scope.
	 */
	public void openNewScope() {
		// TODO implement (task 2.1)
        this.scope= new Scope(this.scope);
	}
	
	/**
	 * Closes the current scope.
	 */
	public void closeCurrentScope() {
		// TODO implement (task 2.1)
		this.scope = scope.parentScope;
	}
}
