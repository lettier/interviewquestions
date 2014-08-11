/*
*
* David Lettier (C) 2014.
*
* http://www.lettier.com/
*
*/

// One's constructor.

function One( a, b, c )
{
	// Private.
	
	var hidden = 0;	
	
	// Public.
	
	this.name = "One";
	
	this.a = a || "a";
	this.b = b || "b";
	this.c = c || "c";	
	
	this.increment_hidden = function ( ) {
		
		hidden += 1;
		
	}
	
	this.print_hidden = function ( ) {
		
		console.log( "hidden:", hidden );
		
	}
	
}

// Extend the One class with a member function.

One.prototype.i_am = function ( ) {
	
	console.log( "this:", this );
	
}

// A static method.

One.static_method = function ( ) {
	
	console.log( "this:" , this );
	
}

One.static_method( );

// Create an instance of One.

var one = new One( );

console.log( "One:" );
one.increment_hidden( );
one.print_hidden( );
one.i_am( );

// Two's constructor.

function Two( d )
{
	// Call One's constructor first.
	// If we didn't do this, Two would not inherit One's
	// constructor properties and methods.
	
	One.call( this, 1, 2, 3 );
	
	// Extend Two with public properties.
	
	this.name = "Two";
	
	this.d = d || "d";
	
}

// Two inherits from One. One way of doing inheritance.

Two.prototype = Object.create( One.prototype );

Two.prototype.constructor = Two;

// Create an instance of Two.

var two = new Two( );

console.log( "\nTwo:" );

two.increment_hidden( );
two.print_hidden( );
two.i_am( );
console.log( "two.__proto__", two.__proto__ );

// Dynamically extend the One class definition with a new property.

One.prototype.e = "e";

// Two has it now.

console.log( "two.e:", two.e );

// Three's constructor.

function Three( f )
{
	
	this.name = "Three";
	this.f = f || "f";
	
}

// Second way of doing inheritance. 

Three.prototype = new Two;

Three.prototype.constructor = Three;

// Create an instance of Three.

var three = new Three( );

console.log( "\nThree:" );

three.increment_hidden( );
three.print_hidden( );
three.i_am( );
console.log( "three.__proto__:", three.__proto__ );
console.log( "three.e:", three.e );

// Four's constructor.

function Four( )
{
	
	One.call( this, 4, 5, 6 );
	Two.call( this, 7 ); // This will overwrite a=4, b=5, and c=6 from One's constructor.
	Three.call( this, 8 );
	
	this.name = "Four";
	
}

// Four won't inherit anyone's prototype
// so give Four its own i_am method.

Four.prototype.i_am = function ( ) {
	
	var that = this;
	
	console.log( "that:", that );
	
}

// Create an instance of Four.

var four = new Four( );

console.log( "\nFour:" );

four.increment_hidden( );
four.print_hidden( );
four.i_am( );
console.log( "four.__proto__:", four.__proto__ );
console.log( "four.e:", four.e ); // Undefined as Four doesn't inherit the 'e' property.

/*
 * 
 * Output:
 * 
 * this: function One( a, b, c )
 * {
 *        // Private.
 * 
 *        var hidden = 0;
 * 
 *        // Public.
 * 
 *        this.name = "One";
 * 
 *        this.a = a || "a";
 *        this.b = b || "b";
 *        this.c = c || "c";
 * 
 *        this.increment_hidden = function ( ) {
 * 
 *                hidden += 1;
 * 
 *        }
 * 
 *        this.print_hidden = function ( ) {
 * 
 *                console.log( "hidden:", hidden );
 * 
 *        }
 * 
 * }
 * One:
 * hidden: 1
 * this: { name: 'One',
 *  a: 'a',
 *  b: 'b',
 *  c: 'c',
 *  increment_hidden: [Function],
 *  print_hidden: [Function] }
 * 
 * Two:
 * hidden: 1
 * this: { name: 'Two',
 *  a: 1,
 *  b: 2,
 *  c: 3,
 *  increment_hidden: [Function],
 *  print_hidden: [Function],
 *  d: 'd' }
 * two.__proto__ {}
 * two.e: e
 * 
 * Three:
 * hidden: 1
 * this: { name: 'Three', f: 'f' }
 * three.__proto__: { name: 'Two',
 *  a: 1,
 *  b: 2,
 *  c: 3,
 *  increment_hidden: [Function],
 *  print_hidden: [Function],
 *  d: 'd' }
 * three.e: e
 * 
 * Four:
 * hidden: 1
 * that: { name: 'Four',
 *  a: 1,
 *  b: 2,
 *  c: 3,
 *  increment_hidden: [Function],
 *  print_hidden: [Function],
 *  d: 7,
 *  f: 8 }
 * four.__proto__: { i_am: [Function] }
 * four.e: undefined
 *  
 */
