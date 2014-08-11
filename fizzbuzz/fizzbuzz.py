#!/usr/bin/python

'''

David Lettier (C) 2014.

http://www.lettier.com/

'''

import sys;
import functools;
	
try:

	m = int( sys.argv[ 1 ] );
	n = int( sys.argv[ 2 ] );
	
except ( IndexError, ValueError ):
	
	m = 0;
	n = 15;
	
	print( "Using default values." );

print( functools.reduce( lambda x, y: str( x ) + ", " + str( y ), 
list( map( lambda x: 'FizzBuzz' if x % 15 == 0 else 'Fizz' if x % 3 == 0 else 'Buzz' if x % 5 == 0 else x, 
range( m, n + 1 ) ) ) ) );