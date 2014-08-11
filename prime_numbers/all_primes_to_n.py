#!/usr/bin/python

'''

David Lettier (C) 2014.

http://www.lettier.com/

'''

import sys;
import functools;
import math;
	
try:

	n = int( sys.argv[ 1 ], 10 );
	
except ( IndexError, ValueError ) as error:
	
	print( "Usage: $ python is_prime.py <number>" );
	
	sys.exit( );
	
if ( n < 2 ):
	
	print( "Input must be 2 or greater." );
	
	sys.exit( );
	
# Sieve of Eratosthenes algorithm.
	
primes = [ True ] * ( n + 1 );

for i in range( 2, n + 1 ):
	
	if ( primes[ i ] == True ):
		
		for j in range( 2, math.ceil( ( n + 1 ) / i ) ):
			
			primes[ i * j ] = False;

	else:
		
		continue;

print( functools.reduce( lambda x, y: str( x ) + " " + str( y ), list( filter( lambda x: True if primes[ x ] == True else False, range( 2, n + 1 ) ) ) ) );