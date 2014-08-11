#!/usr/bin/python

'''

David Lettier (C) 2014.

http://www.lettier.com/

'''

import sys;
	
if ( len( sys.argv ) >= 2 ):
	
	sys.argv[ 1 ] = sys.argv[ 1 ].split( " " );
	
else:
	
	print( "Usage: $ python is_prime.py \"<number> <number> ...\"" );
	
	sys.exit( );
	
for i in range( 0, len( sys.argv[ 1 ] ) ):
	
	try:
	
		n = int( sys.argv[ 1 ][ i ], 10 );
		
	except ValueError:
		
		print( "Usage: $ python is_prime.py \"<number> <number> ...\"" );
	
		sys.exit( );
	
	x = list( filter( lambda x: True if n - ( x * int( n / x ) ) == 0 else False, range( 1, n + 1 ) ) ); # a % b = a - ( b * floor( a / b ) )

	print( "Yes" if len( x ) == 2 else "No, " + "Factors: " + str( x ) );