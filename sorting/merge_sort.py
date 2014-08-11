#!/usr/bin/python

'''

David Lettier (C) 2014.

http://www.lettier.com/

'''

import random;

a = [ random.randint( -20, 20 ) for i in range( 0, 23 ) ];

print( a, "\n" );

def merge( left, right ):
	
	result = [ ];
	
	while ( len( left ) > 0 or len( right ) > 0 ):
		
		if ( len( left ) > 0 and len( right ) > 0 ):
			
			if ( left[ 0 ] >= right[ 0 ] ):
				
				result.append( right[ 0 ] );
				
				right.pop( 0 );
				
			elif ( left[ 0 ] < right[ 0 ] ):
				
				result.append( left[ 0 ] );
				
				left.pop( 0 );
				
		elif ( len( left ) > 0 ):
			
			result.append( left[ 0 ] );
			
			left.pop( 0 );
			
		elif ( len( right ) > 0 ):
			
			result.append( right[ 0 ] );
			
			right.pop( 0 );
			
	return result;

def merge_sort( a ):
	
	if ( len( a ) <= 1 ):
		
		return a;
	
	middle = int( len( a ) / 2 );
	
	left   = [ ];
	right  = [ ];
	result = [ ];
	
	for i in range( 0, middle ):
		
		left.append( a[ i ] );
		
	for i in range( middle, len( a ) ):
		
		right.append( a[ i ] );
		
	left  = merge_sort( left );
	right = merge_sort( right );
	
	result = merge( left, right );
	
	return result;

print( merge_sort( a ) );