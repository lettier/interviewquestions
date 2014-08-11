#!/usr/bin/python

'''

David Lettier (C) 2014.

http://www.lettier.com/

'''

import random;

a = [ random.randint( 0, 23 ) for i in range( 0, 23 ) ];

print( a, "\n" );

for i in range( 1, len( a ) ):
	
	print( a[ : i ], a[ i ], a[ i + 1 : ] );
	
	# One element in the array is sorted.
	
	j = i;
	
	while j > 0 and a[ j - 1 ] > a[ j ]:
		
		# If j is not zero and the element at j
		# is less than the element left before it,
		# at j - 1, swap the element just before 
		# the jth element with the jth element.
		
		temp = a[ j - 1 ];
		a[ j - 1 ] = a[ j ];
		a[ j ] = temp;
		
		# Go further left down the array.
		
		j -= 1;

print( "\n", a );