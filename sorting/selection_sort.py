'''

David Lettier (C) 2014.

http://www.lettier.com/

'''

import random;

a = [ random.randint( 0, 23 ) for i in range( 0, 23 ) ];

print( a, "\n" );

min_index = 0;

for i in range( 0, len( a ) ):
	
	# Step through the array.
	
	print( a[ : i ], a[ i ], a[ i + 1 : ] );
	
	min_index = i;
	
	# Go find the minimum.
	
	for j in range( i + 1, len( a ) ):
		
		if ( a[ j ] < a[ min_index ] ):
			
			min_index = j;
			
	# Swap the current ith position in the array
	# with the minimum found.
			
	if ( min_index != i ):
		
		temp = a[ i ];
		a[ i ] = a[ min_index ];
		a[ min_index ] = temp;
		
print( "\n", a );
	