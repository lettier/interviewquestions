#!/usr/bin/python

'''

David Lettier (C) 2014.

http://www.lettier.com/

'''

import random;

a = [ random.randint( 0, 23 ) for i in range( 0, 23 ) ];

print( a, "\n" );

# Find the max element in a.
# We need a new array of this size.

m = max( a );

print( "Max:", m, "\n" );

counts = [ 0 ] * ( m + 1 );

# Step through the array and count 
# the number of times an element in a
# has been seen.

for i in range( 0, len( a ) ):
	
	counts[ a[ i ] ] += 1;
	
print( "Counts:       ", counts );

# Created an output array of equal size to a.
	
output = [ -1 ] * len( a );

# Create a prefix sum array from the counts.

for i in range( 1, len( counts ) ):
	
	counts[ i ] = counts[ i ] + counts[ i - 1 ];
	
print( "Pre-fix sums: ", counts, "\n" );

# Generate the output sorted array.
# Go from right to left down a.

for i in range( len( a ) - 1, -1, -1 ):
	
	# If a count is not zero.
	
	if ( counts[ a[ i ] ] != 0 ):
		
		# Minus the count by one.
		
		counts[ a[ i ] ] -= 1;
		
	# The value at element i in a is the
	# key to counts. The value in counts
	# is the key to the output array.
	
	# Say a[ i ] is 3 and was seen only one time.
	# Its prefix sum is counts[ 0 ] + counts[ 1 ] + counts[ 2 ] + counts[ 3 ] = 4
	# So counts[ 3 ] = 3 since we subtracted one.
	# We know that three numbers come before 3 (4-1=3).
	# Maybe 0,0,0 or 1,1,1 or 2,2,2 or 0,1,1 or 0,1,2 ... 
	# (Seq.: A001700) C(2n+1,n+1)=C(2*2+1,2+1)=C(5,3)=10 combinations before 3.
	
	# 1 1 1 = 3  nodes.  Tree level 1.
	# 3 2 1 = 6  nodes.  Tree level 2.
	# 6 3 1 = 10 leaves. Tree level 3.
	
	# Thus we put 3 in output[ 3 ].
		
	output[ counts[ a[ i ] ] ] = a[ i ];
	
print( "Python sorted:   ", sorted( a ) );
	
print( "Counting sorted: ", output );
	
	