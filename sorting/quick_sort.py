#!/usr/bin/python

'''

David Lettier (C) 2014.

http://www.lettier.com/

'''

import random;

a = [ random.randint( 0, 23 ) for i in range( 0, 23 ) ];

print( a, "\n" );

def partition( a, l, r ):
	
	if ( l == r ):
		
		return l;
	
	# Select a random pivot point.
	
	pivot_index = random.randint( l, r );
	
	# Swap the element at the pivot point
	# with the element at the right of the
	# array slice.
	
	pivot_value = a[ pivot_index ];
	a[ pivot_index ] = a[ r ];
	a[ r ] = pivot_value;
	
	# The wall index is the point where all elements
	# left of the wall are less than the pivot value
	# and all elements right of the wall are greater 
	# than the pivot value.
	
	wall_index = l;
	
	# Step through the array.
	
	for i in range( l, r + 1 ):
		
		# If the current element value
		# is less than the pivot value 
		# (currently placed at the right side
		# of the array slice) then swap the current
		# element value with the value at the wall
		# index.
		# Move the wall up one in the array.
		
		#  9  1 2 3 0 1 5 3 [ 3 ]
		# w/i
		
		#  9  1 2 3 0 1 5 3 [ 3 ]
		#  w  i
		
		#  1  9  2 3 0 1 5 3 [ 3 ]
		#    w/i
		
		#  1  9  2 3 0 1 5 3 [ 3 ]
		#     w  i
		
		#  1  2  9 3 0 1 5 3 [ 3 ]
		#        w i
		
		#  1  2  9 3 0 1 5 3 [ 3 ]
		#        w   i
		
		#  1  2  0 3 9 1 5 3 [ 3 ]
		#        w   i
		
		#  1  2  0 3 9 1 5 3 [ 3 ]
		#          w i
		
		# ...
		
		if ( a[ i ] < pivot_value ):
			
			temp = a[ wall_index ];
			a[ wall_index ] = a[ i ];
			a[ i ] = temp;
			
			wall_index += 1;
			
	# Now place the pivot value at the wall index.
	# And move the wall index value to the far 
	# right side of the array slice.
			
	temp = a[ wall_index ];
	a[ wall_index ] = pivot_value;
	a[ r ] = temp;
	
	return wall_index;
		

def quick_sort( a, l, r ):
	
	# If left if >= right, do nothing.
	
	if ( l < r ):
		
		# Partition the array such that all values
		# < pivot value are to to the left of the pivot
		# and all values > pivot value are to the right
		# of the pivot value.
		# Get the pivot index.
	
		pivot_index = partition( a, l, r );
		
		print( a[ : l ], a[ pivot_index ], a[ pivot_index + 2 : r ] );
		
		quick_sort( a, l, pivot_index - 1 );
		quick_sort( a, pivot_index + 1, r );
	
quick_sort( a, 0, len( a ) - 1 );
	
print( "\n", a );