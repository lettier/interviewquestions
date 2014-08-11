#!/usr/bin/python

'''

David Lettier (C) 2014.
http://www.lettier.com/

Given the provided hash function and the number 945924806726376,
find the string that when hashed, gives the provided number.

'''

# The provided hash function.

def hash( s ):
	
	h = 7;
	
	letters = "acdegilmnoprstuw";
	
	for i in s:
		
		h = ( h * 37 + letters.find( i ) );
		
	print( "Hash: ", h );
		
	return h;

def reverse_hash( n, x ):
	
	# The variables to the equation we must find:
	# A(x-1) = n - 7 * 37^x - sum_(i=0)^(x-2)[ A(i) * 37^(x - i - 1) ]
	
	parameters = [ 0 ] * x;
	
	# Loop through the variables from 0 to the length of the hashed string - 1.
	
	for i in range( 0, len( parameters ) ):
		
		# Loop through the length of the search string.
		
		for j in range( 15, -1, -1 ):
			
			# Search value starting from 15 to 0.
			
			parameters[ i ] = j;
			
			# Beginning of the equation.
			
			parameters[ -1 ] = n - 7 * ( 37**x );
			
			for k in range( 0, x - 1 ):
				
				# The rest of the equation.
				
				parameters[ -1 ] -= parameters[ k ] * ( 37**( x - k - 1 ) );
				
			# Have we found a sufficiently large enough coefficient?
			
			if ( parameters[ -1 ] >= 0 ):
				
				# Yes. Continue on to the next variable in the equation.
				
				break;
			
	# Assemble the string from the found variable values.
	
	letters = "acdegilmnoprstuw";
	
	s = "";
	
	for i in range( 0, x ):
		
		s += letters[ parameters[ i ] ];
		
	print( "Reverse Hash: ", s );
	
	return s;

# Test cases.
	
assert reverse_hash( hash( "promenade"       ), len( "promenade"       ) ) == "promenade";

assert reverse_hash( hash( "leepadg"         ), len( "leepadg"         ) ) == "leepadg";

assert reverse_hash( hash( "lettier"         ), len( "lettier"         ) ) == "lettier";

assert reverse_hash( hash( "promenade"[::-1] ), len( "promenade"[::-1] ) ) == "promenade"[::-1];

assert hash( reverse_hash( 680131659347,    7 ) ) == 680131659347;
assert hash( reverse_hash( 945924806726376, 9 ) ) == 945924806726376;