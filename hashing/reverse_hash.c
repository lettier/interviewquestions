/*
 * 
 * David Lettier (C) 2014.
 * http://www.lettier.com/
 * 
 * Given the provided hash function and the number 945924806726376,
 * find the string that when hashed, gives the provided number.
 * 
 */

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>

int main( int argc, char* argv[ ] )
{
	
	// The provided hash function.
	
	int64_t hash( char* s )
	{

		int64_t h = 7;
		
		char* letters = "acdegilmnoprstuw";
		
		for ( int i = 0; i < strlen( s ); ++i ) 
		{
			
			h = ( h * 37 + ( ( int64_t ) ( strchr( letters, s[ i ] ) - letters ) ) );
			
		}
		
		printf( "Hash: %ld\n", h );
		
		return h;
		
	}
	
	char* reverse_hash( int64_t n, int x )
	{
		
		// The variables to the equation we must find:
		// A(x-1) = n - 7 * 37^x - sum_(i=0)^(x-2)[ A(i) * 37^(x - i - 1) ]
		
		int64_t* parameters = ( int64_t* ) calloc( x, sizeof( int64_t ) );
		
		for ( int i = 0; i < x; ++i )
		{
			
			// Loop through the length of the search string.
			
			for ( int j = 15; j >= 0; --j )
			{
				
				// Search value starting from 15 to 0.
				
				parameters[ i ] = j;
				
				// Beginning of the equation.
				
				parameters[ x - 1 ] = ( int64_t ) ( n - 7 * ( pow( 37, x ) ) );
				
				for ( int k = 0; k < x - 1; ++k )
				{
					
					// The rest of the equation.
					
					parameters[ x - 1 ] -= ( int64_t ) ( parameters[ k ] * pow( 37, ( x - k - 1 ) ) );
					
				}
				
				// Have we found a sufficiently large enough coefficient?
				
				if ( parameters[ x - 1 ] >= 0 )
				{
					
					// Yes. Continue on to the next variable in the equation.
					
					break;
					
				}				
				
			}
			
		}

		// Assemble the string from the found variable values.
		
		char* letters = "acdegilmnoprstuw";
		
		char* s = calloc( x, sizeof( char ) );
		
		for ( int i = 0; i < x; ++i )
		{
			
			s[ i ] = letters[ parameters[ i ] ];
			
		}
		
		printf( "Reverse Hash: %s\n", s );
		
		return s;
		
	}
	
	// Test cases.
	
	reverse_hash( hash( "promenade" ), strlen( "promenade" ) );	
	reverse_hash( hash( "leepadg" ),   strlen( "leepadg"   ) );
	
	hash( reverse_hash( 680131659347,    7 ) );
	hash( reverse_hash( 945924806726376, 9 ) );
	
	assert( strcmp( reverse_hash( 680131659347,    7 ), "leepadg"   ) == 0 );
	assert( strcmp( reverse_hash( 945924806726376, 9 ), "promenade" ) == 0 );
	
	return 0;
	
}