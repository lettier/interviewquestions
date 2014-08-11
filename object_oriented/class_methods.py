#!/usr/bin/python

'''

David Lettier (C) 2014.

http://www.lettier.com/

'''

import copy;

class Shape( object ):
	
	def __init__( self ):
		
		pass;
	
	@classmethod
	def class_method( cls ):
		
		print( cls );
		
		return cls( );
	
	@staticmethod
	def static_method( ):
		
		print( Shape );
		
		return Shape;
	
	def regular_method( self ):
		
		print( self );
		
		return self;
		
class Rectangle( Shape ):
	
	def __init__( self ):
		
		pass;
	
a = Shape.class_method( );
b = Shape.static_method( );
c = Shape.regular_method( Shape( ) );

d = Rectangle.class_method( );
e = Rectangle.static_method( );
f = Rectangle.regular_method( Rectangle( ) );

g = copy.copy( globals( ) );

for key, value in g.items( ):
		
	print( key, value );

