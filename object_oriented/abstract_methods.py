#!/usr/bin/python

'''

David Lettier (C) 2014.

http://www.lettier.com/

'''

import abc;

class Animal( object, metaclass = abc.ABCMeta ):
	
	@abc.abstractmethod
	def speak( self ):
		
		print( "I'm an animal." );
		
class Cat( Animal ):
	
	def __init__( self, color = "brown" ):
		
		self.name  = "cat";
		self.color = color;
		
	def speak( self ):
		
		super( Cat, self ).speak( );
		
		print( "But more specifically a " + self.name + "! My color is " + self.color + "." );
		
class Alley_Cat( Cat, metaclass = abc.ABCMeta ):
	
	def speak( self ):
		
		print( "I am an alley cat." );
	
	@abc.abstractmethod
	def hide( self ):
		
		print(  "I'm hidden now." );
		
class Calico_Cat( object ):
	
	class Wrapper( Alley_Cat ):
		
		def hide( self ):

			super( Calico_Cat.Wrapper, self ).hide( );	
	
	def __init__( self ):
		
		self.alley_cat = self.Wrapper( );
		
	def speak( self ):
		
		self.alley_cat.speak( );
		
		print( "...and I am calico!" );
		
	def hide( self ):
		
		self.alley_cat.hide( );
		
		print( "Can you see me?" );
	
cat = Cat( "green" );

cat.speak( );
	
calico_cat = Calico_Cat( );

calico_cat.speak( );

calico_cat.hide( );	