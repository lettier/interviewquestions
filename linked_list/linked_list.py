#!/usr/bin/py

'''

David Lettier (C) 2014.

http://www.lettier.com/

'''

'''

Linked list.

'''

class Link( object ):
	
	def __init__( self, data ):
		
		self.data = data;
		self.nxt = None;
		
	
class Linked_List( object ):
	
	def __init__( self, head ):
		
		self.head = head;
		
	def add_link( self, link ):
		
		# From the head, follow along until there are no more links.
		# When there is no more links, add the link.
		
		if ( self.head.nxt == None ):
			
			self.head.nxt = link;
			
			return False;
		
		next_link = self.head.nxt;
		
		while ( next_link.nxt != None ):
			
			next_link = next_link.nxt;
			
		next_link.nxt = link;
		
	def remove_link( self, number ):
		
		# Keep a pointer one behind the current pointer.
		# When the current pointer reaches the number.
		# Set the previous pointer's next to the current
		# pointer's next.
		
		if ( self.head == None ):
			
			return False;
		
		elif ( number == 0 ):
			
			self.head = self.head.nxt;
			
			return True;
		
		else:
		
			current_link  = self.head.nxt;
			previous_link = self.head;
			
			if ( current_link != None ):
				
				i = 1;
				
				while ( i < number ):
					
					if ( current_link.nxt != None ):
					
						previous_link = previous_link.nxt;
						current_link  = current_link.nxt;
					
						i += 1;
						
					else:
						
						return False;
					
				previous_link.nxt = current_link.nxt;
				
			else:
				
				return false;				
				
		
	def print_links( self ):
		
		# Run through the list and print the data for
		# each link.
		
		next_link = self.head
		
		while ( next_link.nxt != None ):
			
			print( next_link.data, end = " " );
			
			next_link = next_link.nxt;
			
		print( next_link.data );
		
	def __reverse( self, link ):
		
		# Follow the links.
		# When you reach the end:
		# 	Set this end link as the head.
		# 	Return the new head.
		# Previous link is the link return by the function.
		# Set the returned previous link's next to the current link
		# in the call stack.
		# Set the current link's next to None.
		# Return the current link which will become the previous link,
		# going up the stack.
		# If there is another link up the chain,
		# this current link's next will become the previous link's next
		# as reverse goes up the stack.
			
		current_link = link;
		
		if ( current_link.nxt == None ):
			
			self.head = current_link;
			
			self.head.nxt = None;
			
			return self.head;
		
		else:
			
			previous_link     = self.__reverse( current_link.nxt );			
			previous_link.nxt = current_link;
			current_link.nxt  = None;			
			
			return current_link;
		
	def reverse_links( self ):		
		
		if ( self.head.nxt == None ):
			
			return False;
		
		else:
			
			self.__reverse( self.head );		
		
head = Link( 0 );

linked_list = Linked_List( head );

for i in range( 1, 23 ):

	linked_list.add_link( Link( i ) );
	
linked_list.print_links( );

linked_list.reverse_links( );

linked_list.print_links( );

linked_list.reverse_links( );

linked_list.print_links( );

linked_list.remove_link( 22 );

linked_list.remove_link( 15 );

linked_list.remove_link( 3 );

linked_list.remove_link( 0 );

linked_list.remove_link( 1 );

linked_list.add_link( Link( 0 ) );

linked_list.print_links( );

print( "\n" );

'''

Double Linked list.

'''

class Double_Link( Link ):
	
	def __init__( self, data ):
		
		super( ).__init__( data );

		self.prv = None;
		
	
class Double_Linked_List( Linked_List ):
		
	def add_link( self, link ):
		
		if ( self.head.nxt == None ):
			
			self.head.nxt = link;
			
			link.prev = self.head;
			
			return False;
		
		next_link = self.head.nxt;
		
		while ( next_link.nxt != None ):
			
			next_link = next_link.nxt;
			
		next_link.nxt = link;
		
		link.prev = next_link;
		
	def remove_link( self, number ):
		
		# Follow the links.
		# When you reach the numbered link,
		# Set its previous link's next to its
		# next.
		
		if ( self.head == None ):
			
			return False;
		
		elif ( number == 0 ):
			
			self.head = self.head.nxt;
			self.head.prev = None;
			
		else:
			
			i = 1;
			
			current_link = self.head.nxt;
			
			while ( i < number ):
				
				if ( current_link.nxt != None ):
				
					current_link = current_link.nxt;
					
				else:
					
					return False;
				
				i += 1
				
			current_link.prv.nxt = current_link.nxt;
		
	def __reverse( self, link ):
		
		# Follow the links.
		# When you reach the end,
		# this is the new head.
		# Return this link.
		# Set the current link, in the stack, previous
		# to the returned previous link.
		# Set the current link, in the stack, next to None.
		# Set the returned previous link's next to the current
		# link in the call stack.
		# Return the current link which will be the previous link
		# in the next rung up in the call stack.
			
		current_link = link;
		
		if ( current_link.nxt == None ):
			
			self.head = current_link;
			
			self.head.nxt = None;
			
			self.head.prv = None;
			
			return self.head;
		
		else:
			
			previous_link     = self.__reverse( current_link.nxt );
			current_link.prv = previous_link;
			current_link.nxt  = None;
			previous_link.nxt = current_link;						
			
			return current_link;
		
	def reverse_links( self ):		
		
		if ( self.head.nxt == None ):
			
			return False;
		
		else:
			
			self.__reverse( self.head );
			
double_head = Double_Link( 0 );

double_linked_list = Double_Linked_List( double_head );

for i in range( 1, 23 ):

	double_linked_list.add_link( Double_Link( i ) );
	
double_linked_list.print_links( );

double_linked_list.reverse_links( );

double_linked_list.print_links( );

double_linked_list.reverse_links( );

double_linked_list.print_links( );

double_linked_list.remove_link( 22 );

double_linked_list.remove_link( 15 );

double_linked_list.remove_link( 3 );

double_linked_list.remove_link( 0 );

double_linked_list.remove_link( 1 );

double_linked_list.add_link( Link( 0 ) );

double_linked_list.print_links( );