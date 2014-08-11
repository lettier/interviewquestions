'''

David Lettier (C) 2014.

http://www.lettier.com/

'''

import random;
import math;
import copy;

class Node( object ):
	
	def __init__( self ):
		
		self.parent   = None;
		self.children = { "left": None, "right": None };
		self.data     = None;
		
class Heap( object ):
	
	def __init__( self, root ):
		
		self.root = root or None;
		
	def __heapify( self, node ):
		
		if ( node.parent == None ):
			
			return True;
		
		if ( node.parent.data > node.data ):
			
			# If the node's parent has higher value
			# than itself, swap the data value
			# and bubble up the tree.
			
			temp = node.parent.data;
			
			node.parent.data = node.data;
			
			node.data = temp;
			
			self.__heapify( node.parent );
			
		
	def add_node( self, node ):
		
		if ( self.root == None ):
			
			self.root = node;
			
		else:
			
			# Breadth-first search.
			
			queue = [ ];
			
			queue.append( self.root );
			
			side = "";
			
			while ( len( queue ) != 0 ):
				
				e = queue.pop( 0 );
				
				if ( e.children[ "left" ] != None ):
					
					queue.append( e.children[ "left" ] );
					
				else:
					
					# If this node in the tree has 
					# a free left child, break and add the node.
					
					side = "left";
					
					break;
				
				if ( e.children[ "right" ] != None ):
					
					queue.append( e.children[ "right" ] );
					
				else:
					
					# If this node in the tree has 
					# a free right child, break and add the node.
					
					side = "right";
					
					break;
				
			# Add the node either to the left child or right child
			# of the last node traversed in the tree.
				
			assert side == "left" or side == "right";
					
			e.children[ side ] = node;
					
			node.parent = e;
			
			# Maintain the minimum heap order.
					
			self.__heapify( node );
					
	def __rebuild_heap( self, node ):
		
		if ( node == None or node.data != "" ):
			
			return False;
		
		else:
			# We have a blank node that needs to be filled with a minimum value
			# from its children and eventually we need to remove a leaf node.
			
			if ( node.children[ "left" ] != None ):
				
				if ( node.children[ "right" ] != None ):
					
					# This node has two children.
					# Select the child node with the lowest value.
					
					if ( node.children[ "left" ].data <= node.children[ "right" ].data ):
						
						# Set the blank node's value to its left child's value.
						
						node.data = node.children[ "left" ].data;
						
						# Make the child a blank node.
						
						node.children[ "left" ].data = "";
						
						# Recursively move down the tree.
						
						self.__rebuild_heap( node.children[ "left" ] );
						
					elif ( node.children[ "left" ].data > node.children[ "right" ].data ):
						
						# Set the blank node's value to its right child's value.
						
						node.data = node.children[ "right" ].data;
						
						# Make the child a blank node.
						
						node.children[ "right" ].data = "";
						
						# Recursively move down the tree.
						
						self.__rebuild_heap( node.children[ "right" ] );
						
				else:
					
					# This node only has a right child.
					
					node.data = node.children[ "left" ].data;
					
					# Make the child a blank node.
						
					node.children[ "left" ].data = "";
					
					# Recursively move down the tree.
						
					self.__rebuild_heap( node.children[ "left" ] );
					
			elif ( node.children[ "right" ] != None ):
				
				# This node only has a right child.
					
				node.data = node.children[ "right" ].data;
						
				node.children[ "right" ].data = "";
				
				# Recursively move down the tree.
						
				self.__rebuild_heap( node.children[ "right" ] );
				
			else:
				
				# This node has no children and is a blank node.
				# Remove it from the tree.
				
				if ( node.parent != None ):
					
					# Find out if this is a left child or right child node.
					
					if ( node == node.parent.children[ "left" ] ):
						
						# Destroy the node.
						
						node.parent.children[ "left" ] = None; 
				
					elif ( node == node.parent.children[ "right" ] ):
						
						# Destroy the node.
						
						node.parent.children[ "right" ] = None;
						
					else:
						
						# This node has a parent but it is 
						# neither the left nor right child
						# of its parent.
						
						raise Exception( "__rebuild_heap: node to remove is neither a left or right child of parent node." );
				
				else:
					
					# This node has no parent so it must be the root.
					# Destroy the root.
					
					self.root = None;
				
				return True;	
					
	def extract_min( self ):
		
		if ( self.root == None ):
			
			return False;
		
		else:
			
			minimum = self.root.data;
			
			self.root.data = "";
			
			self.__rebuild_heap( self.root );
			
			return minimum;
		
	def root_peak( self ):
		
		if ( self.root != None ):
		
			return self.root.data;
		
		else:
			
			return False;
					
	def print_heap( self ):
		
		if ( self.root == None ):
			
			return False;
		
		else:
			
			# Breadth-first search.
			
			queue = [ ];
			
			queue.append( self.root );
			
			# Add a star to indicate that a new level of the tree has been reached.
			
			queue.append( "*" );
			
			saw_star = False;
			
			i = 0;
			
			tree = [ ];
			
			tree.append( [ ] );
			
			while ( len( queue ) != 0 ):
				
				e = queue.pop( 0 );
				
				if ( e == "*" and saw_star == False ):
					
					# New level of the tree.
					# Add another list.
					
					tree.append( [ ] );
					
					# Set a flag to not keep removing
					# and adding a star in succession.
					
					saw_star = True;
					
					# Throw the star to the back of the queue.
					
					queue.append( "*" );
					
					# Increase tree level count.
					
					i += 1;
					
				elif ( e != "*" ):
					
					# Append node data to tree level list.
					
					tree[ i ].append( e.data );
					
					saw_star = False;
					
					# Add the left child to the end of the queue.
				
					if ( e.children[ "left" ] != None ):
						
						queue.append( e.children[ "left" ] );
						
					# Add the right child to the end of the queue.
						
					if ( e.children[ "right" ] != None ):
						
						queue.append( e.children[ "right" ] );
						
			# Remove the empty list at the end
			# that was added when we saw the last star.
						
			tree.pop( );
			
			# Assemble the strings that will make up the
			# drawn tree.
			
			strings = [ ];
			
			# Created the template that will be the basis for formatting
			# all of the higher levels of the tree.
					
			b = "*" + "*".join( str( x ) for x in tree[ -1 ] ) + "*";
			
			# If the basis does not fill the size appropriate for its
			# level in the binary tree, fill the template up.
			
			if ( len( tree[ -1 ] ) < ( 2**( len( tree ) - 1 ) ) ):
				
				b += "-*" * ( 2**( len( tree ) - 1 ) - len( tree[ -1 ] ) );
				
			# Begin assembling the strings from the second to the
			# last level of the tree to the root.
			
			for i in range( len( tree ) - 2, -1, -1 ):
				
				# Add a blank string to append to later.
				
				strings.append( "" );
				
				# The number of stars seen so far.
				
				k = -1;
				
				# The current node at the current height in the tree.
				
				l = 0;
				
				# Our current position in the template.
				
				m = -1;
				
				# Advanced through the base template.
				# The spacing for the first node in the current tree level
				# is different than for the rest of the nodes in the level.
				
				for j in range( 0, len( b ) ):
					
					m += 1;
					
					if( b[ j ] == "*" ):
						
						# Increase k as we read a star.
						
						k += 1;
						
					# If number of stars read == 2^(|T|-tree_level-2)...
						
					if ( k == 2**( len( tree ) - i - 2 ) ):
						
						# We found the correct place to render
						# the node.
						
						strings[ -1 ] += str( tree[ i ][ l ] );
						
						# Use the next node in the current tree height.
						
						l += 1;
						
						break;
					
					# Add a space as we traverse through the template.
						
					strings[ -1 ] += " ";
					
				# Reset the number of stars seen.
					
				k = 0;
				
				# Normally we add a single space for every character read 
				# in the template.
				# At other times, we'll add no space if the node is larger
				# than one character.
				
				add_space = 1;
				
				if ( tree[ i ][ l - 1 ] > 9 ):
					
					# If the last node is more than one character wide
					# advance our position in the template by how many
					# characters wide the last node was minus 1.
					
					m += len( str( tree[ i ][ l - 1 ] ) ) - 1;
					
				# Continue iterating through the base template string
				# where we last left off plus one.
				
				# This second loop is needed as the spacing for the rest of the
				# nodes in the current tree height is different than the spacing
				# needed for the first node.
					
				for j in range( m + 1, len( b ) ):
					
					if( b[ j ] == "*" ):
						
						# Read a star.
						
						k += 1;
						
					# If number of stars read == 2^(|T|-tree_height-1)...
						
					if ( k == 2**( len( tree ) - i - 1 ) ):
						
						# Break if we have no more nodes for the current tree height.
						
						if ( l >= len( tree[ i ] ) ):
							
							break;
						
						# Render the node.
						
						strings[ -1 ] += str( tree[ i ][ l ] );
						
						# If the last node rendered is greater than
						# one character wide...
						
						if ( tree[ i ][ l ] > 9 ):
							
							# Reduce the amount of space we add during 
							# the next x iterations by how many characters
							# wide the last node was minus 1.
							# For example: if the last node was 100
							# don't print any spaces for the next
							# two iterations through the template string
							# b.
							
							add_space = 1;
							
							add_space -= len( str( tree[ i ][ l ] ) ) - 1;
							
						# Use the next node in the current tree height.
						
						l += 1;
						
						# Reset the number of stars seen.
						
						k = 0;
						
						# We printed a node so continue.
						
						continue;
					
					# Add space >= zero.
						
					if ( add_space >= 0 ):
					
						strings[ -1 ] += " " * add_space;
						
					# If the number of spaces to add is less
					# than one...
						
					if ( add_space < 1 ):
						
						# Increase the number of spaces we'll add
						# during the next iteration through the template.
						
						add_space += 1;
						
			# Print the strings that make up the tree in reverse.
			# Do not print the bottom level of the tree.
					
			for i in range( len( strings ) - 1, -1, -1 ):
				
				print( strings[ i ] );
				print( "\n" );
				
			# Print the last level of the tree without the template characters.
				
			print( b.replace( "*", " " ).replace( "-", " " ) );
			
a = [ random.randint( 0, 200 ) for i in range( 31, 0, -1 ) ];

print( a, "\n" );
	
heap = Heap( None );	

for i in range( 0, len( a ) ):
	
	node      = Node( );
	node.data = a[ i ];
	
	heap.add_node( node );

	print( "--------------", "\n" );
	
	print( "Adding: ", a[ i ], "\n" );
	
	print( "Heap: \n" );
	
	heap.print_heap( );
	
b = copy.copy( a );

for i in range( 0, len( a ) ):
	
	print( "--------------", "\n" );
	
	print( "Removing: ", heap.root_peak( ), "\n" );
	
	a[ i ] = heap.extract_min( );
	
	print( "Heap: \n" );
	
	heap.print_heap( );	
	
print( "Python sorted: ", sorted( b ) );	
print( "Heap sorted:   ", a );

# Try it again.

a = [ random.randint( 0, 200 ) for i in range( 31, 0, -1 ) ];

print( "\n" );
print( a, "\n" );

for i in range( 0, len( a ) ):
	
	node      = Node( );
	node.data = a[ i ];
	
	heap.add_node( node );
	
heap.print_heap( );
	
b = copy.copy( a );

for i in range( 0, len( a ) ):
	
	a[ i ] = heap.extract_min( );
	
heap.print_heap( ); # Should print nothing as the tree is empty.

print( "\n" );
print( "Python sorted: ", sorted( b ) );	
print( "Heap sorted:   ", a );