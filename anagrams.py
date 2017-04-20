# Technical Questions
# 1. (20 minutes) Write a function that takes in a one-dimensional list or array of words (containing only lowercase letters and whitespace 
#for simplicity) as its single argument and finds out which of those words are anagrams of other words in the list.
								
# For example, when we consider the sample set:
								
# [ 'batman', 'superman', 'clan pipe', 'debit card', 'bad credit', 'apple inc', 'epic plan', 'man bat', 'perma sun', 'the hulk' ]
								
# the output of the function should be something like:
						
# [['debit card', 'bad credit'],
# ['clan pipe', 'apple inc', 'epic plan'], ['batman', 'man bat'],
# ['superman', 'perma sun'],
# ['the hulk']]


#defining the function
def listmyanagrams(example):
#first i created a list of words given and an emtpy dictionary
	# example = ['batman', 'superman' , 'clan pipe' , 'debit card', 'bad credit', 'apple inc', 'epic plan', 'man bat', 'perma sun', 'the hulk' ]
	dic={}

#Here i am looping through every wordd in the list, cleaning it of empty space and sorting the characters
	for word in example:
    		anagram=''.join(sorted(word.replace(' ', '') ))
    # print anagram
    #looping through and creating a dictionary with each sorted and striped item from the list (example) as a key 
    		dic.setdefault(anagram,[])
    #appending the key itself (the 'word') to the values of the dictionary
    		dic[anagram].append(word)

#two options for output below:
#'the hulk' is intentionally there despite not having an anagram as per the output example above

	print 'If we don\'t care about all items on one line:'
	print dic.values()
	print '\nif we care about having them on different lines:'
	for k,v in dic.iteritems():
  	  print '%s' % (v)

  	return

#running the function
example=['batman', 'superman' , 'clan pipe' , 'debit card', 'bad credit', 'apple inc', 'epic plan', 'man bat', 'perma sun', 'the hulk' ]
listmyanagrams(example)


