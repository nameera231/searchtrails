# searchtrails

¬Project Report CSC 151 group project

General Theme
We are writing a program to provide recommendations for trails to hike in the United States based on a set of inputs from the user, including location, distance range of trail from location, hike length, and any or all of the following:
•	Desired trail difficulty
•	Visitor traffic of trail (highest, lowest, or median)
•	Visitor rating of scale (0 to 5 stars)
•	Type of trail (loop, out and back, or point to point)
•	Desired features of trail
•	Desired activities for trail
 
Data Set
National Park Trails on Kaggle, based on information from alltrails.com
We cleaned the data by removing columns 6, 8, and 18 and the first row (which contained titles for the columns)
One row of data (in our cleaned file) contains the following information about a single trail:
•	Trail ID number
•	Trail name
•	National park where trail is located
•	City where trail is located
•	State where trail is located
•	Latitude and longitude of trailhead 
•	Length of trail in meters
•	Trail difficulty (1 to 7)
•	Route type (loop, out and back, or point to point)
•	Average rating on alltrails.com
•	Available features (waterfall, dog-friendly, good views, etc)
•	Available activities (backpacking, camping, hiking, etc)
Columns removed from the cleaned file:
•	Country where trail is located 
•	Popularity of trail
•	“Units” (we are unclear on the meaning of the values in this column)
Components of the program (high-level explanation)
We used a number of different types of procedures to create this program. They are:
1.	Cleaning procedures: These procedures take in one line from the trail-data file and transform data listed as a string into usable lists of numbers and strings by using string->list, filter, list->string, string->num. The three columns we modified were latitude/longitude, features, and activities. The procedure clean-line performs this function in our code.
2.	User-input procedures: These prompt the user for appropriately formatted preferences for their desired trail descriptors by using read in a function with a series of display statements with built-in prompts and error messages. After an input is received, another prompt will appear. This will continue until all inputs are received. The procedure trail-input performs this function in our code. After receiving all the inputs, it puts them into a vector named input-vec for future retrieval, so we can pass them as parameters to our score-assignment procedures.
3.	Filtering procedures: These filter the table based on limits provided by the user. The procedure limit-distance does this in our code. It limits the trails that a user can receive as a response to only trails that fall within the range they’ve specified based on information about their latitude and longitude (extracted from the us-zip-codes table using the user’s provided zip code) and the latitude and longitude of each trail (turned from a string into a list of usable numbers with clean-line). 
4.	Score-assignment procedures: These procedures use conditionals to assign scores of varying amounts to each trail in the table based on how well they match the user’s input. Trails that best match the user’s preferences get the highest scores. Assignfeaturesscore, assignactivitiesscore, assigndifficultyscore, assignratingscore, assigntypescore, and assigntrafficscore will generally add 100 to the score of a given trail if its description matches the user’s preferences (sometimes 50 or 25 is added instead for slightly worse matches). Assignlengthscore creates an initial score and adds it as a new element to the beginning of the list of trail information, while the other assign procedures just add more numbers to that created score. The code works that way because we decided that length should be a required input, while the others could be optional for the user. Assignlengthscore’s intial scores are rather arbitrary in their magnitude but determined by the percent difference between the desired length and the actual length of a given trail. 
5.	Combinatory procedures: These combine other procedures in order to create code that works more smoothly with minimal required input from the user. Trailmaster-3000 serves this purpose in our code. This procedure uses vector-ref to get individual data from the vector of user inputs, then uses them as the relevant inputs for limit-distance and all of the assign procedures to produce the final list of names of the five trails that best suit the user’s preferences.
6.	Search procedure: the procedure trailmasterdirectory allows the user to gain further information about their recommended trails by searching by the trail’s name.
 
Result
The procedure will return the five trails most-suited to the user’s preferences.
