;;; auto-complete-processing.el --- Auto-complete sources for processing  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Rolando Pereira

;; Author: Rolando Pereira <rolando_pereira@sapo.pt>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'auto-complete)
(require 'shr)

(defvar auto-complete-processing--auto-complete-data
  '(("FloatDict.add" "Add to a value. If the key does not exist, an new pair is initialized with the value supplied.")
     ("FloatDict.clear" "Remove all entries from the data structure.")
     ("FloatDict.div" "Divide a value.")
     ("FloatDict.get" "Return a value for the specified key.")
     ("FloatDict.hasKey" "Check if a key is a part of the data structure.")
     ("FloatDict.keyArray" "Return a copy of the internal keys array. In contrast to the <b>keys()</b> 
method, this array can be modified.")
     ("FloatDict.keys" "Return the internal array being used to store the keys.")
     ("FloatDict.mult" "Multiply a value.")
     ("FloatDict.remove" "Remove a key/value pair.")
     ("FloatDict.set" "Create a new key/value pair or change the value of one.")
     ("FloatDict.size" "Returns the number of key/value pairs.")
     ("FloatDict.sortKeys" "Sort the keys alphabetically (ignoring case). Uses the value as 
a tie-breaker (only really possible with a key that has a case change).")
     ("FloatDict.sortKeysReverse" "Sort the keys alphabetically in reverse (ignoring case). Uses the value 
as a tie-breaker (only really possible with a key that has a case change).")
     ("FloatDict.sortValues" "Sort by values in ascending order. The smallest value will be at [0].")
     ("FloatDict.sortValuesReverse" "Sort by values in descending order. The largest value will be at [0].")
     ("FloatDict.sub" "Subtract from a value.")
     ("FloatDict.valueArray" "The version of this method without a parameter creates a new 
array and copies each of the values into it. The version with
the <b>float[]</b> parameters fills an already-allocated array with the 
values (more efficient than creating a new array each time). 
If 'array' is null, or not the same size as the number of values, 
a new array will be allocated and returned.")
     ("FloatDict.values" "Return the internal array being used to store the values.")
     ("FloatList.add" "Add to a value.")
     ("FloatList.append" "Add a new entry to the list.")
     ("FloatList.array" "Create a new array with a copy of all the values.")
     ("FloatList.clear" "Remove all entries from the list.")
     ("FloatList.div" "Divide a value.")
     ("FloatList.get" "Get an entry at a particular index.")
     ("FloatList.hasValue" "Check if a number is a part of the list.")
     ("FloatList.max" "Return the largest value.")
     ("FloatList.min" "Return the smallest value.")
     ("FloatList.mult" "Multiply a value.")
     ("FloatList.remove" "Remove an element from the specified index.")
     ("FloatList.reverse" "Reverse the order of the list.")
     ("FloatList.set" "Set the entry at a particular index.")
     ("FloatList.shuffle" "Randomize the order of the list elements.")
     ("FloatList.size" "Get the length of the list.")
     ("FloatList.sort" "Sorts an array, lowest to highest.")
     ("FloatList.sortReverse" "A sort in reverse. It's equivalent to running <b>sort()</b> and then <b>reverse()</b>, but is more efficient than running each separately.")
     ("FloatList.sub" "Subtract from a value.")
     ("IntDict.add" "Add to a value. If the key does not exist, an new pair is initialized with the value supplied.")
     ("IntDict.clear" "Remove all entries from the data structure.")
     ("IntDict.div" "Divide a value.")
     ("IntDict.get" "Return a value for the specified key.")
     ("IntDict.hasKey" "Check if a key is a part of the data structure.")
     ("IntDict.increment" "Increase the value of a specific key value by 1")
     ("IntDict.keyArray" "Return a copy of the internal keys array. In contrast to the <b>keys()</b> 
method, this array can be modified.")
     ("IntDict.keys" "Return the internal array being used to store the keys.")
     ("IntDict.mult" "Multiply a value.")
     ("IntDict.remove" "Remove a key/value pair.")
     ("IntDict.set" "Create a new key/value pair or change the value of one.")
     ("IntDict.size" "Returns the number of key/value pairs.")
     ("IntDict.sortKeys" "Sort the keys alphabetically (ignoring case). Uses the value as 
a tie-breaker (only really possible with a key that has a case change).")
     ("IntDict.sortKeysReverse" "Sort the keys alphabetically in reverse (ignoring case). Uses the value as 
a tie-breaker (only really possible with a key that has a case change).")
     ("IntDict.sortValues" "Sort by values in ascending order. The smallest value will be at [0].")
     ("IntDict.sortValuesReverse" "Sort by values in descending order. The largest value will be at [0].")
     ("IntDict.sub" "Subtract from a value.")
     ("IntDict.valueArray" "The version of this method without a parameter creates a new 
array and copies each of the values into it. The version with
the <b>int[]</b> parameters fills an already-allocated array with the 
values (more efficient than creating a new array each time). 
If 'array' is null, or not the same size as the number of values, 
a new array will be allocated and returned.")
     ("IntDict.values" "Return the internal array being used to store the values.")
     ("IntList.add" "Add to a value.")
     ("IntList.append" "Add a new entry to the list.")
     ("IntList.array" "Create a new array with a copy of all the values.")
     ("IntList.clear" "Remove all entries from the list.")
     ("IntList.div" "Divide a value.")
     ("IntList.get" "Get an entry at a particular index.")
     ("IntList.hasValue" "Check if a number is a part of the data structure.")
     ("IntList.increment" "Add one to a value.")
     ("IntList.max" "Return the largest value.")
     ("IntList.min" "Return the smallest value.")
     ("IntList.mult" "Multiply a value.")
     ("IntList.remove" "Remove an element from the specified index.")
     ("IntList.reverse" "Reverse the order of the list.")
     ("IntList.set" "Set the entry at a particular index.")
     ("IntList.shuffle" "Randomize the order of the list elements.")
     ("IntList.size" "Get the length of the list.")
     ("IntList.sort" "Sorts the array, lowest to highest")
     ("IntList.sortReverse" "A sort in reverse. It's equivalent to running <b>sort()</b> and then 
<b>reverse()</b>, but is more efficient than running each separately.")
     ("IntList.sub" "Subtract from a value.")
     ("JSONArray.append" "Appends a new value to the <b>JSONArray</b>, increasing the array's length by one. New values may be of the following types: int, float, String, boolean, <b>JSONObject</b>, or <b>JSONArray</b>.")
     ("JSONArray.getBoolean" "Gets the boolean value associated with the specified index.")
     ("JSONArray.getFloat" "Gets the float value associated with the specified index.")
     ("JSONArray.getInt" "Gets the int value associated with the specified index.")
     ("JSONArray.getIntArray" "Returns the entire <b>JSONArray</b> as an array of ints.  (All values in the array must be of the int type.)")
     ("JSONArray.getJSONArray" "Retrieves the <b>JSONArray</b> with the associated index value.")
     ("JSONArray.getJSONObject" "Retrieves the <b>JSONObject</b> with the associated index value.")
     ("JSONArray.getString" "Gets the String value associated with the specified index.")
     ("JSONArray.getStringArray" "Returns the entire <b>JSONArray</b> as an array of Strings.  (All values in the array must be of the String type.)")
     ("JSONArray.remove" "Removes the element from a <b>JSONArray</b> in the specified index position. Returns either the value associated with the given index, or null, if there is no value.")
     ("JSONArray.setBoolean" "Inserts a new value into the <b>JSONArray</b> at the specified index position. If a value already exists in the specified position, the new value overwrites the old value. If the given index is greater than the length of the <b>JSONArray</b>, then null elements will be added as necessary to pad it out.")
     ("JSONArray.setFloat" "Inserts a new value into the <b>JSONArray</b> at the specified index position. If a value already exists in the specified position, the new value overwrites the old value. If the given index is greater than the length of the <b>JSONArray</b>, then null elements will be added as necessary to pad it out.")
     ("JSONArray.setInt" "Inserts a new value into the <b>JSONArray</b> at the specified index position. If a value already exists in the specified position, the new value overwrites the old value. If the given index is greater than the length of the <b>JSONArray</b>, then null elements will be added as necessary to pad it out.")
     ("JSONArray.setJSONArray" "Sets the value of the <b>JSONArray</b> with the associated index value.")
     ("JSONArray.setJSONObject" "Sets the value of the <b>JSONObject</b> with the index value.")
     ("JSONArray.setString" "Inserts a new value into the <b>JSONArray</b> at the specified index position. If a value already exists in the specified position, the new value overwrites the old value. If the given index is greater than the length of the <b>JSONArray</b>, then null elements will be added as necessary to pad it out.")
     ("JSONArray.size" "Gets the total number of elements in a <b>JSONArray</b> (inclusive of null elements).")
     ("JSONObject.getBoolean" "Gets the boolean value associated with the specified key.")
     ("JSONObject.getFloat" "Gets the float value associated with the specified key.")
     ("JSONObject.getInt" "Gets the int value associated with the specified key.")
     ("JSONObject.getJSONArray" "Retrieves the <b>JSONArray</b> with the associated key.")
     ("JSONObject.getJSONObject" "Given a key value, retrieves the associated <b>JSONObject</b>.")
     ("JSONObject.getString" "Gets the String value associated with the specified key.")
     ("JSONObject.setBoolean" "Inserts a new key/boolean pair into the <b>JSONObject</b> or, if a value with the specified key already exists, assigns a new value.")
     ("JSONObject.setFloat" "Inserts a new key/float pair into the <b>JSONObject</b> or, if a value with the specified key already exists, assigns a new value.")
     ("JSONObject.setInt" "Inserts a new key/int pair into the <b>JSONObject</b> or, if a value with the specified key already exists, assigns a new value.")
     ("JSONObject.setJSONArray" "Sets the value of the <b>JSONArray</b> with the associated key.")
     ("JSONObject.setJSONObject" "Sets the value of the <b>JSONObject</b> with the associated key.")
     ("JSONObject.setString" "Inserts a new key/String pair into the <b>JSONObject</b> or, if a value with the specified key already exists, assigns a new value.")
     ("StringDict.clear" "Remove all entries.")
     ("StringDict.get" "Return a value for the specified key.")
     ("StringDict.hasKey" "Check if a key is a part of the data structure.")
     ("StringDict.keyArray" "Return a copy of the internal keys array.")
     ("StringDict.keys" "Return the internal array being used to store the keys.")
     ("StringDict.remove" "Remove a key/value pair.")
     ("StringDict.set" "Create a new key/value pair or change the value of one.")
     ("StringDict.size" "Returns the number of key/value pairs.")
     ("StringDict.sortKeys" "Sort the keys alphabetically.")
     ("StringDict.sortKeysReverse" "Sort the keys alphabetially in reverse.")
     ("StringDict.sortValues" "Sort by values in descending order.")
     ("StringDict.sortValuesReverse" "Sort by values in descending order.")
     ("StringDict.valueArray" "Create a new array and copy each of the values into it.")
     ("StringDict.values" "Return the internal array being used to store the values.")
     ("StringList.append" "Add a new entry to the list.")
     ("StringList.array" "Create a new array with a copy of all the values.")
     ("StringList.clear" "Remove all entries from the list.")
     ("StringList.get" "Get an entry at a particular index.")
     ("StringList.hasValue" "Check if a value is a part of the list.")
     ("StringList.lower" "Make the entire list lower case.")
     ("StringList.remove" "Remove an element from the specified index.")
     ("StringList.reverse" "Reverses the order of the list.")
     ("StringList.set" "Set the entry at a particular index. If the index is past the length of the list, it'll expand the list to accommodate, and fill the intermediate entries with \"null\".")
     ("StringList.shuffle" "Randomize the order of the list elements")
     ("StringList.size" "Get the length of the list.")
     ("StringList.sort" "Sorts the array in place.")
     ("StringList.sortReverse" "A sort in reverse. It's equivalent to running <b>sort()</b> and then <b>reverse()</b>, but is more efficient than running each separately.")
     ("StringList.upper" "Make the entire list upper case.")
     ("TableRow.getFloat" "Retrieves a float value from the <b>TableRow</b>'s specified column. The column may be specified by either its ID or title.")
     ("TableRow.getInt" "Retrieves an integer value from the <b>TableRow</b>'s specified column. The column may be specified by either its ID or title.")
     ("TableRow.getString" "Retrieves a String value from the <b>TableRow</b>'s specified column. The column may be specified by either its ID or title.")
     ("TableRow.setFloat" "Stores a float value in the <b>TableRow</b>'s specified column. The column may be specified by either its ID or title.")
     ("TableRow.setInt" "Stores an integer value in the <b>TableRow</b>'s specified column. The column may be specified by either its ID or title.")
     ("TableRow.setString" "Stores a String value in the <b>TableRow</b>'s specified column. The column may be specified by either its ID or title.")
     ("Table.addColumn" "Use <b>addColumn()</b> to add a new column to a <b>Table</b> object.  Typically, you will want to specify a title, so the column may be easily referenced later by name.  (If no title is specified, the new column's title will be null.)  A column type may also be specified, in which case all values stored in this column must be of the same type (e.g., Table.INT or Table.FLOAT).  If no type is specified, the default type of STRING is used.")
     ("Table.addRow" "Use <b>addRow()</b> to add a new row of data to a <b>Table</b> object.  By default, an empty row is created.  Typically, you would store a reference to the new row in a <b>TableRow</b> object (see <b>newRow</b> in the example above), and then set individual values using <b>setInt()</b>, <b>setFloat()</b>, or <b>setString()</b>.  If a <b>TableRow</b> object is included as a parameter, then that row is duplicated and added to the table.")
     ("Table.clearRows" "Removes all rows from a <b>Table</b>.  While all rows are removed, columns and column titles are maintained.")
     ("Table.findRow" "Finds the first row in the <b>Table</b> that contains the value provided, and returns a reference to that row.  Even if multiple rows are possible matches, only the first matching row is returned. The column to search may be specified by either its ID or title.")
     ("Table.findRows" "Finds the rows in the <b>Table</b> that contain the value provided, and returns references to those rows.  Returns an iterator, so <b>for</b> must be used to iterate through all the rows, as shown in the example above. The column to search may be specified by either its ID or title.")
     ("Table.getColumnCount" "Returns the total number of columns in a table.")
     ("Table.getFloat" "Retrieves a float value from the <b>Table</b>'s specified row and column. The row is specified by its ID, while the column may be specified by either its ID or title.")
     ("Table.getInt" "Retrieves an integer value from the <b>Table</b>'s specified row and column. The row is specified by its ID, while the column may be specified by either its ID or title.")
     ("Table.getRow" "Returns a reference to the specified <b>TableRow</b>.  The reference can then be used to get and set values of the selected row, as illustrated in the example above.")
     ("Table.getRowCount" "Returns the total number of rows in a table.")
     ("Table.getString" "Retrieves a String value from the <b>Table</b>'s specified row and column. The row is specified by its ID, while the column may be specified by either its ID or title.")
     ("Table.getStringColumn" "Retrieves all values in the specified column, and returns them as a String array.  The column may be specified by either its ID or title.")
     ("Table.matchRow" "Finds the first row in the <b>Table</b> that matches the regular expression provided, and returns a reference to that row.  Even if multiple rows are possible matches, only the first matching row is returned. The column to search may be specified by either its ID or title.")
     ("Table.matchRows" "Finds the rows in the <b>Table</b> that match the regular expression provided, and returns references to those rows.  Returns an iterator, so <b>for</b> must be used to iterate through all the rows, as shown in the example above. The column to search may be specified by either its ID or title.")
     ("Table.removeColumn" "Use <b>removeColumn()</b> to remove an existing column from a <b>Table</b> object.  The column to be removed may be identified by either its title (a String) or its index value (an int).   <b>removeColumn(0)</b> would remove the first column, <b>removeColumn(1)</b> would remove the second column, and so on.")
     ("Table.removeRow" "Removes a row from a <b>Table</b> object.")
     ("Table.removeTokens" "Removes any of the specified characters (or \"tokens\").  The example above removes all commas, dollar signs, and spaces from the table.<br>
<br>
If no column is specified, then the values in all columns and rows are processed.  A specific column may be referenced by either its ID or title.")
     ("Table.rows" "Gets all rows from the table.  Returns an iterator, so <b>for</b> must be used to iterate through all the rows, as shown in the example above.")
     ("Table.setFloat" "Stores a float value in the <b>Table</b>'s specified row and column. The row is specified by its ID, while the column may be specified by either its ID or title.")
     ("Table.setInt" "Stores an integer value in the <b>Table</b>'s specified row and column. The row is specified by its ID, while the column may be specified by either its ID or title.")
     ("Table.setString" "Stores a String value in the <b>Table</b>'s specified row and column. The row is specified by its ID, while the column may be specified by either its ID or title.")
     ("Table.trim" "Trims leading and trailing whitespace, such as spaces and tabs, from String table values.  If no column is specified, then the values in all columns and rows are trimmed.  A specific column may be referenced by either its ID or title.")
     ("HALF_PI" "HALF_PI is a mathematical constant with the value 1.57079632679489661923. It is half the ratio of the circumference of a circle to its diameter. It is useful in combination with the trigonometric functions <b>sin()</b> and <b>cos()</b>.")
     ("PI" "PI is a mathematical constant with the value 3.14159265358979323846. It is the ratio of the circumference of a circle to its diameter. It is useful in combination with the trigonometric functions <b>sin()</b> and <b>cos()</b>.")
     ("QUARTER_PI" "QUARTER_PI is a mathematical constant with the value 0.7853982. It is one quarter the ratio of the circumference of a circle to its diameter. It is useful in combination with the trigonometric functions <b>sin()</b> and <b>cos()</b>.")
     ("TAU" "TAU is an alias for TWO_PI, a mathematical constant with the value 6.28318530717958647693. It is twice the ratio of the circumference of a circle to its diameter. It is useful in combination with the trigonometric functions <b>sin()</b> and <b>cos()</b>.")
     ("TWO_PI" "TWO_PI is a mathematical constant with the value 6.28318530717958647693. It is twice the ratio of the circumference of a circle to its diameter. It is useful in combination with the trigonometric functions <b>sin()</b> and <b>cos()</b>.")

     ("displayHeight" "System variable that stores the height of the entire screen display. This is used to run a full-screen program on any display size.")
     ("focused" "Confirms if a Processing program is \"focused,\" meaning that it is active and will accept mouse or keyboard input. This variable is \"true\" if it is focused and \"false\" if not.")
     ("frameCount" "The system variable <b>frameCount</b> contains the number of frames that have been displayed since the program started. Inside <b>setup()</b> the value is 0, after the first iteration of draw it is 1, etc.")
     ("height" "System variable that stores the height of the display window. This value is set by the second parameter of the <b>size()</b> function. For example, the function call <b>size(320, 240)</b> sets the <b>height</b> variable to the value 240. The value of <b>height</b> defaults to 100 if <b>size()</b> is not used in a program.")
     ("key" "The system variable <b>key</b> always contains the value of the most recent key on the keyboard that was used (either pressed or released). 
<br/> <br/>
For non-ASCII keys, use the <b>keyCode</b> variable. The keys included in the ASCII specification (BACKSPACE, TAB, ENTER, RETURN, ESC, and DELETE) do not require checking to see if they key is coded, and you should simply use the <b>key</b> variable instead of <b>keyCode</b> If you're making cross-platform projects, note that the ENTER key is commonly used on PCs and Unix and the RETURN key is used instead on Macintosh. Check for both ENTER and RETURN to make sure your program will work for all platforms.")
     ("keyCode" "The variable <b>keyCode</b> is used to detect special keys such as the UP, DOWN, LEFT, RIGHT arrow keys and ALT, CONTROL, SHIFT. When checking for these keys, it's first necessary to check and see if the key is coded. This is done with the conditional \"if (key == CODED)\" as shown in the example. 
<br/> <br/>
The keys included in the ASCII specification (BACKSPACE, TAB, ENTER, RETURN, ESC, and DELETE) do not require checking to see if they key is coded, and you should simply use the <b>key</b> variable instead of <b>keyCode</b> If you're making cross-platform projects, note that the ENTER key is commonly used on PCs and Unix and the RETURN key is used instead on Macintosh. Check for both ENTER and RETURN to make sure your program will work for all platforms.
<br/> <br/>
For users familiar with Java, the values for UP and DOWN are simply shorter versions of Java's KeyEvent.VK_UP and KeyEvent.VK_DOWN. Other keyCode values can be found in the Java <a href=\"http://download.oracle.com/javase/6/docs/api/java/awt/event/KeyEvent.html\">KeyEvent</a> reference.")
     ("keyPressed" "The boolean system variable <b>keyPressed</b> is <b>true</b> if any key is pressed and <b>false</b> if no keys are pressed.")
     ("mouseButton" "When a mouse button is pressed, the value of the system variable <b>mouseButton</b> is set to either <b>LEFT</b>, <b>RIGHT</b>, or <b>CENTER</b>, depending on which button is pressed. (If no button is pressed, <b>mouseButton</b> may be reset to <b>0</b>. For that reason, it's best to use <b>mousePressed</b> first to test if any button is being pressed, and only then test the value of <b>mouseButton</b>, as shown in the examples above.)")
     ("mousePressed" "The <b>mousePressed</b> variable stores whether or not a mouse button is currently being pressed. The value is true when <i>any</i> mouse button is pressed, and false if no button is pressed. The <b>mouseButton</b> variable (see the related reference entry) can be used to determine which button has been pressed.")
     ("mouseX" "The system variable <b>mouseX</b> always contains the current horizontal coordinate of the mouse.
<br><br>
Note that Processing can only track the mouse position when the pointer is over the current window. The default value of <b>mouseX</b> is <b>0</b>, so <b>0</b> will be returned until the mouse moves in front of the sketch window. (This typically happens when a sketch is first run.)  Once the mouse moves away from the window, <b>mouseX</b> will continue to report its most recent position.")
     ("mouseY" "The system variable <b>mouseY</b> always contains the current vertical coordinate of the mouse.
<br><br>
Note that Processing can only track the mouse position when the pointer is over the current window. The default value of <b>mouseY</b> is <b>0</b>, so <b>0</b> will be returned until the mouse moves in front of the sketch window. (This typically happens when a sketch is first run.)  Once the mouse moves away from the window, <b>mouseY</b> will continue to report its most recent position.")
     ("pixels[]" "Array containing the values for all the pixels in the display window. These values are of the color datatype. This array is the size of the display window. For example, if the image is 100x100 pixels, there will be 10000 values and if the window is 200x300 pixels, there will be 60000 values. The <b>index</b> value defines the position of a value within the array. For example, the statement <b>color b = pixels[230]</b> will set the variable <b>b</b> to be equal to the value at that location in the array.<br />
<br />
Before accessing this array, the data must loaded with the <b>loadPixels()</b> function. After the array data has been modified, the <b>updatePixels()</b> function must be run to update the changes. Without <b>loadPixels()</b>, running the code may (or will in future releases) result in a NullPointerException.")
     ("pmouseX" "The system variable <b>pmouseX</b> always contains the horizontal position of the mouse in the frame previous to the current frame.
<br><br>
You may find that <b>pmouseX</b> and <b>pmouseY</b> have different values when referenced inside of <b>draw()</b> and inside of mouse events like <b>mousePressed()</b> and <b>mouseMoved()</b>. Inside <b>draw()</b>, <b>pmouseX</b> and <b>pmouseY</b> update only once per frame (once per trip through the <b>draw()</b> loop). But inside mouse events, they update each time the event is called. If these values weren't updated immediately during mouse events, then the mouse position would be read only once per frame, resulting in slight delays and choppy interaction. If the mouse variables were always updated multiple times per frame, then something like <b>line(pmouseX, pmouseY, mouseX, mouseY)</b> inside <b>draw()</b> would have lots of gaps, because <b>pmouseX</b> may have changed several times in between the calls to <b>line()</b>.<br><br>
If you want values relative to the previous frame, use <b>pmouseX</b> and <b>pmouseY</b> inside <b>draw()</b>. If you want continuous response, use <b>pmouseX</b> and <b>pmouseY</b> inside the mouse event functions.")
     ("pmouseY" "The system variable <b>pmouseY</b> always contains the vertical position of the mouse in the frame previous to the current frame.
<br><br>
For more detail on how <b>pmouseY</b> is updated inside of mouse events and <b>draw()</b>, see the reference for <b>pmouseX</b>.")
     ("draw()" "Called directly after <b>setup()</b>, the <b>draw()</b> function continuously executes the lines of code contained inside its block until the program is stopped or <b>noLoop()</b> is called. <b>draw()</b> is called automatically and should never be called explicitly.<br/>
<br/>
It should always be controlled with <b>noLoop()</b>, <b>redraw()</b> and <b>loop()</b>. If <b>noLoop()</b> is used to stop the code in <b>draw()</b> from executing, then <b>redraw()</b> will cause the code inside <b>draw()</b> to be executed a single time, and <b>loop()</b> will cause the code inside <b>draw()</b> to resume executing continuously.<br/>
<br/>
The number of times <b>draw()</b> executes in each second may be controlled with the <b>frameRate()</b> function.<br/>
<br/>
It is common to call <b>background()</b> near the beginning of the <b>draw()</b> loop to clear the contents of the window, as shown in the first example above.  Since pixels drawn to the window are cumulative, omitting <b>background()</b> may result in unintended results, especially when drawing anti-aliased shapes or text.
<br/><br/>
There can only be one <b>draw()</b> function for each sketch, and <b>draw()</b> must exist if you want the code to run continuously, or to process events such as <b>mousePressed()</b>. Sometimes, you might have an empty call to <b>draw()</b> in your program, as shown in the second example above.")
     ("keyPressed()" "The <b>keyPressed()</b> function is called once every time a key is pressed. The key that was pressed is stored in the <b>key</b> variable. 
<br/> <br/>
For non-ASCII keys, use the <b>keyCode</b> variable. The keys included in the ASCII specification (BACKSPACE, TAB, ENTER, RETURN, ESC, and DELETE) do not require checking to see if they key is coded, and you should simply use the <b>key</b> variable instead of <b>keyCode</b> If you're making cross-platform projects, note that the ENTER key is commonly used on PCs and Unix and the RETURN key is used instead on Macintosh. Check for both ENTER and RETURN to make sure your program will work for all platforms.
<br/> <br/>
Because of how operating systems handle key repeats, holding down a key may cause multiple calls to keyPressed() (and keyReleased() as well). The rate of repeat is set by the operating system and how each computer is configured.
<br/><br/>
Mouse and keyboard events only work when a program has <b>draw()</b>. Without <b>draw()</b>, the code is only run once and then stops listening for events.")
     ("keyReleased()" "The <b>keyReleased()</b> function is called once every time a key is released. The key that was released will be stored in the <b>key</b> variable. See <b>key</b> and <b>keyReleased</b> for more information.
<br/><br/>
Mouse and keyboard events only work when a program has <b>draw()</b>. Without <b>draw()</b>, the code is only run once and then stops listening for events.")
     ("keyTyped()" "The <b>keyTyped()</b> function is called once every time a key is pressed, but action keys such as Ctrl, Shift, and Alt are ignored. Because of how operating systems handle key repeats, holding down a key will cause multiple calls to <b>keyTyped()</b>, the rate is set by the operating system and how each computer is configured. 
<br/><br/>
Mouse and keyboard events only work when a program has <b>draw()</b>. Without <b>draw()</b>, the code is only run once and then stops listening for events.")
     ("mouseClicked()" "The <b>mouseClicked()</b> function is called <i>after</i> a mouse button has been pressed and then released.
<br/><br/>
Mouse and keyboard events only work when a program has <b>draw()</b>. Without <b>draw()</b>, the code is only run once and then stops listening for events.")
     ("mouseDragged()" "The <b>mouseDragged()</b> function is called once every time the mouse moves while a mouse button is pressed. (If a button <i>is not</i> being pressed, <b>mouseMoved()</b> is called instead.)
<br/><br/>
Mouse and keyboard events only work when a program has <b>draw()</b>. Without <b>draw()</b>, the code is only run once and then stops listening for events.")
     ("mouseMoved()" "The <b>mouseMoved()</b> function is called every time the mouse moves and a mouse button is not pressed. (If a button <i>is</i> being pressed, <b>mouseDragged()</b> is called instead.)
<br/><br/>
Mouse and keyboard events only work when a program has <b>draw()</b>. Without <b>draw()</b>, the code is only run once and then stops listening for events.")
     ("mousePressed()" "The <b>mousePressed()</b> function is called once after every time a mouse button is pressed. The <b>mouseButton</b> variable (see the related reference entry) can be used to determine which button has been pressed.
<br/><br/>
Mouse and keyboard events only work when a program has <b>draw()</b>. Without <b>draw()</b>, the code is only run once and then stops listening for events.")
     ("mouseWheel()" "The <b>mouseWheel()</b> function returns positive values when the mouse wheel is rotated down (toward the user), and negative values for the other direction (up or away from the user). On OS X with \"natural\" scrolling enabled, the values are opposite.
<br/><br/>
Mouse and keyboard events only work when a program has <b>draw()</b>. Without <b>draw()</b>, the code is only run once and then stops listening for events.")
     ("setup()" "The <b>setup()</b> function is called once when the program starts. It's used to define initial
enviroment properties such as screen size and background color and to load media such as images
and fonts as the program starts. There can only be one <b>setup()</b> function for each program and
it shouldn't be called again after its initial execution. Note: Variables declared within
<b>setup()</b> are not accessible within other functions, including <b>draw()</b>.")
     ("PVector.add" "Adds x, y, and z components to a vector, adds one vector to another, or adds two independent vectors together. The version of the method that adds two vectors together is a static method and returns a PVector, the others have no return value -- they act directly on the vector. See the examples for more context.")
     ("PVector.angleBetween" "Calculates and returns the angle (in radians) between two vectors.")
     ("PVector.array" "Return a representation of this vector as a float array. This is only for temporary use. If used in any other fashion, the contents should be copied by using the <b>PVector.get()</b> method to copy into your own array.")
     ("PVector.copy" "Copies the components of the vector and returns the result as a PVector.")
     ("PVector.cross" "Calculates and returns a vector composed of the cross product between two vectors.")
     ("PVector.dist" "Calculates the Euclidean distance between two points (considering a point as a vector object).")
     ("PVector.div" "Divides a vector by a scalar.  The version of the method that uses a float acts directly on the vector upon which it is called (as in the first example above), and therefore has no return value.  The versions that receive both a PVector and a float as arugments are static methods, and each returns a new PVector that is the result of the division operation.  Both examples above produce the same visual output.")
     ("PVector.dot" "Calculates the dot product of two vectors.")
     ("PVector.fromAngle" "Calculates and returns a new 2D unit vector from the specified angle value (in radians).")
     ("PVector.get" "Gets a copy of the vector, returns a PVector object.")
     ("PVector.heading" "Calculates the angle of rotation for a vector (2D vectors only).")
     ("PVector.lerp" "Calculates linear interpolation from one vector to another vector.  (Just like regular <b>lerp()</b>, but for vectors.)<br/>
<br/>
Note that there is one <em>static</em> version of this method, and two <em>non-static</em> versions.  The static version, <b>lerp(v1, v2, amt)</b> is given the two vectors to interpolate and returns a new PVector object.  The static version is used by referencing the PVector class directly.  (See the middle example above.)  The non-static versions, <b>lerp(v, amt)</b> and <b>lerp(x, y, z, amt)</b>, do not return a new PVector, but transform the values of the PVector on which they are called.  These non-static versions function the same way, but the former takes another vector as input, while the latter takes three float values.  (See the top and bottom examples above, respectively.)")
     ("PVector.limit" "Limit the magnitude of this vector to the value used for the <b>max</b> parameter.")
     ("PVector.mag" "Calculates the magnitude (length) of the vector and returns the result as a float (this is simply the equation <em>sqrt(x*x + y*y + z*z)</em>.)")
     ("PVector.magSq" "Calculates the magnitude (length) of the vector, squared.  This method is often used to improve performance since, unlike <b>mag()</b>, it does not require a <b>sqrt()</b> operation.")
     ("PVector.mult" "Multiplies a vector by a scalar.  The version of the method that uses a float acts directly on the vector upon which it is called (as in the first example above), and therefore has no return value.  The versions that receive both a PVector and a float as arugments are static methods, and each returns a new PVector that is the result of the multiplication operation.  Both examples above produce the same visual output.")
     ("PVector.normalize" "Normalize the vector to length 1 (make it a unit vector).")
     ("PVector.random2D" "Returns a new 2D unit vector with a random direction.  If you pass in <b>this</b> as an argument, it will use the PApplet's random number generator.")
     ("PVector.random3D" "Returns a new 3D unit vector with a random direction.  If you pass in <b>this</b> as an argument, it will use the PApplet's random number generator.")
     ("PVector.rotate" "Rotates a vector by the specified angle (2D vectors only), while maintaining the same magnitude.")
     ("PVector.set" "Sets the x, y, and z component of the vector using three separate variables, the data from a PVector, or the values from a float array.")
     ("PVector.setMag" "Set the magnitude of this vector to the value used for the <b>len</b> parameter.")
     ("PVector.sub" "Subtracts x, y, and z components from a vector, subtracts one vector from another, or subtracts two independent vectors. The version of the method that substracts two vectors is a static method and returns a PVector, the others have no return value -- they act directly on the vector. See the examples for more context.  In all cases, the second vector (v2) is subtracted from the first (v1), resulting in v1-v2.")
     ("PVector.x" "The x component of the vector. This field (variable) can be used to both get and set the value (see above example.)")
     ("PVector.y" "The y component of the vector. This field (variable) can be used to both get and set the value (see above example.)")
     ("PVector.z" "The z component of the vector. This field (variable) can be used to both get and set the value (see above example.)")
     ("PImage.blend" "Blends a region of pixels into the image specified by the <b>img</b> parameter. These copies utilize full alpha channel support and a choice of the following modes to blend the colors of source pixels (A) with the ones of pixels in the destination image (B):<br />
<br />
BLEND - linear interpolation of colours: C = A*factor + B<br />
<br />
ADD - additive blending with white clip: C = min(A*factor + B, 255)<br />
<br />
SUBTRACT - subtractive blending with black clip: C = max(B - A*factor, 0)<br />
<br />
DARKEST - only the darkest colour succeeds: C = min(A*factor, B)<br />
<br />
LIGHTEST - only the lightest colour succeeds: C = max(A*factor, B)<br />
<br />
DIFFERENCE - subtract colors from underlying image.<br />
<br />
EXCLUSION - similar to DIFFERENCE, but less extreme.<br />
<br />
MULTIPLY - Multiply the colors, result will always be darker.<br />
<br />
SCREEN - Opposite multiply, uses inverse values of the colors.<br />
<br />
OVERLAY - A mix of MULTIPLY and SCREEN. Multiplies dark values,
and screens light values.<br />
<br />
HARD_LIGHT - SCREEN when greater than 50% gray, MULTIPLY when lower.<br />
<br />
SOFT_LIGHT - Mix of DARKEST and LIGHTEST.
Works like OVERLAY, but not as harsh.<br />
<br />
DODGE - Lightens light tones and increases contrast, ignores darks.
Called \"Color Dodge\" in Illustrator and Photoshop.<br />
<br />
BURN - Darker areas are applied, increasing contrast, ignores lights.
Called \"Color Burn\" in Illustrator and Photoshop.<br />
<br />
All modes use the alpha information (highest byte) of source image pixels as the blending factor. If the source and destination regions are different sizes, the image will be automatically resized to match the destination size. If the <b>src</b> parameter is not used, the display window is used as the source image.<br />
<br />
As of release 0149, this function ignores <b>imageMode()</b>.")
     ("PImage.copy" "Copies a region of pixels from one image into another. If the source and destination regions aren't the same size, it will automatically resize source pixels to fit the specified target region. No alpha information is used in the process, however if the source image has an alpha channel set, it will be copied as well.
<br /><br />
As of release 0149, this function ignores <b>imageMode()</b>.")
     ("PImage.filter" "Filters the image as defined by one of the following modes:<br />
<br />
THRESHOLD<br />
Converts the image to black and white pixels depending if they are above or below the threshold defined by the level parameter. The parameter must be between 0.0 (black) and 1.0 (white). If no level is specified, 0.5 is used.<br />
<br />
GRAY<br />
Converts any colors in the image to grayscale equivalents. No parameter is used.<br />
<br />
OPAQUE<br />
Sets the alpha channel to entirely opaque. No parameter is used.<br />
<br />
INVERT<br />
Sets each pixel to its inverse value. No parameter is used.<br />
<br />
POSTERIZE<br />
Limits each channel of the image to the number of colors specified as the parameter. The parameter can be set to values between 2 and 255, but results are most noticeable in the lower ranges.<br />
<br />
BLUR<br />
Executes a Guassian blur with the level parameter specifying the extent of the blurring. If no parameter is used, the blur is equivalent to Guassian blur of radius 1. Larger values increase the blur.<br />
<br />
ERODE<br />
Reduces the light areas. No parameter is used.<br />
<br />
DILATE<br />
Increases the light areas. No parameter is used.")
     ("PImage.get" "Reads the color of any pixel or grabs a section of an image. If no parameters are specified, the entire image is returned. Use the <b>x</b> and <b>y</b> parameters to get the value of one pixel. Get a section of the display window by specifying an additional <b>width</b> and <b>height</b> parameter. When getting an image, the <b>x</b> and <b>y</b> parameters define the coordinates for the upper-left corner of the image, regardless of the current <b>imageMode()</b>.<br />
<br />
If the pixel requested is outside of the image window, black is returned. The numbers returned are scaled according to the current color ranges, but only RGB values are returned by this function. For example, even though you may have drawn a shape with <b>colorMode(HSB)</b>, the numbers returned will be in RGB format.<br />
<br />
Getting the color of a single pixel with <b>get(x, y)</b> is easy, but not as fast as grabbing the data directly from <b>pixels[]</b>. The equivalent statement to <b>get(x, y)</b> using <b>pixels[]</b> is <b>pixels[y*width+x]</b>. See the reference for <b>pixels[]</b> for more information.")
     ("PImage.height" "The height of the image in units of pixels.")
     ("PImage.loadPixels" "Loads the pixel data for the image into its <b>pixels[]</b> array. This function must always be called before reading from or writing to <b>pixels[]</b>.
<br/><br/>
Certain renderers may or may not seem to require <b>loadPixels()</b> or <b>updatePixels()</b>. However, the rule is that any time you want to manipulate the <b>pixels[]</b> array, you must first call <b>loadPixels()</b>, and after changes have been made, call <b>updatePixels()</b>. Even if the renderer may not seem to use this function in the current Processing release, this will always be subject to change.")
     ("PImage.mask" "Masks part of an image from displaying by loading another image and using it as an alpha channel. This mask image should only contain grayscale data, but only the blue color channel is used. The mask image needs to be the same size as the image to which it is applied.<br />
<br />
In addition to using a mask image, an integer array containing the alpha channel data can be specified directly. This method is useful for creating dynamically generated alpha masks. This array must be of the same length as the target image's pixels array and should contain only grayscale data of values between 0-255.")
     ("PImage.pixels" "Array containing the values for all the pixels in the image. These values are of the color datatype. This array is the size of the image, meaning if the image is 100 x 100 pixels, there will be 10000 values and if the window is 200 x 300 pixels, there will be 60000 values. The <b>index</b> value defines the position of a value within the array. For example, the statement <b>color b = img.pixels[230]</b> will set the variable <b>b</b> equal to the value at that location in the array. Before accessing this array, the data must loaded with the <b>loadPixels()</b> method. After the array data has been modified, the <b>updatePixels()</b> method must be run to update the changes. Without <b>loadPixels()</b>, running the code may (or will in future releases) result in a NullPointerException.")
     ("PImage.resize" "Resize the image to a new width and height. To make the image scale proportionally, use 0 as the value for the <b>wide</b> or <b>high</b> parameter. For instance, to make the width of an image 150 pixels, and change the height using the same proportion, use resize(150, 0).<br />
<br />
Even though a PGraphics is technically a PImage, it is not possible to rescale the image data found in a PGraphics. (It's simply not possible to do this consistently across renderers: technically infeasible with P3D, or what would it even do with PDF?) If you want to resize PGraphics content, first get a copy of its image data using the <b>get()</b> method, and call <b>resize()</b> on the PImage that is returned.")
     ("PImage.save" "Saves the image into a file. Images are saved in TIFF, TARGA, JPEG, and PNG format depending on the extension within the <b>filename</b> parameter. For example, \"image.tif\" will have a TIFF image and \"image.png\" will save a PNG image. If no extension is included in the filename, the image will save in TIFF format and <b>.tif</b> will be added to the name. These files are saved to the sketch's folder, which may be opened by selecting \"Show sketch folder\" from the \"Sketch\" menu. It is not possible to use <b>save()</b> while running the program in a web browser.<br /><br />To save an image created within the code, rather than through loading, it's necessary to make the image with the <b>createImage()</b> function so it is aware of the location of the program and can therefore save the file to the right place. See the <b>createImage()</b> reference for more information.")
     ("PImage.set" "Changes the color of any pixel or writes an image directly into the display window.<br />
<br />
The <b>x</b> and <b>y</b> parameters specify the pixel to change and the <b>color</b> parameter specifies the color value. The color parameter is affected by the current color mode (the default is RGB values from 0 to 255). When setting an image, the <b>x</b> and <b>y</b> parameters define the coordinates for the upper-left corner of the image, regardless of the current <b>imageMode()</b>.
<br /><br />
Setting the color of a single pixel with <b>set(x, y)</b> is easy, but not as fast as putting the data directly into <b>pixels[]</b>. The equivalent statement to <b>set(x, y, #000000)</b> using <b>pixels[]</b> is <b>pixels[y*width+x] = #000000</b>. See the reference for <b>pixels[]</b> for more information.")
     ("PImage.updatePixels" "Updates the image with the data in its <b>pixels[]</b> array. Use in conjunction with <b>loadPixels()</b>. If you're only reading pixels from the array, there's no need to call <b>updatePixels()</b>.
<br/><br/>
Certain renderers may or may not seem to require <b>loadPixels()</b> or <b>updatePixels()</b>. However, the rule is that any time you want to manipulate the <b>pixels[]</b> array, you must first call <b>loadPixels()</b>, and after changes have been made, call <b>updatePixels()</b>. Even if the renderer may not seem to use this function in the current Processing release, this will always be subject to change.
<br/> <br/>
Currently, none of the renderers use the additional parameters to <b>updatePixels()</b>, however this may be implemented in the future.")
     ("PImage.width" "The width of the image in units of pixels.")
     ("day()" "Processing communicates with the clock on your computer. The <b>day()</b> function returns the current day as a value from 1 - 31.")
     ("hour()" "Processing communicates with the clock on your computer. The <b>hour()</b> function returns the current hour as a value from 0 - 23.")
     ("millis()" "Returns the number of milliseconds (thousandths of a second) since starting the program. This information is often used for timing events and animation sequences.")
     ("minute()" "Processing communicates with the clock on your computer. The <b>minute()</b> function returns the current minute as a value from 0 - 59.")
     ("month()" "Processing communicates with the clock on your computer. The <b>month()</b> function returns the current month as a value from 1 - 12.")
     ("second()" "Processing communicates with the clock on your computer. The <b>second()</b> function returns the current second as a value from 0 - 59.")
     ("year()" "Processing communicates with the clock on your computer. The <b>year()</b> function returns the current year as an integer (2003, 2004, 2005, etc).")
     ("noise()" "Returns the Perlin noise value at specified coordinates. Perlin noise is a random sequence generator producing a more natural, harmonic succession of numbers than that of the standard <b>random()</b> function. It was invented by Ken Perlin in the 1980s and has been used in graphical applications to generate procedural textures, shapes, terrains, and other seemingly organic forms.<br/>
<br/>
In contrast to the <b>random()</b> function, Perlin noise is defined in an infinite n-dimensional space, in which each pair of coordinates corresponds to a fixed semi-random value (fixed only for the lifespan of the program). The resulting value will always be between 0.0 and 1.0. Processing can compute 1D, 2D and 3D noise, depending on the number of coordinates given. The noise value can be animated by moving through the noise space, as demonstrated in the first example above. The 2nd and 3rd dimensions can also be interpreted as time.<br/>
<br/>
The actual noise structure is similar to that of an audio signal, in respect to the function's use of frequencies. Similar to the concept of harmonics in physics, Perlin noise is computed over several octaves which are added together for the final result.<br/>
<br/>
Another way to adjust the character of the resulting sequence is the scale of the input coordinates. As the function works within an infinite space, the value of the coordinates doesn't matter as such; only the <em>distance</em> between successive coordinates is important (such as when using <b>noise()</b> within a loop). As a general rule, the smaller the difference between coordinates, the smoother the resulting noise sequence. Steps of 0.005-0.03 work best for most applications, but this will differ depending on use.")
     ("noiseDetail()" "Adjusts the character and level of detail produced by the Perlin noise function. Similar to harmonics in physics, noise is computed over several octaves. Lower octaves contribute more to the output signal and as such define the overal intensity of the noise, whereas higher octaves create finer-grained details in the noise sequence.<br/>
<br/>
By default, noise is computed over 4 octaves with each octave contributing exactly half than its predecessor, starting at 50% strength for the first octave. This falloff amount can be changed by adding an additional function parameter. For example, a falloff factor of 0.75 means each octave will now have 75% impact (25% less) of the previous lower octave. While any number between 0.0 and 1.0 is valid, note that values greater than 0.5 may result in <b>noise()</b> returning values greater than 1.0.<br/>
<br/>
By changing these parameters, the signal created by the <b>noise()</b> function can be adapted to fit very specific needs and characteristics.")
     ("noiseSeed()" "Sets the seed value for <b>noise()</b>. By default, <b>noise()</b> produces different results each time the program is run. Set the <b>seed</b> parameter to a constant to return the same pseudo-random numbers each time the software is run.")
     ("random()" "Generates random numbers. Each time the <b>random()</b> function is called, it returns an unexpected value within the specified range. If only one parameter is passed to the function, it will return a float between zero and the value of the <b>high</b> parameter. For example, <b>random(5)</b> returns values between 0 and 5 (starting at zero, and up to, but not including, 5).<br/>
<br/>
If two parameters are specified, the function will return a float with a value between the two values. For example, <b>random(-5, 10.2)</b> returns values starting at -5 and up to (but not including) 10.2. To convert a floating-point random number to an integer, use the <b>int()</b> function.")
     ("randomGaussian()" "Returns a float from a random series of numbers having a mean of 0 and standard deviation of 1. Each time the <b>randomGaussian()</b> function is called, it returns a number fitting a Gaussian, or normal, distribution. There is theoretically no minimum or maximum value that <b>randomGaussian()</b> might return. Rather, there is just a very low probability that values far from the mean will be returned; and a higher probability that numbers near the mean will be returned.")
     ("randomSeed()" "Sets the seed value for <b>random()</b>. By default, <b>random()</b> produces different results each time the program is run. Set the <b>seed</b> parameter to a constant to return the same pseudo-random numbers each time the software is run.")
     ("abs()" "Calculates the absolute value (magnitude) of a number. The absolute value of a number is always positive.")
     ("ceil()" "Calculates the closest int value that is greater than or equal to the value of the parameter. For example, <b>ceil(9.03)</b> returns the value 10.")
     ("constrain()" "Constrains a value to not exceed a maximum and minimum value.")
     ("dist()" "Calculates the distance between two points.")
     ("exp()" "Returns Euler's number <i>e</i> (2.71828...) raised to the power of the <b>n</b> parameter.")
     ("floor()" "Calculates the closest int value that is less than or equal to the value of the parameter.")
     ("lerp()" "Calculates a number between two numbers at a specific increment. The <b>amt</b> parameter is the amount to interpolate between the two values where 0.0 equal to the first point, 0.1 is very near the first point, 0.5 is half-way in between, etc. The lerp function is convenient for creating motion along a straight path and for drawing dotted lines.")
     ("log()" "Calculates the natural logarithm (the base-<i>e</i> logarithm) of a number. This function expects the <b>n</b> parameter to be a value greater than 0.0.")
     ("mag()" "Calculates the magnitude (or length) of a vector. A vector is a direction in space commonly used in computer graphics and linear algebra. Because it has no \"start\" position, the magnitude of a vector can be thought of as the distance from the coordinate 0,0 to its x,y value. Therefore, <b>mag()</b> is a shortcut for writing <b>dist(0, 0, x, y)</b>.")
     ("map()" "Re-maps a number from one range to another.<br/>
<br/>
In the first example above, the number 25 is converted from a value in the range of 0 to 100 into a value that ranges from the left edge of the window (0) to the right edge (width).<br/>
<br/>
As shown in the second example, numbers outside of the range are not clamped to the minimum and maximum parameters values, because out-of-range values are often intentional and useful.")
     ("max()" "Determines the largest value in a sequence of numbers, and then returns that value. <b>max()</b> accepts either two or three <b>float</b> or <b>int</b> values as parameters, or an array of any length.")
     ("min()" "Determines the smallest value in a sequence of numbers, and then returns that value. <b>min()</b> accepts either two or three <b>float</b> or <b>int</b> values as parameters, or an array of any length.")
     ("norm()" "Normalizes a number from another range into a value between 0 and 1. Identical to <b>map(value, low, high, 0, 1)</b>.<br/>
<br/>
Numbers outside of the range are not clamped to 0 and 1, because out-of-range values are often intentional and useful.  (See the second example above.)")
     ("pow()" "Facilitates exponential expressions. The <b>pow()</b> function is an efficient way of multiplying numbers by themselves (or their reciprocals) in large quantities. For example, <b>pow(3, 5)</b> is equivalent to the expression 3*3*3*3*3 and <b>pow(3, -5)</b> is equivalent to 1 / 3*3*3*3*3.")
     ("round()" "Calculates the integer closest to the <b>n</b> parameter. For example, <b>round(133.8)</b> returns the value 134.")
     ("sq()" "Squares a number (multiplies a number by itself). The result is always a positive number, as multiplying two negative numbers always yields a positive result. For example, -1 * -1 = 1.")
     ("sqrt()" "Calculates the square root of a number. The square root of a number is always positive, even though there may be a valid negative root. The square root <b>s</b> of number <b>a</b> is such that <b>s*s = a</b>. It is the opposite of squaring.")
     ("acos()" "The inverse of <b>cos()</b>, returns the arc cosine of a value. This function expects the values in the range of -1 to 1 and values are returned in the range <b>0</b> to <b>PI (3.1415927)</b>.")
     ("asin()" "The inverse of <b>sin()</b>, returns the arc sine of a value. This function expects the values in the range of -1 to 1 and values are returned in the range <b>-PI/2</b> to <b>PI/2</b>.")
     ("atan()" "The inverse of <b>tan()</b>, returns the arc tangent of a value. This function expects the values in the range of -Infinity to Infinity (exclusive) and values are returned in the range <b>-PI/2</b> to <b>PI/2 </b>.")
     ("atan2()" "Calculates the angle (in radians) from a specified point to the coordinate origin as measured from the positive x-axis. Values are returned as a <b>float</b> in the range from <b>PI</b> to <b>-PI</b>. The <b>atan2()</b> function is most often used for orienting geometry to the position of the cursor.  Note: The y-coordinate of the point is the first parameter, and the x-coordinate is the second parameter, due the the structure of calculating the tangent.")
     ("cos()" "Calculates the cosine of an angle. This function expects the values of the <b>angle</b> parameter to be provided in radians (values from 0 to PI*2). Values are returned in the range -1 to 1.")
     ("degrees()" "Converts a radian measurement to its corresponding value in degrees. Radians and degrees are two ways of measuring the same thing. There are 360 degrees in a circle and 2*PI radians in a circle. For example, 90&deg; = PI/2 = 1.5707964. All trigonometric functions in Processing require their parameters to be specified in radians.")
     ("radians()" "Converts a degree measurement to its corresponding value in radians. Radians and degrees are two ways of measuring the same thing. There are 360 degrees in a circle and 2*PI radians in a circle. For example, 90&deg; = PI/2 = 1.5707964. All trigonometric functions in Processing require their parameters to be specified in radians.")
     ("sin()" "Calculates the sine of an angle. This function expects the values of the <b>angle</b> parameter to be provided in radians (values from 0 to 6.28). Values are returned in the range -1 to 1.")
     ("tan()" "Calculates the ratio of the sine and cosine of an angle. This function expects the values of the <b>angle</b> parameter to be provided in radians (values from 0 to PI*2). Values are returned in the range <b>infinity</b> to <b>-infinity</b>.")
     ("alpha()" "Extracts the alpha value from a color.")
     ("blendColor()" "Blends two color values together based on the blending mode given as the <b>MODE</b> parameter. The possible modes are described in the reference for the <b>blend()</b> function.")
     ("blue()" "Extracts the blue value from a color, scaled to match current <b>colorMode()</b>. The value is always returned as a float, so be careful not to assign it to an int value.<br />
<br />
The <b>blue()</b> function is easy to use and understand, but it is slower than a technique called bit masking. When working in <b>colorMode(RGB, 255)</b>, you can acheive the same results as <b>blue()</b> but with greater speed by using a bit mask to remove the other color components. For example, the following two lines of code are equivalent means of getting the blue value of the color value <b>c</b>:<br />
<br />
<pre>float b1 = blue(c);   // Simpler, but slower to calculate
float b2 = c & 0xFF;  // Very fast to calculate</pre>")
     ("brightness()" "Extracts the brightness value from a color.")
     ("color()" "Creates colors for storing in variables of the <b>color</b> datatype. The parameters are interpreted as RGB or HSB values depending on the current <b>colorMode()</b>. The default mode is RGB values from 0 to 255 and, therefore, <b>color(255, 204, 0)</b> will return a bright yellow color (see the first example above).<br/>
<br/>
Note that if only one value is provided to <b>color()</b>, it will be interpreted as a grayscale value. Add a second value, and it will be used for alpha transparency. When three values are specified, they are interpreted as either RGB or HSB values. Adding a fourth value applies alpha transparency.<br/>
<br/>
Note that when using hexadecimal notation, it is not necessary to use <b>color()</b>, as in: <b>color c = #006699</b><br/>
<br/>
More about how colors are stored can be found in the reference for the <a href=\"color_datatype.html\">color</a> datatype.")
     ("green()" "Extracts the green value from a color, scaled to match current <b>colorMode()</b>. The value is always returned as a float, so be careful not to assign it to an int value.<br />
<br />
The <b>green()</b> function is easy to use and understand, but it is slower than a technique called bit shifting. When working in <b>colorMode(RGB, 255)</b>, you can acheive the same results as <b>green()</b> but with greater speed by using the right shift operator (<b>>></b>) with a bit mask. For example, the following two lines of code are equivalent means of getting the green value of the color value <b>c</b>:<br />
<br />
<pre>float r1 = green(c);  // Simpler, but slower to calculate
float r2 = c >> 8 & 0xFF;  // Very fast to calculate</pre>")
     ("hue()" "Extracts the hue value from a color.")
     ("lerpColor()" "Calculates a color or colors between two color at a specific increment. The <b>amt</b> parameter is the amount to interpolate between the two values where 0.0 equal to the first point, 0.1 is very near the first point, 0.5 is halfway in between, etc. 
<br />
An amount below 0 will be treated as 0. Likewise, amounts above 1 will be capped at 1. This is different from the behavior of lerp(), but necessary because otherwise numbers outside the range will produce strange and unexpected colors.")
     ("red()" "Extracts the red value from a color, scaled to match current <b>colorMode()</b>. The value is always returned as a float, so be careful not to assign it to an int value.<br />
<br />
The <b>red()</b> function is easy to use and understand, but it is slower than a technique called bit shifting. When working in <b>colorMode(RGB, 255)</b>, you can acheive the same results as <b>red()</b> but with greater speed by using the right shift operator (<b>>></b>) with a bit mask. For example, the following two lines of code are equivalent means of getting the red value of the color value <b>c</b>:<br />
<br />
<pre>float r1 = red(c);  // Simpler, but slower to calculate
float r2 = c >> 16 & 0xFF;  // Very fast to calculate</pre>")
     ("saturation()" "Extracts the saturation value from a color.")
     ("ambient()" "Sets the ambient reflectance for shapes drawn to the screen. This is combined with the ambient light component of environment. The color components set through the parameters define the reflectance. For example in the default color mode, setting v1=255, v2=127, v3=0, would cause all the red light to reflect and half of the green light to reflect. Used in combination with <b>emissive()</b>, <b>specular()</b>, and <b>shininess()</b> in setting the material properties of shapes.")
     ("emissive()" "Sets the emissive color of the material used for drawing shapes drawn to the screen. Used in combination with <b>ambient()</b>, <b>specular()</b>, and <b>shininess()</b> in setting the material properties of shapes.")
     ("shininess()" "Sets the amount of gloss in the surface of shapes. Used in combination with <b>ambient()</b>, <b>specular()</b>, and <b>emissive()</b> in setting the material properties of shapes.")
     ("specular()" "Sets the specular color of the materials used for shapes drawn to the screen, which sets the color of highlights. Specular refers to light which bounces off a surface in a preferred direction (rather than bouncing in all directions like a diffuse light). Used in combination with <b>emissive()</b>, <b>ambient()</b>, and <b>shininess()</b> in setting the material properties of shapes.")
     ("background()" "The <b>background()</b> function sets the color used for the background of the Processing window. The default background is light gray. This function is typically used within <b>draw()</b> to clear the display window at the beginning of each frame, but it can be used inside <b>setup()</b> to set the background on the first frame of animation or if the backgound need only be set once.
<br/> <br/>
An image can also be used as the background for a sketch, although the image's width and height must match that of the sketch window. Images used with <b>background()</b> will ignore the current <b>tint()</b> setting. To resize an image to the size of the sketch window, use image.resize(width, height).
<br/> <br/>
It is not possible to use the transparency <b>alpha</b> parameter with background colors on the main drawing surface. It can only be used along with a <b>PGraphics</b> object and <b>createGraphics()</b>.")
     ("clear()" "Clears the pixels within a buffer. This function only works on PGraphics objects created with the <b>createGraphics()</b> function; it won't work with the main display window. Unlike the main graphics context (the display window), pixels in additional graphics areas created with <b>createGraphics()</b> can be entirely or partially transparent. This function clears everything to make all of the pixels 100% transparent.")
     ("colorMode()" "Changes the way Processing interprets color data. By default, the parameters for <b>fill()</b>, <b>stroke()</b>, <b>background()</b>, and <b>color()</b> are defined by values between 0 and 255 using the RGB color model. The <b>colorMode()</b> function is used to change the numerical range used for specifying colors and to switch color systems. For example, calling <b>colorMode(RGB, 1.0)</b> will specify that values are specified between 0 and 1. The limits for defining colors are altered by setting the parameters <b>max</b>, <b>max1</b>, <b>max2</b>, <b>max3</b>, and <b>maxA</b>.")
     ("fill()" "Sets the color used to fill shapes. For example, if you run <b>fill(204, 102, 0)</b>, all subsequent shapes will be filled with orange. This color is either specified in terms of the RGB or HSB color depending on the current <b>colorMode()</b>.  (The default color space is RGB, with each value in the range from 0 to 255.)
<br/><br/>
When using hexadecimal notation to specify a color, use \"#\" or \"0x\" before the values (e.g., #CCFFAA or 0xFFCCFFAA). The # syntax uses six digits to specify a color (just as colors are typically specified in HTML and CSS). When using the hexadecimal notation starting with \"0x\", the hexadecimal value must be specified with eight characters; the first two characters define the alpha component, and the remainder define the red, green, and blue components. 
<br/><br/>
The value for the \"gray\" parameter must be less than or equal to the current maximum value as specified by <b>colorMode()</b>. The default maximum value is 255.
<br/><br/>
To change the color of an image or a texture, use <b>tint()</b>.")
     ("noFill()" "Disables filling geometry. If both <b>noStroke()</b> and <b>noFill()</b> are called, nothing will be drawn to the screen.")
     ("noStroke()" "Disables drawing the stroke (outline). If both <b>noStroke()</b> and <b>noFill()</b> are called, nothing will be drawn to the screen.")
     ("stroke()" "Sets the color used to draw lines and borders around shapes. This color is either specified in terms of the RGB or HSB color depending on the current <b>colorMode()</b> (the default color space is RGB, with each value in the range from 0 to 255). 
<br/> <br/>
When using hexadecimal notation to specify a color, use \"#\" or \"0x\" before the values (e.g. #CCFFAA, 0xFFCCFFAA). The # syntax uses six digits to specify a color (the way colors are specified in HTML and CSS). When using the hexadecimal notation starting with \"0x\", the hexadecimal value must be specified with eight characters; the first two characters define the alpha component and the remainder the red, green, and blue components. 
<br/> <br/>
The value for the gray parameter must be less than or equal to the current maximum value as specified by <b>colorMode()</b>. The default maximum value is 255.
<br/> <br/>
When drawing in 2D with the default renderer, you may need <b>hint(ENABLE_STROKE_PURE)</b> to improve drawing quality (at the expense of performance). See the hint() documentation for more details.")
     ("modelX()" "Returns the three-dimensional X, Y, Z position in model space. This returns the X value for a given coordinate based on the current set of transformations (scale, rotate, translate, etc.) The X value can be used to place an object in space relative to the location of the original point once the transformations are no longer in use. 
<br/> <br/>
In the example, the <b>modelX()</b>, <b>modelY()</b>, and <b>modelZ()</b> functions record the location of a box in space after being placed using a series of translate and rotate commands. After popMatrix() is called, those transformations no longer apply, but the (x, y, z) coordinate returned by the model functions is used to place another box in the same location.")
     ("modelY()" "Returns the three-dimensional X, Y, Z position in model space. This returns the Y value for a given coordinate based on the current set of transformations (scale, rotate, translate, etc.) The Y value can be used to place an object in space relative to the location of the original point once the transformations are no longer in use.<br /> 
<br />
In the example, the <b>modelX()</b>, <b>modelY()</b>, and <b>modelZ()</b> functions record the location of a box in space after being placed using a series of translate and rotate commands. After popMatrix() is called, those transformations no longer apply, but the (x, y, z) coordinate returned by the model functions is used to place another box in the same location.")
     ("modelZ()" "Returns the three-dimensional X, Y, Z position in model space. This returns the Z value for a given coordinate based on the current set of transformations (scale, rotate, translate, etc.) The Z value can be used to place an object in space relative to the location of the original point once the transformations are no longer in use.<br />
<br />
In the example, the <b>modelX()</b>, <b>modelY()</b>, and <b>modelZ()</b> functions record the location of a box in space after being placed using a series of translate and rotate commands. After popMatrix() is called, those transformations no longer apply, but the (x, y, z) coordinate returned by the model functions is used to place another box in the same location.")
     ("screenX()" "Takes a three-dimensional X, Y, Z position and returns the X value for where it will appear on a (two-dimensional) screen.")
     ("screenY()" "Takes a three-dimensional X, Y, Z position and returns the Y value for where it will appear on a (two-dimensional) screen.")
     ("screenZ()" "Takes a three-dimensional X, Y, Z position and returns the Z value for where it will appear on a (two-dimensional) screen.")
     ("beginCamera()" "The <b>beginCamera()</b> and <b>endCamera()</b> functions enable advanced customization of the camera space. The functions are useful if you want to more control over camera movement, however for most users, the <b>camera()</b> function will be sufficient.<br /><br />The camera functions will replace any transformations (such as <b>rotate()</b> or <b>translate()</b>) that occur before them in <b>draw()</b>, but they will not automatically replace the camera transform itself. For this reason, camera functions should be placed at the beginning of <b>draw()</b> (so that transformations happen afterwards), and the <b>camera()</b> function can be used after <b>beginCamera()</b> if you want to reset the camera before applying transformations.<br /><br />This function sets the matrix mode to the camera matrix so calls such as <b>translate()</b>, <b>rotate()</b>, applyMatrix() and resetMatrix() affect the camera. <b>beginCamera()</b> should always be used with a following <b>endCamera()</b> and pairs of <b>beginCamera()</b> and <b>endCamera()</b> cannot be nested.")
     ("camera()" "Sets the position of the camera through setting the eye position, the center of the scene, and which axis is facing upward. Moving the eye position and the direction it is pointing (the center of the scene) allows the images to be seen from different angles. The version without any parameters sets the camera to the default position, pointing to the center of the display window with the Y axis as up. The default values are <b>camera(width/2.0, height/2.0, (height/2.0) / tan(PI*30.0 / 180.0), width/2.0, height/2.0, 0, 0, 1, 0)</b>. This function is similar to <b>gluLookAt()</b> in OpenGL, but it first clears the current camera settings.")
     ("endCamera()" "The <b>beginCamera()</b> and <b>endCamera()</b> functions enable advanced customization of the camera space. Please see the reference for <b>beginCamera()</b> for a description of how the functions are used.")
     ("frustum()" "Sets a perspective matrix as defined by the parameters.<br/>
<br/>
A frustum is a geometric form: a pyramid with its top cut off.  With the viewer's eye at the imaginary top of the pyramid, the six planes of the frustum act as clipping planes when rendering a 3D view.  Thus, any form inside the clipping planes is rendered and visible; anything outside those planes is not visible.<br/>
<br/>
Setting the frustum has the effect of changing the <em>perspective</em> with which the scene is rendered.  This can be acheived more simply in many cases by using <strong>perspective()</strong>.<br/>
<br/>
Note that the near value must be greater than zero (as the point of the frustum \"pyramid\" cannot converge \"behind\" the viewer).  Similarly, the far value must be greater than the near value (as the \"far\" plane of the frustum must be \"farther away\" from the viewer than the near plane).<br/>
<br/>
Works like glFrustum, except it wipes out the current perspective matrix rather than multiplying itself with it.")
     ("ortho()" "Sets an orthographic projection and defines a parallel clipping volume. All objects with the same dimension appear the same size, regardless of whether they are near or far from the camera. The parameters to this function specify the clipping volume where left and right are the minimum and maximum x values, top and bottom are the minimum and maximum y values, and near and far are the minimum and maximum z values. If no parameters are given, the default is used: ortho(0, width, 0, height).")
     ("perspective()" "Sets a perspective projection applying foreshortening, making distant objects appear smaller than closer ones. The parameters define a viewing volume with the shape of truncated pyramid. Objects near to the front of the volume appear their actual size, while farther objects appear smaller. This projection simulates the perspective of the world more accurately than orthographic projection. The version of perspective without parameters sets the default perspective and the version with four parameters allows the programmer to set the area precisely. The default values are: perspective(PI/3.0, width/height, cameraZ/10.0, cameraZ*10.0) where cameraZ is ((height/2.0) / tan(PI*60.0/360.0));")
     ("printCamera()" "Prints the current camera matrix to the Console (the text window at the bottom of Processing).")
     ("printProjection()" "Prints the current projection matrix to the Console (the text window at the bottom of Processing).")
     ("ambientLight()" "Adds an ambient light. Ambient light doesn't come from a specific direction, the rays have light have bounced around so much that objects are evenly lit from all sides. Ambient lights are almost always used in combination with other types of lights. Lights need to be included in the <b>draw()</b> to remain persistent in a looping program. Placing them in the <b>setup()</b> of a looping program will cause them to only have an effect the first time through the loop. The <b>v1</b>, <b>v2</b>, and <b>v3</b> parameters are interpreted as either RGB or HSB values, depending on the current color mode.")
     ("directionalLight()" "Adds a directional light. Directional light comes from one direction: it is stronger when hitting a surface squarely, and weaker if it hits at a gentle angle. After hitting a surface, directional light scatters in all directions. Lights need to be included in the <b>draw()</b> to remain persistent in a looping program. Placing them in the <b>setup()</b> of a looping program will cause them to only have an effect the first time through the loop. The <b>v1</b>, <b>v2</b>, and <b>v3</b> parameters are interpreted as either RGB or HSB values, depending on the current color mode. The <b>nx</b>, <b>ny</b>, and <b>nz</b> parameters specify the direction the light is facing. For example, setting <b>ny</b> to -1 will cause the geometry to be lit from below (since the light would be facing directly upward).")
     ("lightFalloff()" "Sets the falloff rates for point lights, spot lights, and ambient lights. Like <b>fill()</b>, it affects only the elements which are created after it in the code. The default value is <b>lightFalloff(1.0, 0.0, 0.0)</b>, and the parameters are used to calculate the falloff with the following equation:<br/>
<br/>
d = distance from light position to vertex position<br/>
falloff = 1 / (CONSTANT + d * LINEAR + (d*d) * QUADRATIC)<br/>
<br/>
Thinking about an ambient light with a falloff can be tricky. If you want a region of your scene to be lit ambiently with one color and another region to be lit ambiently with another color, you could use an ambient light with location and falloff. You can think of it as a point light that doesn't care which direction a surface is facing.")
     ("lightSpecular()" "Sets the specular color for lights. Like <b>fill()</b>, it affects only the elements which are created after it in the code. Specular refers to light which bounces off a surface in a preferred direction (rather than bouncing in all directions like a diffuse light) and is used for creating highlights. The specular quality of a light interacts with the specular material qualities set through the <b>specular()</b> and <b>shininess()</b> functions.")
     ("lights()" "Sets the default ambient light, directional light, falloff, and specular values. The defaults are ambientLight(128, 128, 128) and directionalLight(128, 128, 128, 0, 0, -1), lightFalloff(1, 0, 0), and lightSpecular(0, 0, 0). Lights need to be included in the draw() to remain persistent in a looping program. Placing them in the setup() of a looping program will cause them to only have an effect the first time through the loop.")
     ("noLights()" "Disable all lighting. Lighting is turned off by default and enabled with the <b>lights()</b> function. This function can be used to disable lighting so that 2D geometry (which does not require lighting) can be drawn after a set of lighted 3D geometry.")
     ("normal()" "Sets the current normal vector. Used for drawing three dimensional shapes and surfaces, <b>normal()</b> specifies a vector perpendicular to a shape's surface which, in turn, determines how lighting affects it. Processing attempts to automatically assign normals to shapes, but since that's imperfect, this is a better option when you want more control. This function is identical to <b>glNormal3f()</b> in OpenGL.")
     ("pointLight()" "Adds a point light. Lights need to be included in the <b>draw()</b> to remain persistent in a looping program. Placing them in the <b>setup()</b> of a looping program will cause them to only have an effect the first time through the loop. The <b>v1</b>, <b>v2</b>, and <b>v3</b> parameters are interpreted as either RGB or HSB values, depending on the current color mode. The <b>x</b>, <b>y</b>, and <b>z</b> parameters set the position of the light.")
     ("spotLight()" "Adds a spot light. Lights need to be included in the <b>draw()</b> to remain persistent in a looping program. Placing them in the <b>setup()</b> of a looping program will cause them to only have an effect the first time through the loop. The <b>v1</b>, <b>v2</b>, and <b>v3</b> parameters are interpreted as either RGB or HSB values, depending on the current color mode. The <b>x</b>, <b>y</b>, and <b>z</b> parameters specify the position of the light and <b>nx</b>, <b>ny</b>, <b>nz</b> specify the direction of light. The <b>angle</b> parameter affects angle of the spotlight cone, while <b>concentration</b> sets the bias of light focusing toward the center of that cone.")
     ("applyMatrix()" "Multiplies the current matrix by the one specified through the parameters. This is very slow because it will try to calculate the inverse of the transform, so avoid it whenever possible. The equivalent function in OpenGL is glMultMatrix().")
     ("popMatrix()" "Pops the current transformation matrix off the matrix stack. Understanding pushing and popping requires understanding the concept of a matrix stack. The <b>pushMatrix()</b> function saves the current coordinate system to the stack and <b>popMatrix()</b> restores the prior coordinate system. <b>pushMatrix()</b> and <b>popMatrix()</b> are used in conjuction with the other transformation functions and may be embedded to control the scope of the transformations.")
     ("printMatrix()" "Prints the current matrix to the Console (the text window at the bottom of Processing).")
     ("pushMatrix()" "Pushes the current transformation matrix onto the matrix stack. Understanding <b>pushMatrix()</b> and <b>popMatrix()</b> requires understanding the concept of a matrix stack. The <b>pushMatrix()</b> function saves the current coordinate system to the stack and <b>popMatrix()</b> restores the prior coordinate system. <b>pushMatrix()</b> and <b>popMatrix()</b> are used in conjuction with the other transformation functions and may be embedded to control the scope of the transformations.")
     ("resetMatrix()" "Replaces the current matrix with the identity matrix. The equivalent function in OpenGL is <b>glLoadIdentity()</b>.")
     ("rotate()" "Rotates a shape the amount specified by the <b>angle</b> parameter. Angles must be specified in radians (values from <b>0</b> to <b>TWO_PI</b>), or they can be converted from degrees to radians with the <b>radians()</b> function. 
<br/> <br/>
Objects are always rotated around their relative position to the origin, and positive numbers rotate objects in a clockwise direction. Transformations apply to everything that happens afterward, and subsequent calls to the function compound the effect. For example, calling <b>rotate(HALF_PI)</b> once and then calling <b>rotate(HALF_PI)</b> a second time is the same as a single <b>rotate(PI)</b>. All tranformations are reset when <b>draw()</b> begins again. 
<br/> <br/>
Technically, <b>rotate()</b> multiplies the current transformation matrix by a rotation matrix. This function can be further controlled by <b>pushMatrix()</b> and <b>popMatrix()</b>.")
     ("rotateX()" "Rotates a shape around the x-axis the amount specified by the <b>angle</b> parameter. Angles should be specified in radians (values from 0 to PI*2) or converted to radians with the <b>radians()</b> function. Objects are always rotated around their relative position to the origin and positive numbers rotate objects in a counterclockwise direction. Transformations apply to everything that happens after and subsequent calls to the function accumulates the effect. For example, calling <b>rotateX(PI/2)</b> and then <b>rotateX(PI/2)</b> is the same as <b>rotateX(PI)</b>. If <b>rotateX()</b> is called within the <b>draw()</b>, the transformation is reset when the loop begins again. This function requires using P3D as a third parameter to <b>size()</b> as shown in the example above.")
     ("rotateY()" "Rotates a shape around the y-axis the amount specified by the <b>angle</b> parameter. Angles should be specified in radians (values from 0 to PI*2) or converted to radians with the <b>radians()</b> function. Objects are always rotated around their relative position to the origin and positive numbers rotate objects in a counterclockwise direction. Transformations apply to everything that happens after and subsequent calls to the function accumulates the effect. For example, calling <b>rotateY(PI/2)</b> and then <b>rotateY(PI/2)</b> is the same as <b>rotateY(PI)</b>. If <b>rotateY()</b> is called within the <b>draw()</b>, the transformation is reset when the loop begins again. This function requires using P3D as a third parameter to <b>size()</b> as shown in the examples above.")
     ("rotateZ()" "Rotates a shape around the z-axis the amount specified by the <b>angle</b> parameter. Angles should be specified in radians (values from 0 to PI*2) or converted to radians with the <b>radians()</b> function. Objects are always rotated around their relative position to the origin and positive numbers rotate objects in a counterclockwise direction. Transformations apply to everything that happens after and subsequent calls to the function accumulates the effect. For example, calling <b>rotateZ(PI/2)</b> and then <b>rotateZ(PI/2)</b> is the same as <b>rotateZ(PI)</b>. If <b>rotateZ()</b> is called within the <b>draw()</b>, the transformation is reset when the loop begins again. This function requires using P3D as a third parameter to <b>size()</b> as shown in the examples above.")
     ("scale()" "Increases or decreases the size of a shape by expanding and contracting vertices. Objects always scale from their relative origin to the coordinate system. Scale values are specified as decimal percentages. For example, the function call <b>scale(2.0)</b> increases the dimension of a shape by 200%.<br/>
<br/>
Transformations apply to everything that happens after and subsequent calls to the function multiply the effect. For example, calling <b>scale(2.0)</b> and then <b>scale(1.5)</b> is the same as <b>scale(3.0)</b>. If <b>scale()</b> is called within <b>draw()</b>, the transformation is reset when the loop begins again. Using this fuction with the <b>z</b> parameter requires using P3D as a parameter for <b>size()</b>, as shown in the third example above. This function can be further controlled with <b>pushMatrix()</b> and <b>popMatrix()</b>.")
     ("shearX()" "Shears a shape around the x-axis the amount specified by the <b>angle</b> parameter. Angles should be specified in radians (values from 0 to PI*2) or converted to radians with the <b>radians()</b> function. Objects are always sheared around their relative position to the origin and positive numbers shear objects in a clockwise direction. Transformations apply to everything that happens after and subsequent calls to the function accumulates the effect. For example, calling <b>shearX(PI/2)</b> and then <b>shearX(PI/2)</b> is the same as <b>shearX(PI)</b>. If <b>shearX()</b> is called within the <b>draw()</b>, the transformation is reset when the loop begins again.
<br/> <br/>
Technically, <b>shearX()</b> multiplies the current transformation matrix by a rotation matrix. This function can be further controlled by the <b>pushMatrix()</b> and <b>popMatrix()</b> functions.")
     ("shearY()" "Shears a shape around the y-axis the amount specified by the <b>angle</b> parameter. Angles should be specified in radians (values from 0 to PI*2) or converted to radians with the <b>radians()</b> function. Objects are always sheared around their relative position to the origin and positive numbers shear objects in a clockwise direction. Transformations apply to everything that happens after and subsequent calls to the function accumulates the effect. For example, calling <b>shearY(PI/2)</b> and then <b>shearY(PI/2)</b> is the same as <b>shearY(PI)</b>. If <b>shearY()</b> is called within the <b>draw()</b>, the transformation is reset when the loop begins again.
<br/> <br/>
Technically, <b>shearY()</b> multiplies the current transformation matrix by a rotation matrix. This function can be further controlled by the <b>pushMatrix()</b> and <b>popMatrix()</b> functions.")
     ("translate()" "Specifies an amount to displace objects within the display window. The <b>x</b> parameter specifies left/right translation, the <b>y</b> parameter specifies up/down translation, and the <b>z</b> parameter specifies translations toward/away from the screen. Using this function with the <b>z</b> parameter requires using P3D as a parameter in combination with size as shown in the above example.
<br/><br/>
Transformations are cumulative and apply to everything that happens after and subsequent calls to the function accumulates the effect. For example, calling <b>translate(50, 0)</b> and then <b>translate(20, 0)</b> is the same as <b>translate(70, 0)</b>. If <b>translate()</b> is called within <b>draw()</b>, the transformation is reset when the loop begins again. This function can be further controlled by using <b>pushMatrix()</b> and <b>popMatrix()</b>.")
     ("beginContour()" "Use the <b>beginContour()</b> and <b>endContour()</b> function to create negative shapes within shapes. For instance, the center of the letter 'O'. <b>beginContour()</b> begins recording vertices for the shape and <b>endContour()</b> stops recording. These functions can only be within a <b>beginShape()</b>/<b>endShape()</b> pair and they only work with the P2D and P3D renderers. <br />
<br/>
Transformations such as <b>translate()</b>, <b>rotate()</b>, and <b>scale()</b> do not work within a <b>beginContour()</b>/<b>endContour()</b> pair. It is also not possible to use other shapes, such as <b>ellipse()</b> or <b>rect()</b> within.")
     ("beginShape()" "Using the <b>beginShape()</b> and <b>endShape()</b> functions allow creating more complex forms. <b>beginShape()</b> begins recording vertices for a shape and <b>endShape()</b> stops recording. The value of the <b>kind</b> parameter tells it which types of shapes to create from the provided vertices. With no mode specified, the shape can be any irregular polygon. The parameters available for beginShape() are POINTS, LINES, TRIANGLES, TRIANGLE_FAN, TRIANGLE_STRIP, QUADS, and QUAD_STRIP. After calling the <b>beginShape()</b> function, a series of <b>vertex()</b> commands must follow. To stop drawing the shape, call <b>endShape()</b>. The <b>vertex()</b> function with two parameters specifies a position in 2D and the <b>vertex()</b> function with three parameters specifies a position in 3D. Each shape will be outlined with the current stroke color and filled with the fill color. 
<br/><br/>
Transformations such as <b>translate()</b>, <b>rotate()</b>, and <b>scale()</b> do not work within <b>beginShape()</b>. It is also not possible to use other shapes, such as <b>ellipse()</b> or <b>rect()</b> within <b>beginShape()</b>. 
<br/><br/>
The P3D renderer settings allow <b>stroke()</b> and <b>fill()</b> settings to be altered per-vertex, but P2D and the default renderer do not. Settings such as <b>strokeWeight()</b>, <b>strokeCap()</b>, and <b>strokeJoin()</b> cannot be changed while inside a <b>beginShape()</b>/<b>endShape()</b> block with any renderer.")
     ("bezierVertex()" "Specifies vertex coordinates for Bezier curves. Each call to <b>bezierVertex()</b> defines the position of two control points and one anchor point of a Bezier curve, adding a new segment to a line or shape. The first time <b>bezierVertex()</b> is used within a <b>beginShape()</b> call, it must be prefaced with a call to <b>vertex()</b> to set the first anchor point. This function must be used between <b>beginShape()</b> and <b>endShape()</b> and only when there is no MODE parameter specified to <b>beginShape()</b>. Using the 3D version requires rendering with P3D (see the Environment reference for more information).")
     ("curveVertex()" "Specifies vertex coordinates for curves. This function may only be used between <b>beginShape()</b> and <b>endShape()</b> and only when there is no MODE parameter specified to <b>beginShape()</b>. The first and last points in a series of <b>curveVertex()</b> lines will be used to guide the beginning and end of a the curve. A minimum of four points is required to draw a tiny curve between the second and third points. Adding a fifth point with <b>curveVertex()</b> will draw the curve between the second, third, and fourth points. The <b>curveVertex()</b> function is an implementation of Catmull-Rom splines. Using the 3D version requires rendering with P3D (see the Environment reference for more information).")
     ("endContour()" "Use the <b>beginContour()</b> and <b>endContour()</b> function to create negative shapes within shapes. For instance, the center of the letter 'O'. <b>beginContour()</b> begins recording vertices for the shape and <b>endContour()</b> stops recording. These functions can only be within a <b>beginShape()</b>/<b>endShape()</b> pair and they only work with the P2D and P3D renderers. <br />
<br/>
Transformations such as <b>translate()</b>, <b>rotate()</b>, and <b>scale()</b> do not work within a <b>beginContour()</b>/<b>endContour()</b> pair. It is also not possible to use other shapes, such as <b>ellipse()</b> or <b>rect()</b> within.")
     ("endShape()" "The <b>endShape()</b> function is the companion to <b>beginShape()</b> and may only be called after <b>beginShape()</b>. When <b>endshape()</b> is called, all of image data defined since the previous call to <b>beginShape()</b> is written into the image buffer. The constant CLOSE as the value for the MODE parameter to close the shape (to connect the beginning and the end).")
     ("quadraticVertex()" "Specifies vertex coordinates for quadratic Bezier curves. Each call to <b>quadraticVertex()</b> defines the position of one control points and one anchor point of a Bezier curve, adding a new segment to a line or shape. The first time <b>quadraticVertex()</b> is used within a <b>beginShape()</b> call, it must be prefaced with a call to <b>vertex()</b> to set the first anchor point. This function must be used between <b>beginShape()</b> and <b>endShape()</b> and only when there is no MODE parameter specified to <b>beginShape()</b>. Using the 3D version requires rendering with P3D (see the Environment reference for more information).")
     ("texture()" "Sets a texture to be applied to vertex points. The <b>texture()</b> function must be called between <b>beginShape()</b> and <b>endShape()</b> and before any calls to <b>vertex()</b>. This function only works with the P2D and P3D renderers.<br/>
<br/>
When textures are in use, the fill color is ignored. Instead, use <b>tint()</b> to specify the color of the texture as it is applied to the shape.")
     ("textureMode()" "Sets the coordinate space for texture mapping. The default mode is <b>IMAGE</b>, which refers to the actual coordinates of the image. <b>NORMAL</b> refers to a normalized space of values ranging from 0 to 1. This function only works with the P2D and P3D renderers.<br />
<br/>
With <b>IMAGE</b>, if an image is 100 x 200 pixels, mapping the image onto the entire size of a quad would require the points (0,0) (100, 0) (100,200) (0,200). The same mapping in <b>NORMAL</b> is (0,0) (1,0) (1,1) (0,1).")
     ("vertex()" "All shapes are constructed by connecting a series of vertices. <b>vertex()</b> is used to specify the vertex coordinates for points, lines, triangles, quads, and polygons. It is used exclusively within the <b>beginShape()</b> and <b>endShape()</b> functions.
<br/><br/>
Drawing a vertex in 3D using the <b>z</b> parameter requires the P3D parameter in combination with size, as shown in the above example.
<br/><br/>
This function is also used to map a texture onto geometry. The <b>texture()</b> function declares the texture to apply to the geometry and the <b>u</b> and <b>v</b> coordinates set define the mapping of this texture to the form. By default, the coordinates used for <b>u</b> and <b>v</b> are specified in relation to the image's size in pixels, but this relation can be changed with <b>textureMode()</b>.")
     ("ellipseMode()" "Modifies the location from which ellipses are drawn by changing the way in which parameters given to <b>ellipse()</b> are intepreted.<br/>
<br/>
The default mode is <b>ellipseMode(CENTER)</b>, which interprets the first two parameters of <b>ellipse()</b> as the shape's center point, while the third and fourth parameters are its width and height.<br/>
<br/>
<b>ellipseMode(RADIUS)</b> also uses the first two parameters of <b>ellipse()</b> as the shape's center point, but uses the third and fourth parameters to specify half of the shapes's width and height.<br />
<br/>
<b>ellipseMode(CORNER)</b> interprets the first two parameters of <b>ellipse()</b> as the upper-left corner of the shape, while the third and fourth parameters are its width and height.<br/>
<br/>
<b>ellipseMode(CORNERS)</b> interprets the first two parameters of <b>ellipse()</b> as the location of one corner of the ellipse's bounding box, and the third and fourth parameters as the location of the opposite corner.<br/>
<br/>
The parameter must be written in ALL CAPS because Processing is a case-sensitive language.")
     ("noSmooth()" "Draws all geometry with jagged (aliased) edges.  Note that <b>smooth()</b> is active by default, so it is necessary to call <b>noSmooth()</b> to disable smoothing of geometry, images, and fonts.")
     ("rectMode()" "Modifies the location from which rectangles are drawn by changing the way in which parameters given to <b>rect()</b> are intepreted.<br/>
<br/>
The default mode is <b>rectMode(CORNER)</b>, which interprets the first two parameters of <b>rect()</b> as the upper-left corner of the shape, while the third and fourth parameters are its width and height.<br/>
<br/>
<b>rectMode(CORNERS)</b> interprets the first two parameters of <b>rect()</b> as the location of one corner, and the third and fourth parameters as the location of the opposite corner.<br/>
<br/>
<b>rectMode(CENTER)</b> interprets the first two parameters of <b>rect()</b> as the shape's center point, while the third and fourth parameters are its width and height.<br/>
<br/>
<b>rectMode(RADIUS)</b> also uses the first two parameters of <b>rect()</b> as the shape's center point, but uses the third and fourth parameters to specify half of the shapes's width and height.<br />
<br />
The parameter must be written in ALL CAPS because Processing is a case-sensitive language.")
     ("smooth()" "Draws all geometry with smooth (anti-aliased) edges. <b>smooth()</b> will also improve image quality of resized images. Note that <b>smooth()</b> is active by default; <b>noSmooth()</b> can be used to disable smoothing of geometry, images, and fonts.<br />
<br />
The <b>level</b> parameter increases the level of smoothness with the P2D and P3D renderers. This is the level of over sampling applied to the graphics buffer. The value \"2\" will double the rendering size before scaling it down to the display size. This is called \"2x anti-aliasing.\" The value 4 is used for 4x anti-aliasing and 8 is specified for 8x anti-aliasing. If <b>level</b> is set to 0, it will disable all smoothing; it's the equivalent of the function <b>noSmooth()</b>. The maximum anti-aliasing level is determined by the hardware of the machine that is running the software.<br >
<br />
With the default renderer, <b>smooth(2)</b> is bilinear and <b>smooth(4)</b> is bicubic. Nothing implemented on Android 2D.")
     ("strokeCap()" "Sets the style for rendering line endings. These ends are either squared, extended, or rounded, each of which specified with the corresponding parameters: SQUARE, PROJECT, and ROUND. The default cap is ROUND.")
     ("strokeJoin()" "Sets the style of the joints which connect line segments. These joints are either mitered, beveled, or rounded and specified with the corresponding parameters MITER, BEVEL, and ROUND. The default joint is MITER.")
     ("strokeWeight()" "Sets the width of the stroke used for lines, points, and the border around shapes. All widths are set in units of pixels.")
     ("box()" "A box is an extruded rectangle. A box with equal dimensions on all sides is a cube.")
     ("sphere()" "A sphere is a hollow ball made from tessellated triangles.")
     ("sphereDetail()" "Controls the detail used to render a sphere by adjusting the number of vertices of the sphere mesh. The default resolution is 30, which creates a fairly detailed sphere definition with vertices every 360/30 = 12 degrees. If you're going to render a great number of spheres per frame, it is advised to reduce the level of detail using this function. The setting stays active until <b>sphereDetail()</b> is called again with a new parameter and so should <i>not</i> be called prior to every <b>sphere()</b> statement, unless you wish to render spheres with different settings, e.g. using less detail for smaller spheres or ones further away from the camera. To control the detail of the horizontal and vertical resolution independently, use the version of the functions with two parameters.")
     ("bezier()" "Draws a Bezier curve on the screen. These curves are defined by a series of anchor and control points. The first two parameters specify the first anchor point and the last two parameters specify the other anchor point. The middle parameters specify the control points which define the shape of the curve. Bezier curves were developed by French engineer Pierre Bezier. Using the 3D version requires rendering with P3D (see the Environment reference for more information).")
     ("bezierDetail()" "Sets the resolution at which Beziers display. The default value is 20. This function is only useful when using the <b>P3D</b> renderer; the default <b>P2D</b> renderer does not use this information.")
     ("bezierPoint()" "Evaluates the Bezier at point t for points a, b, c, d. The parameter t varies between 0 and 1, a and d are points on the curve, and b and c are the control points. This can be done once with the x coordinates and a second time with the y coordinates to get the location of a bezier curve at t.")
     ("bezierTangent()" "Calculates the tangent of a point on a Bezier curve. There is a good definition of <a href=\"http://en.wikipedia.org/wiki/Tangent\" target=\"new\"><em>tangent</em> on Wikipedia</a>.")
     ("curve()" "Draws a curved line on the screen. The first and second parameters specify the beginning control point and the last two parameters specify the ending control point. The middle parameters specify the start and stop of the curve. Longer curves can be created by putting a series of <b>curve()</b> functions together or using <b>curveVertex()</b>. An additional function called <b>curveTightness()</b> provides control for the visual quality of the curve. The <b>curve()</b> function is an implementation of Catmull-Rom splines. Using the 3D version requires rendering with P3D (see the Environment reference for more information).")
     ("curveDetail()" "Sets the resolution at which curves display. The default value is 20. This function is only useful when using the P3D renderer as the default P2D renderer does not use this information.")
     ("curvePoint()" "Evaluates the curve at point <b>t</b> for points <b>a</b>, <b>b</b>, <b>c</b>, <b>d</b>. The parameter <b>t</b> may range from 0 (the start of the curve) and 1 (the end of the curve). <b>a</b> and <b>d</b> are points on the curve, and <b>b</b> and <b>c</b> are the control points. This can be used once with the <b>x</b> coordinates and a second time with the <b>y</b> coordinates to get the location of a curve at <b>t</b>.")
     ("curveTangent()" "Calculates the tangent of a point on a curve. There's a good definition of <a href=\"http://en.wikipedia.org/wiki/Tangent\" target=\"new\"><em>tangent</em> on Wikipedia</a>.")
     ("curveTightness()" "Modifies the quality of forms created with <b>curve()</b> and <b>curveVertex()</b>. The parameter <b>tightness</b> determines how the curve fits to the vertex points. The value 0.0 is the default value for <b>tightness</b> (this value defines the curves to be Catmull-Rom splines) and the value 1.0 connects all the points with straight lines. Values within the range -5.0 and 5.0 will deform the curves but will leave them recognizable and as values increase in magnitude, they will continue to deform.")
     ("arc()" "Draws an arc to the screen. Arcs are drawn along the outer edge of an ellipse defined by the <b>a</b>, <b>b</b>, <b>c</b>, and <b>d</b> parameters. The origin of the arc's ellipse may be changed with the <b>ellipseMode()</b> function. Use the <b>start</b> and <b>stop</b> parameters to specify the angles (in radians) at which to draw the arc.<br />
<br />
There are three ways to draw an arc; the rendering technique used is defined by the optional seventh paramter. The three options, depicted in the above examples, are PIE, OPEN, and CHORD. The default mode is the OPEN stroke with a PIE fill.")
     ("ellipse()" "Draws an ellipse (oval) to the screen. An ellipse with equal width and height is a circle. By default, the first two parameters set the location, and the third and fourth parameters set the shape's width and height. The origin may be changed with the <b>ellipseMode()</b> function.")
     ("line()" "Draws a line (a direct path between two points) to the screen. The version of <b>line()</b> with four parameters draws the line in 2D.  To color a line, use the <b>stroke()</b> function. A line cannot be filled, therefore the <b>fill()</b> function will not affect the color of a line. 2D lines are drawn with a width of one pixel by default, but this can be changed with the <b>strokeWeight()</b> function. The version with six parameters allows the line to be placed anywhere within XYZ space. Drawing this shape in 3D with the <b>z</b> parameter requires the P3D parameter in combination with <b>size()</b> as shown in the above example.")
     ("point()" "Draws a point, a coordinate in space at the dimension of one pixel. The first parameter is the horizontal value for the point, the second value is the vertical value for the point, and the optional third value is the depth value. Drawing this shape in 3D with the <b>z</b> parameter requires the P3D parameter in combination with <b>size()</b> as shown in the above example.")
     ("quad()" "A quad is a quadrilateral, a four sided polygon. It is similar to a rectangle, but the angles between its edges are not constrained to ninety degrees. The first pair of parameters (x1,y1) sets the first vertex and the subsequent pairs should proceed clockwise or counter-clockwise around the defined shape.")
     ("rect()" "Draws a rectangle to the screen. A rectangle is a four-sided shape with every angle at ninety degrees. By default, the first two parameters set the location of the upper-left corner, the third sets the width, and the fourth sets the height. The way these parameters are interpreted, however, may be changed with the <b>rectMode()</b> function.<br />
<br />
To draw a rounded rectangle, add a fifth parameter, which is used as the radius value for all four corners.<br/>
<br/>
To use a different radius value for each corner, include eight parameters. When using eight parameters, the latter four set the radius of the arc at each corner separately, starting with the top-left corner and moving clockwise around the rectangle.")
     ("triangle()" "A triangle is a plane created by connecting three points. The first two arguments specify the first point, the middle two arguments specify the second point, and the last two arguments specify the third point.")
     ("print()" "The <b>print()</b> function writes to the console area, the black rectangle at the bottom of the Processing environment. This function is often helpful for looking at the data a program is producing. The companion function <b>println()</b> works like <b>print()</b>, but creates a new line of text for each call to the function. More than one parameter can be passed into the function by separating them with commas. Alternatively, individual elements can be separated with quotes (\"\") and joined with the addition operator (+).<br />
<br />
Using <b>print()</b> on an object will output <b>null</b>, a memory location that may look like \"@10be08,\" or the result of the <b>toString()</b> method from the object that's being printed. Advanced users who want more useful output when calling <b>print()</b> on their own classes can add a <b>toString()</b> method to the class that returns a String.")
     ("printArray()" "The <b>printArray()</b> function writes array data to the text area of the Processing environment's console. A new line is put between each element of the array. This function can only print one dimensional arrays.")
     ("println()" "The <b>print()</b> function writes to the console area, the black rectangle at the bottom of the Processing environment. This function is often helpful for looking at the data a program is producing. Each call to this function creates a new line of output. More than one parameter can be passed into the function by separating them with commas. Alternatively, individual elements can be separated with quotes (\"\") and joined with the addition operator (+).<br />
<br/>
Before Processing 2.1, <b>println()</b> was used to write array data to the console. Now, use <b>printArray()</b> to write array data to the console.")
     ("textAscent()" "Returns ascent of the current font at its current size. This information is useful for determining the height of the font above the baseline. For example, adding the <b>textAscent()</b> and <b>textDescent()</b> values will give you the total height of the line.")
     ("textDescent()" "Returns descent of the current font at its current size. This information is useful for determining the height of the font below the baseline. For example, adding the <b>textAscent()</b> and <b>textDescent()</b> values will give you the total height of the line.")
     ("join()" "Combines an array of Strings into one String, each separated by the character(s) used for the <b>separator</b> parameter. To join arrays of ints or floats, it's necessary to first convert them to Strings using <b>nf()</b> or <b>nfs()</b>.")
     ("match()" "This function is used to apply a regular expression to a piece of text, and return matching groups (elements found inside parentheses) as a String array. If there are no matches, a null value will be returned. If no groups are specified in the regular expression, but the sequence matches, an array of length 1 (with the matched text as the first element of the array) will be returned.<br/>
<br/>
To use the function, first check to see if the result is null. If the result is null, then the sequence did not match at all. If the sequence did match, an array is returned.<br/>
<br/>
If there are groups (specified by sets of parentheses) in the regular expression, then the contents of each will be returned in the array. Element [0] of a regular expression match returns the entire matching string, and the match groups start at element [1] (the first group is [1], the second [2], and so on).<br/>
<br/>
The syntax can be found in the reference for Java's <a href=\"http://download.oracle.com/javase/6/docs/api/\">Pattern</a> class. For regular expression syntax, read the <a href=\"http://download.oracle.com/javase/tutorial/essential/regex/\">Java Tutorial</a> on the topic.")
     ("matchAll()" "This function is used to apply a regular expression to a piece of text, and return a list of matching groups (elements found inside parentheses) as a two-dimensional String array. If there are no matches, a null value will be returned. If no groups are specified in the regular expression, but the sequence matches, a two dimensional array is still returned, but the second dimension is only of length one.<br />
<br />
To use the function, first check to see if the result is null. If the result is null, then the sequence did not match at all. If the sequence did match, a 2D array is returned.<br/>
<br/>
If there are groups (specified by sets of parentheses) in the regular expression, then the contents of each will be returned in the array. Assuming a loop with counter variable i, element [i][0] of a regular expression match returns the entire matching string, and the match groups start at element [i][1] (the first group is [i][1], the second [i][2], and so on).<br/>
<br/>
The syntax can be found in the reference for Java's <a href=\"http://download.oracle.com/javase/6/docs/api/\">Pattern</a> class. For regular expression syntax, read the <a href=\"http://download.oracle.com/javase/tutorial/essential/regex/\">Java Tutorial</a> on the topic.")
     ("nf()" "Utility function for formatting numbers into strings. There are two versions: one for formatting floats, and one for formatting ints. The values for the <b>digits</b>, <b>left</b>, and <b>right</b> parameters should always be positive integers.<br /><br />As shown in the above example, <b>nf()</b> is used to add zeros to the left and/or right of a number. This is typically for aligning a list of numbers. To <em>remove</em> digits from a floating-point number, use the <b>int()</b>, <b>ceil()</b>, <b>floor()</b>, or <b>round()</b> functions.")
     ("nfc()" "Utility function for formatting numbers into strings and placing appropriate commas to mark units of 1000. There are two versions: one for formatting ints, and one for formatting an array of ints. The value for the <b>right</b> parameter should always be a positive integer.
<br/> <br/>
For a non-US locale, this will insert periods instead of commas, or whatever is apprioriate for that region.")
     ("nfp()" "Utility function for formatting numbers into strings. Similar to <b>nf()</b> but puts a \"+\" in front of positive numbers and a \"-\" in front of negative numbers. There are two versions: one for formatting floats, and one for formatting ints. The values for the <b>digits</b>, <b>left</b>, and <b>right</b> parameters should always be positive integers.")
     ("nfs()" "Utility function for formatting numbers into strings. Similar to <b>nf()</b>, but leaves a blank space in front of positive numbers so they align with negative numbers in spite of the minus symbol. There are two versions: one for formatting floats, and one for formatting ints. The values for the <b>digits</b>, <b>left</b>, and <b>right</b> parameters should always be positive integers.")
     ("split()" "The <b>split()</b> function breaks a String into pieces using a character or string as the delimiter. The <b>delim</b> parameter specifies the character or characters that mark the boundaries between each piece. A String[] array is returned that contains each of the pieces.
<br/> <br/>
If the result is a set of numbers, you can convert the String[] array to to a float[] or int[] array using the datatype conversion functions <b>int()</b> and <b>float()</b>.  (See the second example above.)
<br/> <br/> 
The <b>splitTokens()</b> function works in a similar fashion, except that it splits using a range of characters instead of a specific character or sequence.
<!--
<br /><br />
This function uses regular expressions to determine how the <b>delim</b> parameter divides the <b>str</b> parameter. Therefore, if you use characters such parentheses and brackets that are used with regular expressions as a part of the <b>delim</b> parameter, you'll need to put two blackslashes (\\\\\\\\) in front of the character (see example above). You can read more about <a href=\"http://en.wikipedia.org/wiki/Regular_expression\">regular expressions</a> and <a href=\"http://en.wikipedia.org/wiki/Escape_character\">escape characters</a> on Wikipedia.
-->")
     ("splitTokens()" "The <b>splitTokens()</b> function splits a String at one or many character delimiters or \"tokens.\" The <b>delim</b> parameter specifies the character or characters to be used as a boundary.<br/>
<br/>
If no <b>delim</b> characters are specified, any whitespace character is used to split. Whitespace characters include tab (&#92;t), line feed (&#92;n), carriage return (&#92;r), form feed (&#92;f), and space.<br/>
<br/>
After using this function to parse incoming data, it is common to convert the data from Strings to integers or floats by using the datatype conversion functions <b>int()</b> and <b>float()</b>.")
     ("trim()" "Removes whitespace characters from the beginning and end of a String. In addition to standard whitespace characters such as space, carriage return, and tab, this function also removes the Unicode \"nbsp\" character.")
     ("append()" "Expands an array by one element and adds data to the new position. The datatype of the <b>element</b> parameter must be the same as the datatype of the array.<br/>
<br/>
When using an array of objects, the data returned from the function must be cast to the object array's data type. For example: <em>SomeClass[] items = (SomeClass[]) append(originalArray, element)</em>")
     ("arrayCopy()" "Copies an array (or part of an array) to another array. The <b>src</b> array is copied to the <b>dst</b> array, beginning at the position specified by <b>srcPosition</b> and into the position specified by <b>dstPosition</b>. The number of elements to copy is determined by <b>length</b>. Note that copying values overwrites existing values in the destination array. To append values instead of overwriting them, use <b>concat()</b>.<br/>
<br/>
The simplified version with only two arguments &mdash; <b>arrayCopy(src, dst)</b> &mdash; copies an entire array to another of the same size. It is equivalent to <b>arrayCopy(src, 0, dst, 0, src.length)</b>.<br/>
<br/>
Using this function is far more efficient for copying array data than iterating through a <b>for()</b> loop and copying each element individually.
<br/>
This function only copies references, which means that for most purposes it only copies one-dimensional arrays (a single set of brackets). If used with a two (or three or more) dimensional array, it will only copy the references at the first level, because a two dimensional array is simply an \"array of arrays\". This does not produce an error, however, because this is often the desired behavior. 
<br/>
Internally, this function calls Java's <a href=\"http://docs.oracle.com/javase/7/docs/api/java/lang/System.html#arraycopy(java.lang.Object, int, java.lang.Object, int, int)\">System.arraycopy()</a> method, so most things that apply there are inherited.")
     ("concat()" "Concatenates two arrays. For example, concatenating the array { 1, 2, 3 } and the array { 4, 5, 6 } yields { 1, 2, 3, 4, 5, 6 }. Both parameters must be arrays of the same datatype.
<br/> <br/>
When using an array of objects, the data returned from the function must be cast to the object array's data type. For example: <em>SomeClass[] items = (SomeClass[]) concat(array1, array2)</em>.")
     ("expand()" "Increases the size of an array. By default, this function doubles the size of the array, but the optional <b>newSize</b> parameter provides precise control over the increase in size. 
<br/> <br/>
When using an array of objects, the data returned from the function must be cast to the object array's data type. For example: <em>SomeClass[] items = (SomeClass[]) expand(originalArray)</em>")
     ("reverse()" "Reverses the order of an array.")
     ("shorten()" "Decreases an array by one element and returns the shortened array.
<br/> <br/>
When using an array of objects, the data returned from the function must be cast to the object array's data type. For example: <em>SomeClass[] items = (SomeClass[]) shorten(originalArray)</em>")
     ("sort()" "Sorts an array of numbers from smallest to largest, or puts an array of words in alphabetical order. The original array is not modified; a re-ordered array is returned. The <b>count</b> parameter states the number of elements to sort. For example, if there are 12 elements in an array and <b>count</b> is set to 5, only the first 5 elements in the array will be sorted. <!--As of release 0126, the alphabetical ordering is case insensitive.-->")
     ("splice()" "Inserts a value or an array of values into an existing array. The first two parameters must be arrays of the same datatype. The first parameter specifies the intial array to be modified, and the second parameter defines the data to be inserted. The third parameter is an index value which specifies the array position from which to insert data. (Remember that array index numbering starts at zero, so the first position is 0, the second position is 1, and so on.)<br/>
<br/>
When splicing an array of objects, the data returned from the function must be cast to the object array's data type. For example: <em>SomeClass[] items = (SomeClass[]) splice(array1, array2, index)</em>")
     ("subset()" "Extracts an array of elements from an existing array. The <b>list</b> parameter defines the array from which the elements will be copied, and the <b>start</b> and <b>count</b> parameters specify which elements to extract. If no <b>count</b> is given, elements will be extracted from the <b>start</b> to the end of the array. When specifying the <b>start</b>, remember that the first array element is 0. This function does not change the source array.<br/>
<br/>
When using an array of objects, the data returned from the function must be cast to the object array's data type. For example: <em>SomeClass[] items = (SomeClass[]) subset(originalArray, 0, 4)</em>")
     ("save()" "Saves an image from the display window. Append a file extension to the name of the file, to indicate the file format to be used: either TIFF (.tif), TARGA (.tga), JPEG (.jpg), or PNG (.png). If no extension is included in the filename, the image will save in TIFF format and <b>.tif</b> will be added to the name. These files are saved to the sketch's folder, which may be opened by selecting \"Show sketch folder\" from the \"Sketch\" menu. Alternatively, the files can be saved to any location on the computer by using an absolute path (something that starts with / on Unix and Linux, or a drive letter on Windows).<br/>
<br/>
All images saved from the main drawing window will be opaque. To save images without a background, use <b>createGraphics()</b>.")
     ("saveFrame()" "Saves a numbered sequence of images, one image each time the function is run. To save an image that is identical to the display window, run the function at the end of <b>draw()</b> or within mouse and key events such as <b>mousePressed()</b> and <b>keyPressed()</b>. Use the Movie Maker program in the Tools menu to combine these images to a movie.<br />
<br />
If <b>saveFrame()</b> is used without parameters, it will save files as screen-0000.tif, screen-0001.tif, and so on. You can specify the name of the sequence with the <b>filename</b> parameter, including hash marks (####), which will be replaced by the current <b>frameCount</b> value. (The number of hash marks is used to determine how many digits to include in the file names.) Append a file extension, to indicate the file format to be used: either TIFF (.tif), TARGA (.tga), JPEG (.jpg), or PNG (.png). Image files are saved to the sketch's folder, which may be opened by selecting \"Show Sketch Folder\" from the \"Sketch\" menu.<br />
<br />
Alternatively, the files can be saved to any location on the computer by using an absolute path (something that starts with / on Unix and Linux, or a drive letter on Windows).<br/>
<br />
All images saved from the main drawing window will be opaque. To save images without a background, use <b>createGraphics()</b>.")
     ("blend()" "Blends a region of pixels from one image into another (or in itself again) with full alpha channel support. There is a choice of the following modes to blend the source pixels (A) with the ones of pixels in the destination image (B):<br />
<br />
BLEND - linear interpolation of colours: C = A*factor + B<br />
<br />
ADD - additive blending with white clip: C = min(A*factor + B, 255)<br />
<br />
SUBTRACT - subtractive blending with black clip: C = max(B - A*factor, 0)<br />
<br />
DARKEST - only the darkest colour succeeds: C = min(A*factor, B)<br />
<br />
LIGHTEST - only the lightest colour succeeds: C = max(A*factor, B)<br />
<br />
DIFFERENCE - subtract colors from underlying image.<br />
<br />
EXCLUSION - similar to DIFFERENCE, but less extreme.<br />
<br />
MULTIPLY - Multiply the colors, result will always be darker.<br />
<br />
SCREEN - Opposite multiply, uses inverse values of the colors.<br />
<br />
OVERLAY - A mix of MULTIPLY and SCREEN. Multiplies dark values,
and screens light values.<br />
<br />
HARD_LIGHT - SCREEN when greater than 50% gray, MULTIPLY when lower.<br />
<br />
SOFT_LIGHT - Mix of DARKEST and LIGHTEST. 
Works like OVERLAY, but not as harsh.<br />
<br />
DODGE - Lightens light tones and increases contrast, ignores darks.
Called \"Color Dodge\" in Illustrator and Photoshop.<br />
<br />
BURN - Darker areas are applied, increasing contrast, ignores lights.
Called \"Color Burn\" in Illustrator and Photoshop.<br />
<br />
All modes use the alpha information (highest byte) of source image pixels as the blending factor. If the source and destination regions are different sizes, the image will be automatically resized to match the destination size. If the <b>srcImg</b> parameter is not used, the display window is used as the source image.<br />
<br />
As of release 0149, this function ignores <b>imageMode()</b>.")
     ("copy()" "Copies a region of pixels from the display window to another area of the display window and copies a region of pixels from an image used as the <b>srcImg</b> parameter into the display window. If the source and destination regions aren't the same size, it will automatically resize the source pixels to fit the specified target region. No alpha information is used in the process, however if the source image has an alpha channel set, it will be copied as well.
<br /><br />
As of release 0149, this function ignores <b>imageMode()</b>.")
     ("filter()" "Filters the display window using a preset filter or with a custom shader. Using a shader with <b>filter()</b> is much faster than without. Shaders require the P2D or P3D renderer in size().<br />
<br />
The presets options are:<br />
<br />
THRESHOLD<br />
Converts the image to black and white pixels depending if they are above or below the threshold defined by the level parameter. The parameter must be between 0.0 (black) and 1.0 (white). If no level is specified, 0.5 is used.<br />
<br />
GRAY<br />
Converts any colors in the image to grayscale equivalents. No parameter is used.<br />
<br />
OPAQUE<br />
Sets the alpha channel to entirely opaque. No parameter is used.<br />
<br />
INVERT<br />
Sets each pixel to its inverse value. No parameter is used.<br />
<br />
POSTERIZE<br />
Limits each channel of the image to the number of colors specified as the parameter. The parameter can be set to values between 2 and 255, but results are most noticeable in the lower ranges.<br />
<br />
BLUR<br />
Executes a Guassian blur with the level parameter specifying the extent of the blurring. If no parameter is used, the blur is equivalent to Guassian blur of radius 1. Larger values increase the blur.<br />
<br />
ERODE<br />
Reduces the light areas. No parameter is used.<br />
<br />
DILATE<br />
Increases the light areas. No parameter is used.")
     ("get()" "Reads the color of any pixel or grabs a section of an image. If no parameters are specified, the entire image is returned. Use the <b>x</b> and <b>y</b> parameters to get the value of one pixel. Get a section of the display window by specifying additional <b>w</b> and <b>h</b> parameters. When getting an image, the <b>x</b> and <b>y</b> parameters define the coordinates for the upper-left corner of the image, regardless of the current <b>imageMode()</b>.<br />
<br />
If the pixel requested is outside of the image window, black is returned. The numbers returned are scaled according to the current color ranges, but only RGB values are returned by this function. For example, even though you may have drawn a shape with <b>colorMode(HSB)</b>, the numbers returned will be in RGB format.
<br /><br />
Getting the color of a single pixel with <b>get(x, y)</b> is easy, but not as fast as grabbing the data directly from <b>pixels[]</b>. The equivalent statement to <b>get(x, y)</b> using <b>pixels[]</b> is <b>pixels[y*width+x]</b>. See the reference for <a href=\"pixels.html\">pixels[]</a> for more information.")
     ("loadPixels()" "Loads the pixel data for the display window into the <b>pixels[]</b> array. This function must always be called before reading from or writing to <b>pixels[]</b>.<br/>
<br/>
Certain renderers may or may not seem to require <b>loadPixels()</b> or <b>updatePixels()</b>. However, the rule is that any time you want to manipulate the <b>pixels[]</b> array, you must first call <b>loadPixels()</b>, and after changes have been made, call <b>updatePixels()</b>. Even if the renderer may not seem to use this function in the current Processing release, this will always be subject to change.")
     ("set()" "Changes the color of any pixel, or writes an image directly to the display window.<br />
<br />
The <b>x</b> and <b>y</b> parameters specify the pixel to change and the <b>c</b> parameter specifies the color value. The <b>c</b> parameter is interpreted according to the current color mode.  (The default color mode is RGB values from 0 to 255.)  When setting an image, the <b>x</b> and <b>y</b> parameters define the coordinates for the upper-left corner of the image, regardless of the current <b>imageMode()</b>.
<br /><br />
Setting the color of a single pixel with <b>set(x, y)</b> is easy, but not as fast as putting the data directly into <b>pixels[]</b>. The equivalent statement to <b>set(x, y, #000000)</b> using <b>pixels[]</b> is <b>pixels[y*width+x] = #000000</b>. See the reference for <a href=\"pixels.html\">pixels[]</a> for more information.")
     ("updatePixels()" "Updates the display window with the data in the <b>pixels[]</b> array. Use in conjunction with <b>loadPixels()</b>. If you're only reading pixels from the array, there's no need to call <b>updatePixels()</b> &mdash; updating is only necessary to apply changes.
<br/><br/>
Certain renderers may or may not seem to require <b>loadPixels()</b> or <b>updatePixels()</b>. However, the rule is that any time you want to manipulate the <b>pixels[]</b> array, you must first call <b>loadPixels()</b>, and after changes have been made, call <b>updatePixels()</b>. Even if the renderer may not seem to use this function in the current Processing release, this will always be subject to change.
<br/><br/>
Currently, while none of the renderers use the additional parameters to <b>updatePixels()</b>, this may be implemented in the future.")
     ("textAlign()" "Sets the current alignment for drawing text. The parameters LEFT, CENTER, and RIGHT set the display characteristics of the letters in relation to the values for the <b>x</b> and <b>y</b> parameters of the <b>text()</b> function.
<br/> <br/>
An optional second parameter can be used to vertically align the text. BASELINE is the default, and the vertical alignment will be reset to BASELINE if the second parameter is not used. The TOP and CENTER parameters are straightforward. The BOTTOM parameter offsets the line based on the current <b>textDescent()</b>. For multiple lines, the final line will be aligned to the bottom, with the previous lines appearing above it.
<br/> <br/>
When using <b>text()</b> with width and height parameters, BASELINE is ignored, and treated as TOP. (Otherwise, text would by default draw outside the box, since BASELINE is the default setting. BASELINE is not a useful drawing mode for text drawn in a rectangle.)
<br/> <br/>
The vertical alignment is based on the value of <b>textAscent()</b>, which many fonts do not specify correctly. It may be necessary to use a hack and offset by a few pixels by hand so that the offset looks correct. To do this as less of a hack, use some percentage of <b>textAscent()</b> or <b>textDescent()</b> so that the hack works even if you change the size of the font.")
     ("textLeading()" "Sets the spacing between lines of text in units of pixels. This setting will be used in all subsequent calls to the <b>text()</b> function.  Note, however, that the leading is reset by <b>textSize()</b>. For example, if the leading is set to 20 with <b>textLeading(20)</b>, then if <b>textSize(48)</b> is run at a later point, the leading will be reset to the default for the text size of 48.")
     ("textMode()" "Sets the way text draws to the screen, either as texture maps or as vector geometry. The default <b>textMode(MODEL)</b>, uses textures to render the fonts. The <b>textMode(SHAPE)</b> mode draws text using the glyph outlines of individual characters rather than as textures. This mode is only supported with the <b>PDF</b> and <b>P3D</b> renderer settings. With the <b>PDF</b> renderer, you must call <b>textMode(SHAPE)</b> before any other drawing occurs. If the outlines are not available, then <b>textMode(SHAPE)</b> will be ignored and <b>textMode(MODEL)</b> will be used instead.<br />
<br />
The <b>textMode(SHAPE)</b> option in <b>P3D</b> can be combined with <b>beginRaw()</b> to write vector-accurate text to 2D and 3D output files, for instance <b>DXF</b> or <b>PDF</b>. The <b>SHAPE</b> mode is not currently optimized for <b>P3D</b>, so if recording shape data, use <b>textMode(MODEL)</b> until you're ready to capture the geometry with <b>beginRaw()</b>.")
     ("textSize()" "Sets the current font size. This size will be used in all subsequent calls to the <b>text()</b> function. Font size is measured in units of pixels.")
     ("textWidth()" "Calculates and returns the width of any character or text string.")
     ("image()" "The <b>image()</b> function draws an image to the display window. Images must be in the sketch's \"data\" directory to load correctly. Select \"Add file...\" from the \"Sketch\" menu to add the image to the data directory, or just drag the image file onto the sketch window. Processing currently works with GIF, JPEG, and PNG images. <br />
<br />
The <b>img</b> parameter specifies the image to display and by default the <b>a</b> and <b>b</b> parameters define the location of its upper-left corner. The image is displayed at its original size unless the <b>c</b> and <b>d</b> parameters specify a different size. The <b>imageMode()</b> function can be used to change the way these parameters draw the image.<br/>
<br/>
The color of an image may be modified with the <b>tint()</b> function. This function will maintain transparency for GIF and PNG images.")
     ("imageMode()" "Modifies the location from which images are drawn by changing the way in which parameters given to <b>image()</b> are intepreted.<br/>
<br/>
The default mode is <b>imageMode(CORNER)</b>, which interprets the second and third parameters of <b>image()</b> as the upper-left corner of the image. If two additional parameters are specified, they are used to set the image's width and height.<br/>
<br/>
<b>imageMode(CORNERS)</b> interprets the second and third parameters of <b>image()</b> as the  location of one corner, and the fourth and fifth parameters as the opposite corner.<br/>
<br/>
<b>imageMode(CENTER)</b> interprets the second and third parameters of <b>image()</b> as the image's center point. If two additional parameters are specified, they are used to set the image's width and height.<br/>
<br/>
The parameter must be written in ALL CAPS because Processing is a case-sensitive language.")
     ("loadImage()" "Loads an image into a variable of type <b>PImage</b>. Four types of images ( <b>.gif</b>, <b>.jpg</b>, <b>.tga</b>, <b>.png</b>) images may be loaded. To load correctly, images must be located in the data directory of the current sketch.<br>
<br />
In most cases, load all images in <b>setup()</b> to preload them at the start of the program. Loading images inside <b>draw()</b> will reduce the speed of a program. Images cannot be loaded outside <b>setup()</b> unless they're inside a function that's called after <b>setup()</b> has already run.<br>
<br/>
Alternatively, the file maybe be loaded from anywhere on the local computer using an absolute path (something that starts with / on Unix and Linux, or a drive letter on Windows), or the filename parameter can be a URL for a file found on a network.<br />
<br />
If the file is not available or an error occurs, <b>null</b> will be returned and an error message will be printed to the console. The error message does not halt the program, however the null value may cause a NullPointerException if your code does not check whether the value returned is null.<br/>
<br />
The <b>extension</b> parameter is used to determine the image type in cases where the image filename does not end with a proper extension. Specify the extension as the second parameter to <b>loadImage()</b>, as shown in the third example on this page.<br/> 
<br/>
Depending on the type of error, a <b>PImage</b> object may still be returned, but the width and height of the image will be set to -1. This happens if bad image data is returned or cannot be decoded properly. Sometimes this happens with image URLs that produce a 403 error or that redirect to a password prompt, because <b>loadImage()</b> will attempt to interpret the HTML as image data.")
     ("noTint()" "Removes the current fill value for displaying images and reverts to displaying images with their original hues.")
     ("requestImage()" "This function loads images on a separate thread so that your sketch doesn't freeze while images load during <b>setup()</b>. While the image is loading, its width and height will be 0. If an error occurs while loading the image, its width and height will be set to -1. You'll know when the image has loaded properly because its <b>width</b> and <b>height</b> will be greater than 0. Asynchronous image loading (particularly when downloading from a server) can dramatically improve performance.<br />
<br/>
The <b>extension</b> parameter is used to determine the image type in cases where the image filename does not end with a proper extension. Specify the extension as the second parameter to <b>requestImage()</b>.")
     ("tint()" "Sets the fill value for displaying images. Images can be tinted to specified colors or made transparent by including an alpha value.<br /> 
<br />
To apply transparency to an image without affecting its color, use white as the tint color and specify an alpha value. For instance, tint(255, 128) will make an image 50% transparent (assuming the default alpha range of 0-255, which can be changed with <b>colorMode()</b>).
<br /><br />
When using hexadecimal notation to specify a color, use \"#\" or \"0x\" before the values (e.g. #CCFFAA, 0xFFCCFFAA). The # syntax uses six digits to specify a color (the way colors are specified in HTML and CSS). When using the hexadecimal notation starting with \"0x\", the hexadecimal value must be specified with eight characters; the first two characters define the alpha component and the remainder the red, green, and blue components.
<br /><br />
The value for the gray parameter must be less than or equal to the current maximum value as specified by <b>colorMode()</b>. The default maximum value is 255.
<br /><br />
The <b>tint()</b> function is also used to control the coloring of textures in 3D.")
     ("createFont()" "Dynamically converts a font to the format used by Processing from a .ttf or .otf file inside the sketch's \"data\" folder or a font that's installed elsewhere on the computer. If you want to use a font installed on your computer, use the <b>PFont.list()</b> method to first determine the names for the fonts recognized by the computer and are compatible with this function. Not all fonts can be used and some might work with one operating system and not others. When sharing a sketch with other people or posting it on the web, you may need to include a .ttf or .otf version of your font in the data directory of the sketch because other people might not have the font installed on their computer. Only fonts that can legally be distributed should be included with a sketch.<br />
<br />
The <b>size</b> parameter states the font size you want to generate. The <b>smooth</b> parameter specifies if the font should be antialiased or not. The <b>charset</b> parameter is an array of chars that specifies the characters to generate.<br />
<br />
This function allows Processing to work with the font natively in the default renderer, so the letters are defined by vector geometry and are rendered quickly. In the <b>P2D</b> and <b>P3D</b> renderers, the function sets the project to render the font as a series of small textures. For instance, when using the default renderer, the actual native version of the font will be employed by the sketch, improving drawing quality and performance. With the <b>P2D</b> and <b>P3D</b> renderers, the bitmapped version will be used to improve speed and appearance, but the results are poor when exporting if the sketch does not include the .otf or .ttf file, and the requested font is not available on the machine running the sketch.")
     ("loadFont()" "Loads a .vlw formatted font into a <b>PFont</b> object. Create a .vlw font by selecting \"Create Font...\" from the Tools menu. This tool creates a texture for each alphanumeric character and then adds them as a .vlw file to the current sketch's data folder. Because the letters are defined as textures (and not vector data) the size at which the fonts are created must be considered in relation to the size at which they are drawn. For example, load a 32pt font if the sketch displays the font at 32 pixels or smaller. Conversely, if a 12pt font is loaded and displayed at 48pts, the letters will be distorted because the program will be stretching a small graphic to a large size.<br />
<br />
Like <b>loadImage()</b> and other functions that load data, the <b>loadFont()</b> function should not be used inside <b>draw()</b>, because it will slow down the sketch considerably, as the font will be re-loaded from the disk (or network) on each frame. It's recommended to load files inside <b>setup()</b><br />
<br />
To load correctly, fonts must be located in the \"data\" folder of the current sketch. Alternatively, the file maybe be loaded from anywhere on the local computer using an absolute path (something that starts with / on Unix and Linux, or a drive letter on Windows), or the filename parameter can be a URL for a file found on a network.<br />
<br />
If the file is not available or an error occurs, <b>null</b> will be returned and an error message will be printed to the console. The error message does not halt the program, however the null value may cause a NullPointerException if your code does not check whether the value returned is null.<br/>
<br />
Use <b>createFont()</b> (instead of <b>loadFont()</b>) to enable vector data to be used with the default renderer setting. This can be helpful when many font sizes are needed, or when using any renderer based on the default renderer, such as the PDF library.")
     ("text()" "Draws text to the screen. Displays the information specified in the first parameter on the screen in the position specified by the additional parameters. A default font will be used unless a font is set with the <b>textFont()</b> function and a default size will be used unless a font is set with <b>textSize()</b>. Change the color of the text with the <b>fill()</b> function. The text displays in relation to the <b>textAlign()</b> function, which gives the option to draw to the left, right, and center of the coordinates.<br/>
<br/>
The <b>x2</b> and <b>y2</b> parameters define a rectangular area to display within and may only be used with string data. When these parameters are specified, they are interpreted based on the current <b>rectMode()</b> setting. Text that does not fit completely within the rectangle specified will not be drawn to the screen.<br/>
<br/>
Note that Processing now lets you call <b>text()</b> without first specifying a PFont with <b>textFont()</b>. In that case, a generic sans-serif font will be used instead. (See the third example above.)")
     ("textFont()" "Sets the current font that will be drawn with the <b>text()</b> function. Fonts must be created for Processing with <b>createFont()</b> or loaded with <b>loadFont()</b> before they can be used. The font set through <b>textFont()</b> will be used in all subsequent calls to the <b>text()</b> function. If no <b>size</b> parameter is input, the font will appear at its original size (the size in which it was created with the \"Create Font...\" tool) until it is changed with <b>textSize()</b>.<br />
<br /> Because fonts are usually bitmapped, you should create fonts at the sizes that will be used most commonly. Using <b>textFont()</b> without the size parameter will result in the cleanest type. <br />
<br />
With the default and PDF renderers, it's also possible to enable the use of native fonts via the command <b>hint(ENABLE_NATIVE_FONTS)</b>. This will produce vector text in both on-screen sketches and PDF output when the vector data is available, such as when the font is still installed, or the font is created dynamically via the <b>createFont()</b> function (rather than with the \"Create Font...\" tool).")
     ("PShader.set" "Sets the uniform variables inside the shader to modify the effect while the program is running.")
     ("loadShader()" "Loads a shader into the PShader object. The shader file must be loaded in the sketch's \"data\" folder/directory to load correctly. Shaders are compatible with the P2D and P3D renderers, but not with the default renderer.<br />
<br />
Alternatively, the file maybe be loaded from anywhere on the local computer using an absolute path (something that starts with / on Unix and Linux, or a drive letter on Windows), or the filename parameter can be a URL for a file found on a network.<br />
<br />
If the file is not available or an error occurs, <b>null</b> will be returned and an error message will be printed to the console. The error message does not halt the program, however the null value may cause a NullPointerException if your code does not check whether the value returned is null.<br/>")
     ("resetShader()" "Restores the default shaders. Code that runs after <b>resetShader()</b> will not be affected by previously defined shaders.")
     ("shader()" "Applies the shader specified by the parameters. It's compatible with the P2D and P3D renderers, but not with the default renderer.")
     ("PFont.list" "Gets a list of the fonts installed on the system. The data is returned as a String array. This list provides the names of each font for input into <b>createFont()</b>, which allows Processing to dynamically format fonts.")
     ("createImage()" "Creates a new PImage (the datatype for storing images). This provides a fresh buffer of pixels to play with. Set the size of the buffer with the <b>width</b> and <b>height</b> parameters. The <b>format</b> parameter defines how the pixels are stored. See the PImage reference for more information.
<br/> <br/>
Be sure to include all three parameters, specifying only the width and height (but no format) will produce a strange error.
<br/> <br/>
Advanced users please note that createImage() should be used instead of the syntax <tt>new PImage()</tt>.")
     ("PShape.addChild()" "This is a new reference entry for Processing 2.0. It will be updated shortly. For now, please check the Examples to see how to use this new syntax.")
     ("PShape.beginContour()" "The <b>beginContour()</b> and <b>endContour()</b> methods make it possible to define shapes with other shapes cut out of them. For example, the inside of a letter 'O'. These two functions are always used together, you'll never use one without the other. Between them, define the geometry you want to create. As you'll see when you run the example above, the second smaller shape is cut out of the first larger shape.")
     ("PShape.beginShape()" "This method is used to start a custom shape created with the <b>createShape()</b> function. It's always and only used with <b>createShape()</b>.")
     ("PShape.disableStyle()" "Disables the shape's style data and uses Processing's current styles. Styles include attributes such as colors, stroke weight, and stroke joints.")
     ("PShape.enableStyle()" "Enables the shape's style data and ignores Processing's current styles. Styles include attributes such as colors, stroke weight, and stroke joints.")
     ("PShape.endContour()" "The <b>beginContour()</b> and <b>endContour()</b> methods make it possible to define shapes with other shapes cut out of them. For example, the inside of a letter 'O'. These two functions are always used together, you'll never use one without the other. Between them, define the geometry you want to create. As you'll see when you run the example above, the second smaller shape is cut out of the first larger shape.")
     ("PShape.endShape()" "This method is used to complete a custom shape created with the <b>createShape()</b> function. It's always and only used with <b>createShape()</b>.")
     ("PShape.getChild()" "Extracts a child shape from a parent shape. Specify the name of the shape with the <b>target</b> parameter. The shape is returned as a <b>PShape</b> object, or <b>null</b> is returned if there is an error.")
     ("PShape.getChildCount()" "Returns the number of children within the PShape.")
     ("PShape.getVertex()" "The <b>getVertex()</b> method returns a PVector with the coordinates of the vertex point located at the position defined by the <b>index</b> parameter. This method works when shapes are created as shown in the example above, but won't work properly when a shape is defined explicitly (e.g. createShape(RECT, 20, 20, 80, 80).")
     ("PShape.getVertexCount()" "The <b>getVertexCount()</b> method returns the number of vertices that make up a PShape. In the above example, the value 4 is returned by the <b>getVertexCount()</b> method because 4 vertices are defined in <b>setup()</b>.")
     ("PShape.height" "The height of the PShape document.")
     ("PShape.isVisible()" "Returns a boolean value \"true\" if the image is set to be visible, \"false\" if not. This value can be modified with the <b>setVisible()</b> method.<br/>
<br/>
The default visibility of a shape is usually controlled by whatever program created the SVG file. For instance, this parameter is controlled by showing or hiding the shape in the layers palette in Adobe Illustrator.")
     ("PShape.resetMatrix()" "Replaces the current matrix of a shape with the identity matrix. The equivalent function in OpenGL is <b>glLoadIdentity()</b>.")
     ("PShape.rotate()" "Rotates a shape the amount specified by the <b>angle</b> parameter. Angles should be specified in radians (values from 0 to TWO_PI) or converted to radians with the <b>radians()</b> method.
<br /><br />
Shapes are always rotated around the upper-left corner of their bounding box. Positive numbers rotate objects in a clockwise direction. Transformations apply to everything that happens after and subsequent calls to the method accumulates the effect. For example, calling <b>rotate(HALF_PI)</b> and then <b>rotate(HALF_PI)</b> is the same as <b>rotate(PI)</b>. This transformation is applied directly to the shape, it's not refreshed each time <b>draw()</b> is run.")
     ("PShape.rotateX()" "Rotates a shape around the x-axis the amount specified by the <b>angle</b> parameter. Angles should be specified in radians (values from 0 to TWO_PI) or converted to radians with the <b>radians()</b> method.
<br /><br />
Shapes are always rotated around the upper-left corner of their bounding box. Positive numbers rotate objects in a clockwise direction. Subsequent calls to the method accumulates the effect. For example, calling <b>rotateX(HALF_PI)</b> and then <b>rotateX(HALF_PI)</b> is the same as <b>rotateX(PI)</b>. This transformation is applied directly to the shape, it's not refreshed each time <b>draw()</b> is run.  
<br /><br />
This method requires a 3D renderer. You need to use P3D as a third parameter for the <b>size()</b> function as shown in the example above.")
     ("PShape.rotateY()" "Rotates a shape around the y-axis the amount specified by the <b>angle</b> parameter. Angles should be specified in radians (values from 0 to TWO_PI) or converted to radians with the <b>radians()</b> method.
<br /><br />
Shapes are always rotated around the upper-left corner of their bounding box. Positive numbers rotate objects in a clockwise direction. Subsequent calls to the method accumulates the effect. For example, calling <b>rotateY(HALF_PI)</b> and then <b>rotateY(HALF_PI)</b> is the same as <b>rotateY(PI)</b>. This transformation is applied directly to the shape, it's not refreshed each time <b>draw()</b> is run. 
<br /><br />
This method requires a 3D renderer. You need to use P3D as a third parameter for the <b>size()</b> function as shown in the example above.")
     ("PShape.rotateZ()" "Rotates a shape around the z-axis the amount specified by the <b>angle</b> parameter. Angles should be specified in radians (values from 0 to TWO_PI) or converted to radians with the <b>radians()</b> method.
<br /><br />
Shapes are always rotated around the upper-left corner of their bounding box. Positive numbers rotate objects in a clockwise direction. Subsequent calls to the method accumulates the effect. For example, calling <b>rotateZ(HALF_PI)</b> and then <b>rotateZ(HALF_PI)</b> is the same as <b>rotateZ(PI)</b>. This transformation is applied directly to the shape, it's not refreshed each time <b>draw()</b> is run. 
<br /><br />
This method requires a 3D renderer. You need to use P3D as a third parameter for the <b>size()</b> function as shown in the example above.")
     ("PShape.scale()" "Increases or decreases the size of a shape by expanding and contracting vertices. Shapes always scale from the relative origin of their bounding box. Scale values are specified as decimal percentages. For example, the method call <b>scale(2.0)</b> increases the dimension of a shape by 200%. Subsequent calls to the method multiply the effect. For example, calling <b>scale(2.0)</b> and then <b>scale(1.5)</b> is the same as <b>scale(3.0)</b>. This transformation is applied directly to the shape; it's not refreshed each time <b>draw()</b> is run. 
<br /><br />
Using this method with the <b>z</b> parameter requires using the P3D parameter in combination with size.")
     ("PShape.setVertex()" "The <b>setVertex()</b> method defines the coordinates of the vertex point located at the position defined by the <b>index</b> parameter. This method works when shapes are created as shown in the example above, but won't work properly when a shape is defined explicitly (e.g. createShape(RECT, 20, 20, 80, 80).")
     ("PShape.setVisible()" "Sets the shape to be visible or invisible. This is determined by the value of the <b>visible</b> parameter.<br/>
<br/>
The default visibility of a shape is usually controlled by whatever program created the SVG file. For instance, this parameter is controlled by showing or hiding the shape in the layers palette in Adobe Illustrator.")
     ("PShape.translate()" "Specifies an amount to displace the shape. The <b>x</b> parameter specifies left/right translation, the <b>y</b> parameter specifies up/down translation, and the <b>z</b> parameter specifies translations toward/away from the screen. Subsequent calls to the method accumulates the effect. For example, calling <b>translate(50, 0)</b> and then <b>translate(20, 0)</b> is the same as <b>translate(70, 0)</b>. This transformation is applied directly to the shape, it's not refreshed each time <b>draw()</b> is run. 
<br /><br />
Using this method with the <b>z</b> parameter requires using the P3D parameter in combination with size.")
     ("PShape.width" "The width of the PShape document.")
     ("loadShape()" "Loads geometry into a variable of type <b>PShape</b>. SVG and OBJ files may be loaded. To load correctly, the file must be located in the data directory of the current sketch. In most cases, <b>loadShape()</b> should be used inside <b>setup()</b> because loading shapes inside <b>draw()</b> will reduce the speed of a sketch.<br />
<br />
Alternatively, the file maybe be loaded from anywhere on the local computer using an absolute path (something that starts with / on Unix and Linux, or a drive letter on Windows), or the filename parameter can be a URL for a file found on a network.<br />
<br />
If the file is not available or an error occurs, <b>null</b> will be returned and an error message will be printed to the console. The error message does not halt the program, however the null value may cause a NullPointerException if your code does not check whether the value returned is null.<br/>")
     ("shape()" "Draws shapes to the display window. Shapes must be in the sketch's \"data\" directory to load correctly. Select \"Add file...\" from the \"Sketch\" menu to add the shape. Processing currently works with SVG, OBJ, and custom-created shapes. The <b>shape</b> parameter specifies the shape to display and the coordinate parameters define the location of the shape from its upper-left corner. The shape is displayed at its original size unless the <b>c</b> and <b>d</b> parameters specify a different size. The <b>shapeMode()</b> function can be used to change the way these parameters are interpreted.")
     ("shapeMode()" "Modifies the location from which shapes draw. The default mode is <b>shapeMode(CORNER)</b>, which specifies the location to be the upper left corner of the shape and uses the third and fourth parameters of <b>shape()</b> to specify the width and height. The syntax <b>shapeMode(CORNERS)</b> uses the first and second parameters of <b>shape()</b> to set the location of one corner and uses the third and fourth parameters to set the opposite corner. The syntax <b>shapeMode(CENTER)</b> draws the shape from its center point and uses the third and forth parameters of <b>shape()</b> to specify the width and height. The parameter must be written in \"ALL CAPS\" because Processing is a case sensitive language.")
     ("createShape()" "The <b>createShape()</b> function is used to define a new shape. Once created, this shape can be drawn with the <b>shape()</b> function. The basic way to use the function defines new primitive shapes. One of the following parameters are used as the first parameter: <b>ELLIPSE</b>, <b>RECT</b>, <b>ARC</b>, <b>TRIANGLE</b>, <b>SPHERE</b>, <b>BOX</b>, <b>QUAD</b>, <b>LINE</b>. The parameters for each of these different shapes are the same as their corrsponding functions: <b>ellipse()</b>, <b>rect()</b>, <b>arc()</b>, <b>triangle()</b>, <b>sphere()</b>, <b>box()</b>, and <b>line()</b>. The first example above clarifies how this works.<br />
<br />
Custom, unique shapes can be made by using <b>createShape()</b> without a parameter. After the shape is started, the drawing attributes and geometry can be set directly to the shape within the <b>beginShape()</b> and <b>endShape()</b> methods. See the second example above for specifics.<br />
<br />
Geometry that groups vertices to build larger forms, such as group of triangles, can be created with parameters to <b>beginShape()</b>. These options are <b>POINTS</b>, <b>LINES</b>, <b>TRIANGLES</b>, <b>TRIANGLE_FAN</b>, <b>TRIANGLE_STRIP</b>, <b>QUADS</b>, and <b>QUAD_STRIP</b>. See the third example above.<br />
<br />
The  <b>createShape()</b> function can also be used to make a complex shape made of other shapes. This is called a \"group\" and it's created by using the parameter <b>GROUP</b> as the first parameter. See the fourth example above to see how it works.<br />
<br />
When a shape is first created inside the <b>beingShape()</b> and <b>endShape()</b> methods, the normal Processing style functions like <b>fill()</b> and <b>stroke()</b> are used to define the drawing attributes. However, after a shape is created, a different set of functions needs to be used. These include the setFill() and setStroke() functions shown in the examples above. The complete list of methods and fields for the PShape class are in the <a href=\"http://processing.org/reference/javadoc/core/\">Processing Javadoc</a>.")
     ("createInput()" "This is a shorthand function for advanced programmers to initialize and open a Java InputStream. It's useful if you want to use the facilities provided by PApplet to easily open files from the data folder or from a URL, but you need an InputStream object so that you can use other parts of Java to take more control of how the stream is read.<br/>
<br/>
The filename passed in can be:<br/>
- A URL, as in: <b>createInput(\"http://processing.org/\")</b><br/>
- The name of a file in the sketch's <b>data</b> folder<br/>
- The full path to a file to be opened locally (when running as an application)<br/>
<br/>
If the requested item doesn't exist, null is returned. If not online, this will also check to see if the user is asking for a file whose name isn't properly capitalized. If capitalization is different, an error will be printed to the console. This helps prevent issues that appear when a sketch is exported to the web, where case sensitivity matters, as opposed to running from inside the Processing Development Environment on Windows or Mac OS, where case sensitivity is preserved but ignored.<br/>
<br/>
If the file ends with <b>.gz</b>, the stream will automatically be gzip decompressed. If you don't want the automatic decompression, use the related function <b>createInputRaw()</b>.<br/>
<br/>
In earlier releases, this function was called <b>openStream()</b>.")
     ("createReader()" "Creates a <b>BufferedReader</b> object that can be used to read files line-by-line as individual <b>String</b> objects. This is the complement to the <b>createWriter()</b> function.
<br/> <br/>
Starting with Processing release 0134, all files loaded and saved by the Processing API use UTF-8 encoding. In previous releases, the default encoding for your platform was used, which causes problems when files are moved to other platforms.")
     ("loadBytes()" "Reads the contents of a file and places it in a byte array. If the name of the file is used as the parameter, as in the above example, the file must be loaded in the sketch's \"data\" directory/folder. <br />
<br />
Alternatively, the file maybe be loaded from anywhere on the local computer using an absolute path (something that starts with / on Unix and Linux, or a drive letter on Windows), or the filename parameter can be a URL for a file found on a network.<br />
<br />
If the file is not available or an error occurs, <b>null</b> will be returned and an error message will be printed to the console. The error message does not halt the program, however the null value may cause a NullPointerException if your code does not check whether the value returned is null.<br/>")
     ("loadJSONArray()" "Loads an array of JSON objects from the data folder or a URL, and returns a <b>JSONArray</b>.  Per standard JSON syntax, the array must be enclosed in a pair of hard brackets <b>[]</b>, and each object within the array must be separated by a comma.<br>
<br>
All files loaded and saved by the Processing API use UTF-8 encoding.")
     ("loadJSONObject()" "Loads a JSON from the data folder or a URL, and returns a <b>JSONObject</b>.<br>
<br>
All files loaded and saved by the Processing API use UTF-8 encoding.")
     ("loadStrings()" "Reads the contents of a file and creates a String array of its individual lines. If the name of the file is used as the parameter, as in the above example, the file must be loaded in the sketch's \"data\" directory/folder. <br />
<br />
Alternatively, the file maybe be loaded from anywhere on the local computer using an absolute path (something that starts with / on Unix and Linux, or a drive letter on Windows), or the filename parameter can be a URL for a file found on a network.<br />
<br />
If the file is not available or an error occurs, <b>null</b> will be returned and an error message will be printed to the console. The error message does not halt the program, however the null value may cause a NullPointerException if your code does not check whether the value returned is null.<br/>
<br />
Starting with Processing release 0134, all files loaded and saved by the Processing API use UTF-8 encoding. In previous releases, the default encoding for your platform was used, which causes problems when files are moved to other platforms.")
     ("loadTable()" "Reads the contents of a file or URL and creates an Table object with its values. If a file is specified, it must be located in the sketch's \"data\" folder. The filename parameter can also be a URL to a file found online.  By default, the file is assumed to be comma-separated (in CSV format).  To use tab-separated data, include \"tsv\" in the options parameter.<br/>
<br/>
If the file contains a header row, include \"header\" in the options parameter.  If the file does not have a header row, then simply omit the \"header\" option.<br/>
<br/>
When specifying both a header and the file type, separate the options with commas, as in: <b>loadTable(\"data.csv\", \"header, tsv\")</b><br/>
<br/>
All files loaded and saved by the Processing API use UTF-8 encoding.")
     ("loadXML()" "Reads the contents of a file or URL and creates an XML object with its values. If a file is specified, it must be located in the sketch's \"data\" folder. The filename parameter can also be a URL to a file found online.<br/>
<br/>
All files loaded and saved by the Processing API use UTF-8 encoding. If you need to load an XML file that's not in UTF-8 format, see the <a href=\"http://processing.org/reference/javadoc/core/processing/data/XML.html\">developer's reference</a> for the XML object.")
     ("open()" "Attempts to open an application or file using your platform's launcher. The <b>filename</b> parameter is a String specifying the file name and location. The location parameter must be a full path name, or the name of an executable in the system's PATH. In most cases, using a full path is the best option, rather than relying on the system PATH. Be sure to make the file executable before attempting to open it (chmod +x). 
<br/> <br/>
The <b>argv</b> parameter is a String or String array which is passed to the command line. If you have multiple parameters, e.g. an application and a document, or a command with multiple switches, use the version that takes a String array, and place each individual item in a separate element. 
<br/> <br/>
If <b>argv</b> is a String (not an array), then it can only be a single file or application with no parameters. It's not the same as executing that String using a shell. For instance, <b>open(\"jikes -help\")</b> will not work properly.
<br/> <br/>
This function behaves differently on each platform. On Windows, the parameters are sent to the Windows shell via \"cmd /c\". On Mac OS X, the \"open\" command is used (type \"man open\" in Terminal.app for documentation). On Linux, it first tries gnome-open, then kde-open, but if neither are available, it sends the command to the shell without any alterations. 
<br/> <br/>
For users familiar with Java, this is not quite the same as Runtime.exec(), because the launcher command is prepended. Instead, the <b>exec(String[])</b> function is a shortcut for Runtime.getRuntime.exec(String[]).")
     ("parseXML()" "Takes a String, parses its contents, and returns an XML object.  If the String does not contain XML data or cannot be parsed, a null value is returned.<br/>
<br/>
<b>parseXML()</b> is most useful when pulling data dynamically, such as from third-party APIs.  Normally, API results would be saved to a String, and then can be converted to a structured XML object using <b>parseXML()</b>.  Be sure to check if null is returned before performing operations on the new XML object, in case the String content could not be parsed.<br/>
<br/>
If your data already exists as an XML file in the data folder, it is simpler to use <b>loadXML()</b>.")
     ("selectFolder()" "Opens a platform-specific file chooser dialog to select a folder. After the selection is made, the selection will be passed to the 'callback' function. If the dialog is closed or canceled, null will be sent to the function, so that the program is not waiting for additional input. The callback is necessary because of how threading works.")
     ("selectInput()" "Opens a platform-specific file chooser dialog to select a file for input. After the selection is made, the selected File will be passed to the 'callback' function. If the dialog is closed or canceled, null will be sent to the function, so that the program is not waiting for additional input. The callback is necessary because of how threading works."))
  "List of functions and variables available by default in Processing.")

(defvar auto-complete-processing--functions-to-remove-prefix
  '(("FloatDict.add" . "add")
     ("FloatDict.clear" . "clear")
     ("FloatDict.div" . "div")
     ("FloatDict.get" . "get")
     ("FloatDict.hasKey" . "hasKey")
     ("FloatDict.keyArray" . "keyArray")
     ("FloatDict.keys" . "keys")
     ("FloatDict.mult" . "mult")
     ("FloatDict.remove" . "remove")
     ("FloatDict.set" . "set")
     ("FloatDict.size" . "size")
     ("FloatDict.sortKeys" . "sortKeys")
     ("FloatDict.sortKeysReverse" . "sortKeysReverse")
     ("FloatDict.sortValues" . "sortValues")
     ("FloatDict.sortValuesReverse" . "sortValuesReverse")
     ("FloatDict.sub" . "sub")
     ("FloatDict.valueArray" . "valueArray")
     ("FloatDict.values" . "values")
     ("FloatList.add" . "add")
     ("FloatList.append" . "append")
     ("FloatList.array" . "array")
     ("FloatList.clear" . "clear")
     ("FloatList.div" . "div")
     ("FloatList.get" . "get")
     ("FloatList.hasValue" . "hasValue")
     ("FloatList.max" . "max")
     ("FloatList.min" . "min")
     ("FloatList.mult" . "mult")
     ("FloatList.remove" . "remove")
     ("FloatList.reverse" . "reverse")
     ("FloatList.set" . "set")
     ("FloatList.shuffle" . "shuffle")
     ("FloatList.size" . "size")
     ("FloatList.sort" . "sort")
     ("FloatList.sortReverse" . "sortReverse")
     ("FloatList.sub" . "sub")
     ("IntDict.add" . "add")
     ("IntDict.clear" . "clear")
     ("IntDict.div" . "div")
     ("IntDict.get" . "get")
     ("IntDict.hasKey" . "hasKey")
     ("IntDict.increment" . "increment")
     ("IntDict.keyArray" . "keyArray")
     ("IntDict.keys" . "keys")
     ("IntDict.mult" . "mult")
     ("IntDict.remove" . "remove")
     ("IntDict.set" . "set")
     ("IntDict.size" . "size")
     ("IntDict.sortKeys" . "sortKeys")
     ("IntDict.sortKeysReverse" . "sortKeysReverse")
     ("IntDict.sortValues" . "sortValues")
     ("IntDict.sortValuesReverse" . "sortValuesReverse")
     ("IntDict.sub" . "sub")
     ("IntDict.valueArray" . "valueArray")
     ("IntDict.values" . "values")
     ("IntList.add" . "add")
     ("IntList.append" . "append")
     ("IntList.array" . "array")
     ("IntList.clear" . "clear")
     ("IntList.div" . "div")
     ("IntList.get" . "get")
     ("IntList.hasValue" . "hasValue")
     ("IntList.increment" . "increment")
     ("IntList.max" . "max")
     ("IntList.min" . "min")
     ("IntList.mult" . "mult")
     ("IntList.remove" . "remove")
     ("IntList.reverse" . "reverse")
     ("IntList.set" . "set")
     ("IntList.shuffle" . "shuffle")
     ("IntList.size" . "size")
     ("IntList.sort" . "sort")
     ("IntList.sortReverse" . "sortReverse")
     ("IntList.sub" . "sub")
     ("JSONArray.append" . "append")
     ("JSONArray.getBoolean" . "getBoolean")
     ("JSONArray.getFloat" . "getFloat")
     ("JSONArray.getInt" . "getInt")
     ("JSONArray.getIntArray" . "getIntArray")
     ("JSONArray.getJSONArray" . "getJSONArray")
     ("JSONArray.getJSONObject" . "getJSONObject")
     ("JSONArray.getString" . "getString")
     ("JSONArray.getStringArray" . "getStringArray")
     ("JSONArray.remove" . "remove")
     ("JSONArray.setBoolean" . "setBoolean")
     ("JSONArray.setFloat" . "setFloat")
     ("JSONArray.setInt" . "setInt")
     ("JSONArray.getJSONArray" . "getJSONArray")
     ("JSONArray.getJSONObject" . "getJSONObject")
     ("JSONArray.setString" . "setString")
     ("JSONArray.size" . "size")
     ("JSONObject.getBoolean" . "getBoolean")
     ("JSONObject.getFloat" . "getFloat")
     ("JSONObject.getInt" . "getInt")
     ("JSONObject.getJSONArray" . "getJSONArray")
     ("JSONObject.getJSONObject" . "getJSONObject")
     ("JSONObject.getString" . "getString")
     ("JSONObject.setBoolean" . "setBoolean")
     ("JSONObject.setFloat" . "setFloat")
     ("JSONObject.setInt" . "setInt")
     ("JSONObject.setJSONArray" . "setJSONArray")
     ("JSONObject.setJSONObject" . "setJSONObject")
     ("JSONObject.setString" . "setString")
     ("StringDict.clear" . "clear")
     ("StringDict.get" . "get")
     ("StringDict.hasKey" . "hasKey")
     ("StringDict.keyArray" . "keyArray")
     ("StringDict.keys" . "keys")
     ("StringDict.remove" . "remove")
     ("StringDict.set" . "set")
     ("StringDict.size" . "size")
     ("StringDict.sortKeys" . "sortKeys")
     ("StringDict.sortKeysReverse" . "sortKeysReverse")
     ("StringDict.sortValues" . "sortValues")
     ("StringDict.sortValuesReverse" . "sortValuesReverse")
     ("StringDict.valueArray" . "valueArray")
     ("StringDict.values" . "values")
     ("StringList.append" . "append")
     ("StringList.array" . "array")
     ("StringList.clear" . "clear")
     ("StringList.get" . "get")
     ("StringList.hasValue" . "hasValue")
     ("StringList.lower" . "lower")
     ("StringList.remove" . "remove")
     ("StringList.reverse" . "reverse")
     ("StringList.set" . "set")
     ("StringList.shuffle" . "shuffle")
     ("StringList.size" . "size")
     ("StringList.sort" . "sort")
     ("StringList.sortReverse" . "sortReverse")
     ("StringList.upper" . "upper")
     ("TableRow.getFloat" . "getFloat")
     ("TableRow.getFloat" . "getFloat")
     ("TableRow.getString" . "getString")
     ("TableRow.setFloat" . "setFloat")
     ("TableRow.setInt" . "setInt")
     ("TableRow.setString" . "setString")
     ("Table.addColumn" . "addColumn")
     ("Table.addRow" . "addRow")
     ("Table.clearRows" . "clearRows")
     ("Table.findRow" . "findRow")
     ("Table.findRows" . "findRows")
     ("Table.getColumnCount" . "getColumnCount")
     ("Table.getFloat" . "getFloat")
     ("Table.getInt" . "getInt")
     ("Table.getRow" . "getRow")
     ("Table.getRowCount" . "getRowCount")
     ("Table.getString" . "getString")
     ("Table.getStringColumn" . "getStringColumn")
     ("Table.matchRow" . "matchRow")
     ("Table.matchRows" . "matchRows")
     ("Table.removeColumn" . "removeColumn")
     ("Table.removeRow" . "removeRow")
     ("Table.removeTokens" . "removeTokens")
     ("Table.rows" . "rows")
     ("Table.setFloat" . "setFloat")
     ("Table.setInt" . "setInt")
     ("Table.setString" . "setString")
     ("Table.trim" . "trim")
     ("PVector.add" . "add")
     ("PVector.angleBetween" . "angleBetween")
     ("PVector.array" . "array")
     ("PVector.copy" . "copy")
     ("PVector.cross" . "cross")
     ("PVector.dist" . "dist")
     ("PVector.div" . "div")
     ("PVector.dot" . "dot")
     ("PVector.fromAngle" . "fromAngle")
     ("PVector.get" . "get")
     ("PVector.heading" . "heading")
     ("PVector.lerp" . "lerp")
     ("PVector.limit" . "limit")
     ("PVector.mag" . "mag")
     ("PVector.magSq" . "magSq")
     ("PVector.mult" . "mult")
     ("PVector.normalize" . "normalize")
     ("PVector.random2D" . "random2D")
     ("PVector.random3D" . "random3D")
     ("PVector.rotate" . "rotate")
     ("PVector.set" . "set")
     ("PVector.setMag" . "setMag")
     ("PVector.sub" . "sub")
     ("PVector.x" . "x")
     ("PVector.y" . "y")
     ("PVector.z" . "z")
     ("PImage.blend" . "blend")
     ("PImage.copy" . "copy")
     ("PImage.filter" . "filter")
     ("PImage.get" . "get")
     ("PImage.height" . "height")
     ("PImage.loadPixels" . "loadPixels")
     ("PImage.mask" . "mask")
     ("PImage.pixels" . "pixels")
     ("PImage.resize" . "resize")
     ("PImage.save" . "save")
     ("PImage.set" . "set")
     ("PImage.updatePixels" . "updatePixels")
     ("PImage.width" . "width")
     ("PShader.set" . "set")
     ("PFont.list" . "list")
     ("PShape.addChild" . "addChild")
     ("PShape.beginContour" . "beginContour")
     ("PShape.beginShape" . "beginShape")
     ("PShape.disableStyle" . "disableStyle")
     ("PShape.enableStyle" . "enableStyle")
     ("PShape.endContour" . "endContour")
     ("PShape.endShape" . "endShape")
     ("PShape.getChild" . "getChild")
     ("PShape.getChildCount" . "getChildCount")
     ("PShape.getVertex" . "getVertex")
     ("PShape.getVertexCount" . "getVertexCount")
     ("PShape.height" . "height")
     ("PShape.isVisible" . "isVisible")
     ("PShape.resetMatrix" . "resetMatrix")
     ("PShape.rotate" . "rotate")
     ("PShape.rotateX" . "rotateX")
     ("PShape.rotateY" . "rotateY")
     ("PShape.rotateZ" . "rotateZ")
     ("PShape.scale" . "scale")
     ("PShape.setVertex" . "setVertex")
     ("PShape.setVisible" . "setVisible")
     ("PShape.translate" . "translate")
     ("PShape.width" . "width")))

(defun auto-complete-processing--get-candidates ()
  "Return a list of strings containing the candidates to pass to auto-complete."
  (mapcar #'car auto-complete-processing--auto-complete-data))

;; (ppcb (mapcar (lambda (x)
;;                 (cons x (second (s-split "\\\." x))))
;;         (mapcar #'car auto-complete-processing--auto-complete-data)))

(defun auto-complete-processing--get-documentation (completion)
  "Return the documentation for COMPLETION."
  ;; NOTE: `completion' contains the class prefix if it exists,
  ;; e.g. "Table.trim" instead of just "trim"
  (let ((contents (substring-no-properties completion)))
    (with-temp-buffer
      ;; Each element in `auto-complete-processing--auto-complete-data'
      ;; has the following syntax:
      ;;   car - a function name e.g. "Table.trim"
      ;;   cadr - it's documentation e.g. "Trims leading and trailing [...snip...]"
      (insert (cadr (assoc contents auto-complete-processing--auto-complete-data)))

      ;; Unfortunately `popup-tip' does a `substring-no-properties' on the return value
      ;; of `auto-complete-processing--get-documentation' which removes the "bolds" and
      ;; "italics" rendered by `shr-render-function'.
      ;;
      ;; At least `shr-render-region' removes the ugly "<b>" and "</b>" found in the
      ;; documentation.
      ;;
      ;; Note that the shr package isn't in Emacs 23 and requires emacs compile with libxml2.
      ;;
      ;; TODO: Perhaps send a patch to popup.el to stop it from removing the text properties?
      (shr-render-region (point-min) (point-max))
      (buffer-string))))

(defun auto-complete-processing--remove-class-prefix-from-method ()
  "Convert things like \"FloadList.sortReverse()\" into \"sortReverse()\" if needed."
  ;; Check `ac-complete-1' to see how the variable `ac-last-completion' is created
  (let ((just-completed-string (substring-no-properties (cdr ac-last-completion))))
    (unless (looking-back just-completed-string)
      ;; We should never reach this form.
      (error "Not `looking-back' at `ac-last-completion'."))
    (save-excursion
      (re-search-backward just-completed-string)
      (let ((replace (assoc just-completed-string auto-complete-processing--functions-to-remove-prefix)))
        (when replace
          (replace-match (cdr replace))
          ;; If there is a ". " before `just-completed-string' then remove the space
          ;; This let's you do something like the following:
          ;;     FloatList foo;
          ;;     foo. |                <-- cursor is here
          ;;     foo. float|[list.add] <-- cursor needs to be separated from "foo." to start the auto-complete
          ;;     foo.add               <-- pressing RET joins the "add" with the "foo."
          (when (looking-back (concat "\\\. " (cdr replace)))
            (re-search-backward (concat "\\\. " (cdr replace)))
            ;; Remove the space after the "." character
            (replace-match (concat "." (cdr replace)) nil 'literal)))))))

(defvar ac-source-processing
  '((candidates . auto-complete-processing--get-candidates)
     (action . auto-complete-processing--remove-class-prefix-from-method)
     (document . auto-complete-processing--get-documentation)
     (cache)))



(provide 'auto-complete-processing)
;;; auto-complete-processing.el ends here
