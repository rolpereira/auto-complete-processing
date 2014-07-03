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
     ("randomSeed()" "Sets the seed value for <b>random()</b>. By default, <b>random()</b> produces different results each time the program is run. Set the <b>seed</b> parameter to a constant to return the same pseudo-random numbers each time the software is run."))
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
     ("PImage.width" . "width")))

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
