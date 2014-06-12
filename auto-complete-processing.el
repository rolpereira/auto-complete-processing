;;; auto-complete-processing.el --- Auto-complete sources for processing  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Rolando Pereira

;; Author: Rolando Pereira <finalyugi@sapo.pt>
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
     ("JSONArray.getJSONArray" "Sets the value of the <b>JSONArray</b> with the associated index value.")
     ("JSONArray.getJSONObject" "Sets the value of the <b>JSONObject</b> with the index value.")
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
     ("TableRow.getFloat" "Retrieves an integer value from the <b>TableRow</b>'s specified column. The column may be specified by either its ID or title.")
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
For more detail on how <b>pmouseY</b> is updated inside of mouse events and <b>draw()</b>, see the reference for <b>pmouseX</b>."))  
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
     ("Table.trim" . "trim")))

(defun auto-complete-processing--get-candidates ()
  "Return a list of strings containing the candidates to pass to auto-complete."
  (mapcar #'car auto-complete-processing--auto-complete-data))


(defun auto-complete-processing--get-documentation (completion)
  "Return the documentation for COMPLETION."
  ;; NOTE: `completion' contains the class prefix if it exists,
  ;; e.g. "Table.trim" instead of just "trim"
  (let ((contents (substring-no-properties completion)))
    ;; Each element in `auto-complete-processing--auto-complete-data'
    ;; has the following syntax:
    ;;   car - a function name e.g. "Table.trim"
    ;;   cadr - it's documentation e.g. "Trims leading and trailing [...snip...]"
    (cadr (assoc contents auto-complete-processing--auto-complete-data))))

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
