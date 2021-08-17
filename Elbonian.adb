with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

with Ada.Strings.Hash;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
use Ada.Containers;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;




procedure Elbonian is
-------------------------------------------------------
-- Elbonian Cipher
-------------------------------------------------------


-- Unbounded_String Set
-- package Unbounded_String_Set is new Ada.Containers.Ordered_Sets(
-- 		Element_Type => Unbounded_String);

--Dictionary_Words_X : Unbounded_String_Set;

--type String_Array is array(Positive range <>) of String;
type Unbounded_String_Array is array(Positive range <>) of Unbounded_String;
Dictionary_Words : Unbounded_String_Array := (
		To_Unbounded_String("hi"),
		To_Unbounded_String("bye"),
		To_Unbounded_String("a"),
		To_Unbounded_String("cat"),
		To_Unbounded_String("cab"),
		To_Unbounded_String("bat"),
		To_Unbounded_String("back"),
		To_Unbounded_String("def"),
		To_Unbounded_String("fed"),
		To_Unbounded_String("feed"),
		To_Unbounded_String("hello"),
		To_Unbounded_String("world"),
		To_Unbounded_String("there"),
		To_Unbounded_String("call"),
		To_Unbounded_String("ball"),
		To_Unbounded_String("baby"));

--Dictionary_Words : Unbounded_String_Array;

-- TODO ^ This has to be refactored to pull from a JSON or something...





package Unbounded_String_Vectors is new Ada.Containers.Vectors
		(Index_Type => Natural,
		Element_Type => Unbounded_String);
subtype Unbounded_String_Vector is Unbounded_String_Vectors.Vector;

-----------
Permutation_List : Unbounded_String_Vector;
Translated_Permutation_List : Unbounded_String_Vector;
All_Options : Unbounded_String_Vector;
Word_Options : Unbounded_String_Vector;
Sentence_Options : Unbounded_String_Vector;

package Unbounded_String_Vector_Vectors is new Ada.Containers.Vectors
		(Index_Type => Natural,
		 Element_Type => Unbounded_String_Vectors.Vector,
		"=" => Unbounded_String_Vectors."=");
subtype Unbounded_String_Vector_Vector is Unbounded_String_Vector_Vectors.Vector;
--https://stackoverflow.com/questions/42193102/subtype-mark-required-in-this-context-ada

Full_Permutation_List : Unbounded_String_Vector_Vector;


-------


-- Letter to Number Hash
function Letter_To_Number_Equivalent_Key (Left, Right : Character) return Boolean is
begin
	return Left = Right;
end Letter_To_Number_Equivalent_Key;

function Letter_To_Number_Hash_Func(Key : Character) return Ada.Containers.Hash_Type is
begin
	return Ada.Strings.Hash(Character'Image(Key));
end Letter_To_Number_Hash_Func;

package Letter_To_Number_Hash is new Ada.Containers.Hashed_Maps(
Key_Type => Character, Element_Type => Natural,
		Hash => Letter_To_Number_Hash_Func, Equivalent_Keys => Letter_To_Number_Equivalent_Key);

-- Number to Letter Hash
function Number_To_Letter_Equivalent_Key (Left, Right : Natural) return Boolean is
begin
	return Left = Right;
end Number_To_Letter_Equivalent_Key;

function Number_To_Letter_Hash_Func(Key : Natural) return Ada.Containers.Hash_Type is
begin
	return Ada.Strings.Hash(Natural'Image(Key));
end Number_To_Letter_Hash_Func;

package Number_To_Letter_Hash is new Ada.Containers.Hashed_Maps(
		Key_Type => Natural,
		Element_Type => Character,
		Hash => Number_To_Letter_Hash_Func,
		Equivalent_Keys => Number_To_Letter_Equivalent_Key);

-- Number to Unbounded_String_Vector Hash
function Number_To_Unbounded_String_Vector_Equivalent_Key (Left, Right : Natural) return Boolean is
begin
	return Left = Right;
end Number_To_Unbounded_String_Vector_Equivalent_Key;

function Number_To_Unbounded_String_Vector_Hash_Func(Key : Natural) return Ada.Containers.Hash_Type is
begin
	return Ada.Strings.Hash(Natural'Image(Key));
end Number_To_Unbounded_String_Vector_Hash_Func;

package Number_To_Unbounded_String_Vector_Hash is new Ada.Containers.Hashed_Maps(
		Key_Type => Natural,
		Element_Type => Unbounded_String_Vector,
		Hash => Number_To_Unbounded_String_Vector_Hash_Func,
		Equivalent_Keys => Number_To_Unbounded_String_Vector_Equivalent_Key,
		"="             => Unbounded_String_Vectors."=");

------------------------------------------

type Character_Array is array(Positive range <>) of Character;
type Natural_Array is array(Positive range <>) of Natural;

Letter_To_Number_Map : Letter_To_Number_Hash.Map;
Number_To_Letter_Map : Number_To_Letter_Hash.Map;
Key_List : Character_Array := ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v' ,'w', 'x', 'y', 'z', ' ');
Element_List : Natural_Array := (2, 22, 222, 3, 33, 333, 4, 44, 444, 5, 55, 555, 6, 66, 666, 7, 77, 777, 7777, 8, 88, 8888, 9, 99, 999, 9999, 0);

Cost_Permutations_Map : Number_To_Unbounded_String_Vector_Hash.Map;







--- functions

---------------------------------------------------------------
-- This function will convert the inputted Natural Number to a String without the +/- padding that is normally present
-- Input: A Natural number
-- Output: A String representation of the Natural
---------------------------------------------------------------
function To_Unpadded_Natural(Num : Natural) return String is
	Raw_Image : constant String := Natural'Image(Num);
	Trimmed_Image : constant String := Trim(Raw_Image, Ada.Strings.Left);
begin
	return Trimmed_Image;
end To_Unpadded_Natural;


---------------------------------------------------------------
procedure Set_Keyboard_Maps is
begin
	-- put them into the map
	for I in Key_List'range loop
		Letter_To_Number_Map.Insert(Key => (Key_List(I)), New_Item => (Element_List(I)));
		Number_To_Letter_Map.Insert(Key => (Element_List(I)), New_Item => (Key_List(I)));
	end loop;
end Set_Keyboard_Maps;


---------------------------------------------------------------
procedure Print_Letter_To_Number_Map is
begin
	-- read and print the map
	for I in Key_List'range loop
		Ada.Text_Io.Put_Line(Character'Image(Key_List(I)) & " => " &
					To_Unpadded_Natural(Letter_To_Number_Map.Element(Key_List(I))));
	end loop;
end Print_Letter_To_Number_Map;

---------------------------------------------------------------
procedure Print_Number_To_Letter_Map is
begin
	-- read and print the map
	for I in Key_List'range loop
		Ada.Text_Io.Put_Line(To_Unpadded_Natural(Element_List(I)) & " => " &
					Character'Image(Number_To_Letter_Map.Element(Element_List(I))));
	end loop;
end Print_Number_To_Letter_Map;



---------------------------------------------------------------
-- This function will convert the String input to the valid Elbonian ciphertext.
-- Input: An Unbounded_String representing a plaintext sentence
-- Output: An Unbounded_String representing the ciphertext
---------------------------------------------------------------
function Elbonian_Encrypt (Input_String : Unbounded_String) return Unbounded_String is
	Encrypted_String : Unbounded_String := To_Unbounded_String("");
	Current_Character : Character;
	Mapped_Natural : Natural;
begin
	-- loop through each character in the Input_String and map it to its "encrypted" value
	for I in 1 .. Length(Input_String) loop
		Current_Character :=  Element(Input_String, I);
		Mapped_Natural := Letter_To_Number_Map.Element(Current_Character);
		Append(Encrypted_String, To_Unpadded_Natural(Mapped_Natural));
	end loop;
   
	return Encrypted_String;
end Elbonian_Encrypt;

-----------------------------------------------------------------------
-- returns the number of number chunks there are where a number chunk is a set of consistent, consecutive numbers.
-----------------------------------------------------------------------
function Get_Num_Chunks(Encrypted_String : Unbounded_String) return Natural is
	Num_Chunks : Natural := 0;
	Current_Char : Character := ' ';
begin
   
	for I in 1 .. Length(Encrypted_String) loop
		if Element(Encrypted_String, I) /= Current_Char then
			Num_Chunks := Num_Chunks + 1;
			Current_Char := Element(Encrypted_String, I);
		end if;
	end loop;
   
	return Num_Chunks;
end Get_Num_Chunks;

----------------------------------------------------------------------
-- returns the length for each number chunk each in the array
----------------------------------------------------------------------
function Get_Chunk_Lengths(Encrypted_String : Unbounded_String; Num_Chunks : Natural) return Natural_Array is
	Chunk_Lengths : Natural_Array(1 .. Num_Chunks);
	Current_Chunk_Char : Character := ' ';
	Current_Chunk_Length : Natural := 0;
	Current_Chunk : Positive := 1;
begin
   
	for I in 1 .. Length(Encrypted_String) loop
		if Element(Encrypted_String, I) = Current_Chunk_Char then
			Current_Chunk_Length := Current_Chunk_Length + 1;
		else
			-- If this wasn't the current char, make it the current char and set the last one
			if Current_Chunk_Length /= 0 then
				Chunk_Lengths(Current_Chunk) := Current_Chunk_Length;
				Current_Chunk := Current_Chunk + 1;
			end if;
			Current_Chunk_Char := Element(Encrypted_String, I);
			Current_Chunk_Length := 1;
		end if;
	end loop;
   
	-- Add the last one to the chunks
	if Current_Chunk_Length /= 0 then
		Chunk_Lengths(Current_Chunk) := Current_Chunk_Length;
	end if;
   
	return Chunk_Lengths;
end Get_Chunk_Lengths;

----------------------------------------------------------------------
-- returns the number for each number chunk each in the array
----------------------------------------------------------------------
function Get_Chunk_Numbers(Encrypted_String : Unbounded_String; Num_Chunks : Natural) return Natural_Array is
	Chunk_Numbers : Natural_Array(1 .. Num_Chunks);
	Current_Chunk_Char : Character := ' ';
	Current_Chunk : Positive := 1;
begin
   
	for I in 1 .. Length(Encrypted_String) loop
		if Element(Encrypted_String, I) /= Current_Chunk_Char then
			-- If this wasn't the current char, make it the current char and set the last one
			if Current_Chunk_Char /= ' ' then
				Chunk_Numbers(Current_Chunk) := Natural'Value((1 => Current_Chunk_Char));
				Current_Chunk := Current_Chunk + 1;
			end if;
			Current_Chunk_Char := Element(Encrypted_String, I);
		end if;
	end loop;
   
	-- Add the last one to the chunks
	if Current_Chunk_Char /= ' ' then
		Chunk_Numbers(Current_Chunk) := Natural'Value((1 => Current_Chunk_Char));
	end if;
   
	return Chunk_Numbers;
end Get_Chunk_Numbers;


-----------------------------------------------------------------------
-- returns all 'weight' options with this 'cost'. Like the bag problem
-- the weights are either 1, 2, 3, or 4 respectively
--
-- Update the Permutation_List accordingly
-----------------------------------------------------------------------
procedure Get_Permutations(Current_String : Unbounded_String; Cost : Natural) is
begin
	if Cost >= 4 then
		-- If we can add a four to the string
		Get_Permutations(Current_String & "4", Cost - 4);
	end if;
	if Cost >= 3 then
		-- If we can add a three to the string
		Get_Permutations(Current_String & "3", Cost - 3);
	end if;
	if Cost >= 2 then
		-- If we can add a two to the string
		Get_Permutations(Current_String & "2", Cost - 2);
	end if;
	if Cost >= 1 then
		-- If we can add a one to the string
		Get_Permutations(Current_String & "1", Cost - 1);
	end if;
   
	if Cost = 0 and then not Permutation_List.Contains(Current_String) then
		-- if we completed the string cost and it isn't already recorded, record it.
		Permutation_List.Append(Current_String);
	end if;
end Get_Permutations;


-----------------------------------------------------------------
-- Print
----------------------------------------------------------------
procedure Print_Unbounded_String_Vector(Input_Vector : Unbounded_String_Vector) is
begin
	-- Put_Line("Length is:" & Ada.Containers.Count_Type'Image(Input_Vector.Length));
	for E of Input_Vector loop
		Put_Line(E);
	end loop;
end Print_Unbounded_String_Vector;
---------------------------------------------------------------
procedure Print_Permutation_List is
begin
	Print_Unbounded_String_Vector(Permutation_List);
end Print_Permutation_List;
---------------------------------------------------------------
procedure Print_Translated_Permutation_List is
begin
	Print_Unbounded_String_Vector(Translated_Permutation_List);
end Print_Translated_Permutation_List;
---------------------------------------------------------------
procedure Print_Full_Permutation_List is
begin
	-- Put_Line("Outer-Length is:" & Ada.Containers.Count_Type'Image(Full_Permutation_List.Length));
	for E of Full_Permutation_List loop
		Print_Unbounded_String_Vector(E);
	end loop;
end Print_Full_Permutation_List;
---------------------------------------------------------------
procedure Print_All_Options is
begin
	Print_Unbounded_String_Vector(All_Options);
end Print_All_Options;


---------------------------------------------------------------------
function Convert_To_Letters(Input_String : Unbounded_String; Keypad_Num : Natural) return Unbounded_String is
	One_Conversion : Unbounded_String := To_Unbounded_String("");
	Two_Conversion : Unbounded_String := To_Unbounded_String("");
	Three_Conversion : Unbounded_String := To_Unbounded_String("");
	Four_Conversion : Unbounded_String := To_Unbounded_String("");
	Output_String : Unbounded_String := To_Unbounded_String("");
begin
	-- set the conversion values for the keypad
	if Keypad_Num = 2 then
		One_Conversion := To_Unbounded_String("a");
		Two_Conversion := To_Unbounded_String("b");
		Three_Conversion := To_Unbounded_String("c");
	elsif Keypad_Num = 3 then
		One_Conversion := To_Unbounded_String("d");
		Two_Conversion := To_Unbounded_String("e");
		Three_Conversion := To_Unbounded_String("f");
	elsif Keypad_Num = 4 then
		One_Conversion := To_Unbounded_String("g");
		Two_Conversion := To_Unbounded_String("h");
		Three_Conversion := To_Unbounded_String("i");
	elsif Keypad_Num = 5 then
		One_Conversion := To_Unbounded_String("j");
		Two_Conversion := To_Unbounded_String("k");
		Three_Conversion := To_Unbounded_String("l");
	elsif Keypad_Num = 6 then
		One_Conversion := To_Unbounded_String("m");
		Two_Conversion := To_Unbounded_String("n");
		Three_Conversion := To_Unbounded_String("o");
	elsif Keypad_Num = 7 then
		One_Conversion := To_Unbounded_String("p");
		Two_Conversion := To_Unbounded_String("q");
		Three_Conversion := To_Unbounded_String("r");
		Four_Conversion := To_Unbounded_String("s");
	elsif Keypad_Num = 8 then
		One_Conversion := To_Unbounded_String("t");
		Two_Conversion := To_Unbounded_String("u");
		Three_Conversion := To_Unbounded_String("v");
	elsif Keypad_Num = 9 then
		One_Conversion := To_Unbounded_String("w");
		Two_Conversion := To_Unbounded_String("x");
		Three_Conversion := To_Unbounded_String("y");
		Four_Conversion := To_Unbounded_String("z");
	elsif Keypad_Num = 0 then
		One_Conversion := To_Unbounded_String(" ");
	end if;
   
	-- replace the numbers with their keypad letters
	for I in 1 .. Length(Input_String) loop
		if Element(Input_String, I) = '1' then
			Output_String := Output_String & One_Conversion;
		elsif Element(Input_String, I) = '2' then
			Output_String := Output_String & Two_Conversion;
		elsif Element(Input_String, I) = '3' then
			Output_String := Output_String & Three_Conversion;
		elsif Element(Input_String, I) = '4' then
			-- check if we accept an input_string with a 4
			if Four_Conversion = To_Unbounded_String("") then
				return To_Unbounded_String("");
			end if;
			Output_String := Output_String & Four_Conversion;
		end if;
	end loop;
   
	return Output_String;
end Convert_To_Letters;


--------------------------------------------------------------------
-- Constantly update All_Options, to append all the possible permutations onto it over and over again. All_Options is constantly getting longer with more elements, and longer with each word's length
--------------------------------------------------------------------
procedure Generate_All_Options is
	Temp_Options : Unbounded_String_Vector;
begin
	for E of Full_Permutation_List loop -- 'E' is "Permutation List"
		Temp_Options := Unbounded_String_Vectors.Empty_Vector;
		for X of E loop  -- 'X' is Element in 'Permutation List'
			-- for each thing already in the list,
			--Put_Line("X: " & X);
			for Z of All_Options loop -- 'Z' is Element in All_Options
				--Put_Line("Z: " & Z);
				Temp_Options.Append(Z & X);
			end loop;
		   
			if Unbounded_String_Vectors.Is_Empty(All_Options) then
				Temp_Options.Append(X);
			end if;
		end loop;
		All_Options := Temp_Options;
	end loop;
end Generate_All_Options;

---------------------------------------------------------------
-- mini
---------------------------------------------------------------
procedure Generate_Word_Options is
	Temp_Options : Unbounded_String_Vector;
begin
	for E of Full_Permutation_List loop -- 'E' is "Permutation List"
		Temp_Options := Unbounded_String_Vectors.Empty_Vector;
		for X of E loop  -- 'X' is Element in 'Permutation List'
			-- for each thing already in the list,
			--Put_Line("X: " & X);
			for Z of Word_Options loop -- 'Z' is Element in Word_Options
				--Put_Line("Z: " & Z);
				Temp_Options.Append(Z & X);
			end loop;
		   
			if Unbounded_String_Vectors.Is_Empty(Word_Options) then
				Temp_Options.Append(X);
			end if;
		end loop;
		Word_Options := Temp_Options;
	end loop;
end Generate_Word_Options;

---------------------------------------------------------------
-- mega
---------------------------------------------------------------
procedure Generate_Sentence_Options is
	Temp_Options : Unbounded_String_Vector;
begin
	Temp_Options := Unbounded_String_Vectors.Empty_Vector;
	for E of Word_Options loop -- 'E' is "Permutation List"
		--Put_Line("E:" & E);
		for Z of Sentence_Options loop -- 'Z' is Element in Word_Options
			--Put_Line("Z: " & Z);
			--Put_Line("Z:" & Z);
			Temp_Options.Append(Z & E);
		end loop;
	   
		if Unbounded_String_Vectors.Is_Empty(Sentence_Options) then
			Temp_Options.Append(E);
		end if;
	   
	end loop;
	Sentence_Options := Temp_Options;
end Generate_Sentence_Options;




---------------------------------------------------------------
function Almost_Equals(String_1: Unbounded_String; String_2: Unbounded_String; Num_Misspelled_Characters_Allowed : Natural) return Boolean is
	String_2_Additional_Characters : Integer := Length(String_2) - Length(String_1); -- positive if String_2 is longer
	Num_Characters_Off : Natural := 0;
	Index_Offset : Integer := 0;
begin
	if Num_Misspelled_Characters_Allowed < abs String_2_Additional_Characters then
		-- if there are too many or two few characters, we can return immediately
		return False;
	end if;
   
	if String_2_Additional_Characters = 0 then
		-- they are the same size
		for I in 1 .. Length(String_1) loop
			if Element(String_1, I) /= Element(String_2, I) then
				Num_Characters_Off := Num_Characters_Off + 1;
				if Num_Characters_Off > Num_Misspelled_Characters_Allowed then
					return False;
				end if;
			end if;
		end loop;
		return True;
	-- else
	-- 	-- they are different sizes
	   
	-- 	-- Only allows for one mistake!
	   
	-- 	-- idk if this works ......? Untested....
	   
	-- 	if Length(String_1) > Length(String_2) then
	-- 		-- add the offset to String_1 since it is longer
	-- 		for I in 1 .. Length(String_1) loop
			   
	-- 			if I - Index_Offset < Length(String_2) and then I - Index_Offset >= 0 and then Element(String_1, I) /= Element(String_2, I - Index_Offset) then
			   
	-- 				Num_Characters_Off := Num_Characters_Off + 1;
	-- 				Index_Offset := Index_Offset + 1;
				   
	-- 				if Num_Characters_Off > Num_Misspelled_Characters_Allowed then
	-- 					return False;
	-- 				end if;
	-- 			end if;
	-- 		end loop;
		   
	-- 	elsif Length(String_1) < Length(String_2) then
	-- 		-- add the offset to String_2 since it is longer
	-- 		for I in 1 .. Length(String_2) loop
			   
	-- 			if I - Index_Offset < Length(String_1) and then I - Index_Offset >= 0 and then Element(String_2, I) /= Element(String_1, I - Index_Offset) then
			   
	-- 				Num_Characters_Off := Num_Characters_Off + 1;
	-- 				Index_Offset := Index_Offset + 1;
				   
	-- 				if Num_Characters_Off > Num_Misspelled_Characters_Allowed then
	-- 					return False;
	-- 				end if;
	-- 			end if;
	-- 		end loop;
	-- 	end if;
	   
	   
	end if;
   
   
	return False;
end Almost_Equals;


----------------------------------------------------------------------
-- This function will remove all of the words from Word_Options that are not in the dictionary of words
----------------------------------------------------------------------
procedure Remove_Invalid_Word_Options is
	Temp_Options : Unbounded_String_Vector;
	Num_Misspelled_Characters_Allowed : Natural := 1;
begin
	Temp_Options := Unbounded_String_Vectors.Empty_Vector;
   
	for E of Word_Options loop
	   
		-- Check if it is in the Dictionary
		for I in 1 .. Dictionary_Words'Length loop
			if E = Dictionary_Words(I) then
				Temp_Options.Append(E);
				exit;
			elsif Num_Misspelled_Characters_Allowed /= 0 and then Almost_Equals(E, Dictionary_Words(I), Num_Misspelled_Characters_Allowed) then
				Temp_Options.Append(E);
				exit;
			end if;
		end loop;
	   
	end loop;
   
	Word_Options := Temp_Options;
end Remove_Invalid_Word_Options;


--------------------------------------------------------------------
-- This function will generate all options and then remove the invalid words
--------------------------------------------------------------------
procedure Add_Word_To_Sentence_Options(Generate_All_Words : Boolean) is
begin
	-- generate all the options for this word
	Generate_Word_Options;
	--Print_Unbounded_String_Vector(Word_Options);
	--Put_Line("-------");
   
	-- remove the bad words
	if not Generate_All_Words then
		Remove_Invalid_Word_Options;
	end if;
   
	-- add this word to the final sentence options
	Generate_Sentence_Options;
   
	--Print_Unbounded_String_Vector(All_Word_Options);
	--Put_Line("*******************");
   
	-- clear globals for the next iteration (word) to use
	Full_Permutation_List := Unbounded_String_Vector_Vectors.Empty_Vector;
	Word_Options := Unbounded_String_Vectors.Empty_Vector;
end Add_Word_To_Sentence_Options;



---------------------------------------------------------------
procedure Add_Space_To_Sentence_Options is
	Temp_Options : Unbounded_String_Vector;
begin
	-- add a space
	Temp_Options := Unbounded_String_Vectors.Empty_Vector;
	for E of Sentence_Options loop -- 'Z' is Element in Word_Options
		Temp_Options.Append(E & To_Unbounded_String(" "));
	end loop;
	Sentence_Options := Temp_Options;
end Add_Space_To_Sentence_Options;



----------------------------------------------------------------------
-- Generate the 1223114 type permutation string vectors
----------------------------------------------------------------------
procedure Generate_Permutation_List(Chunk_Length : Natural) is
begin
	-- if this current length cost doesn't already have a permutation, generate and add it to the database.
	if not Number_To_Unbounded_String_Vector_Hash.Contains(Cost_Permutations_Map, Chunk_Length) then
		-- generate the permutation
		Get_Permutations(To_Unbounded_String(""), Chunk_Length);
		-- then add it to the database map
		Number_To_Unbounded_String_Vector_Hash.Insert(Cost_Permutations_Map, Chunk_Length, Permutation_List);
	else
		-- pull the permutation_list from the database
		Permutation_List := Number_To_Unbounded_String_Vector_Hash.Element(Cost_Permutations_Map, Chunk_Length);
	end if;
end Generate_Permutation_List;


----------------------------------------------------------------------
-- Generate the abbccad type permutation string vectors
----------------------------------------------------------------------
procedure Generate_Translated_Permutation_List(Chunk_Number : Natural) is
	Translated_String : Unbounded_String := To_Unbounded_String("");
begin
	-- Translate the permutation and add it to the Fullt List
	for E of Permutation_List loop
		Translated_String := Convert_To_Letters(E, Chunk_Number);
		if Translated_String /= To_Unbounded_String("") then
			-- only add valid strings (not strings with an invalid "4" in them)
			Translated_Permutation_List.Append(Translated_String);
		end if;
	end loop;
end Generate_Translated_Permutation_List;


----------------------------------------------------------------------
procedure Elbonian_Decrypt (Encrypted_String : Unbounded_String; Generate_All_Words : Boolean) is
	Num_Chunks : Natural := Get_Num_Chunks(Encrypted_String);
	Chunk_Lengths : Natural_Array(1 .. Num_Chunks) := Get_Chunk_Lengths(Encrypted_String, Num_Chunks);
	Chunk_Numbers : Natural_Array(1 .. Num_Chunks) := Get_Chunk_Numbers(Encrypted_String, Num_Chunks);
begin
   
	for I in 1 .. Num_Chunks loop
	   
		if Chunk_Numbers(I) = 0 then
			Add_Word_To_Sentence_Options(Generate_All_Words);
			Add_Space_To_Sentence_Options;
		else
			Generate_Permutation_List(Chunk_Lengths(I));
			--Print_Permutation_List;
		   
			Generate_Translated_Permutation_List(Chunk_Numbers(I));
			--Print_Translated_Permutation_List;
		   
			Full_Permutation_List.Append(Translated_Permutation_List);
		end if;
	   
		-- clear this Permutation_List for the next iteration's cost
		Permutation_List := Unbounded_String_Vectors.Empty_Vector;
		Translated_Permutation_List := Unbounded_String_Vectors.Empty_Vector;
	end loop;
   
	-- Do the last one. We don't expect a terminating zero
	Add_Word_To_Sentence_Options(Generate_All_Words);
   
	--Full_Permutation_List.Append(Translated_Permutation_List);
   
	--Print_Full_Permutation_List;
	--Generate_All_Options; --(to use, comment out generate_Word if block)
	--Generate_All_Words;
   
end Elbonian_Decrypt;








---------------------------------------------------------------------
-- This function will ask for an input string, print the encryption, then print the decryption.
---------------------------------------------------------------------
procedure Main is
	Input_String : Unbounded_String;
	Encrypted_String : Unbounded_String;
begin
	Set_Keyboard_Maps;
	--Print_Letter_To_Number_Map;
	--Print_Number_To_Letter_Map;
   
	Input_String := To_Unbounded_String(Get_Line);
	Put_Line("Input:");
	Put_Line(Input_String);
   
	Encrypted_String := Elbonian_Encrypt(Input_String);
	Put_Line("Encrypted:");
	Put_Line(Encrypted_String);
   
	-- True to generate all options, False to only Generate those in the dictionary
	Elbonian_Decrypt(Encrypted_String, False);
	Put_Line("Decrypted:");
	-- Print_Full_Permutation_List;
	--Print_All_Options;
	Print_Unbounded_String_Vector(Sentence_Options);
end Main;



-------------------------------------------------------

begin
	-- this is the main function that runs the other code examples
	Main;
end Elbonian;
