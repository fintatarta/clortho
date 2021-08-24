with Ada.Strings.Maps;

package Password_Style_Parsers is
   type Set_Array is array (Natural range <>) of Ada.Strings.Maps.Character_Set;

   type Password_Style_Descriptor (N_Sets : Natural) is
      record
         Prohibited : Ada.Strings.Maps.Character_Set;
         Mandatory  : Set_Array (1 .. N_Sets);
      end record;

   --  Parse the input string and return the password style descriptor
   function Parse (Input : String) return Password_Style_Descriptor;

   --  Create a string representation of the descriptor. Mostly useful for debug.
   function Image (Descr : Password_Style_Descriptor) return String;

   --  Return the characters that are not specified in the descriptor
   function Missing (Descr : Password_Style_Descriptor) return Ada.Strings.Maps.Character_Set;

   --  Return true if all the sets in the descriptor (both prohibited and mandatory) are
   --  disjoint
   function Is_Valid (Descr : Password_Style_Descriptor) return Boolean;

   Parsing_Error : exception;
end Password_Style_Parsers;
