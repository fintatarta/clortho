with Ada.Strings.Maps;

package Password_Style_Parsers is
   type Set_Array is array (Natural range <>) of Ada.Strings.Maps.Character_Set;

   type Password_Style_Descriptor (N_Sets : Natural) is
      record
         Prohibited : Ada.Strings.Maps.Character_Set;
         Mandatory  : Set_Array (1 .. N_Sets);
      end record;

   type Exit_Status is (Ok, Parsing_Error);

   type Parsing_Result (Success : Exit_Status;
                        N_Sets  : Natural) is
      record
         case Success is
            when Ok =>
               Style : Password_Style_Descriptor (N_Sets);

            when others =>
               null;
         end case;
      end record;

   --  Parse the input string and return the password style descriptor
   function Parse (Input : String) return Parsing_Result;

   --  Create a string representation of the descriptor. Useful for debug.
   function Image (Descr : Password_Style_Descriptor) return String;

   --  Return the characters that are not specified in the descriptor
   function Missing (Descr : Password_Style_Descriptor)
                     return Ada.Strings.Maps.Character_Set;

   --  Return true if all the sets in the descriptor (both prohibited and
   --  mandatory) are disjoint
   function Is_Valid (Descr : Password_Style_Descriptor) return Boolean;
end Password_Style_Parsers;
