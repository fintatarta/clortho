package Clortho.Command_Line.Option_Sets is

   type Option_Set is private;

   procedure Set_Default_Value (Item : out Option_Set);

   function Is_Action_Defined (Item : Option_Set) return Boolean;

   function Action (Item : Option_Set) return Command_Type;

   procedure Do_Get_Old_Password (Item : in out Option_Set;
                                  N    : Positive)
     with
       Pre => not Is_Action_Defined (Item),
       Post =>
         Is_Action_Defined (Item)
         and Action (Item) = Get_Old_Password;

   procedure Do_Create_Entry (Item : in out Option_Set)
     with
       Pre => not Is_Action_Defined (Item),
       Post =>
         Is_Action_Defined (Item)
         and Action (Item) = Create_Entry;

   procedure Do_Renew_Password (Item : in out Option_Set)
     with
       Pre => not Is_Action_Defined (Item),
       Post =>
         Is_Action_Defined (Item)
         and Action (Item) = Renew_Password;

   procedure Do_Vacuum (Item : in out Option_Set)
     with
       Pre => not Is_Action_Defined (Item),
       Post =>
         Is_Action_Defined (Item)
         and Action (Item) = Vacuum_Entry;

   procedure Do_Full_Vacuum (Item : in out Option_Set)
     with
       Pre => not Is_Action_Defined (Item),
       Post =>
         Is_Action_Defined (Item)
         and Action (Item) = Vacuum_All;

   procedure Do_Delete (Item : in out Option_Set)
     with
       Pre => not Is_Action_Defined (Item),
       Post =>
         Is_Action_Defined (Item)
         and Action (Item) = Delete_Entry;

   procedure Do_Roll_Back (Item : in out Option_Set)
     with
       Pre => not Is_Action_Defined (Item),
       Post =>
         Is_Action_Defined (Item)
         and Action (Item) = Roll_Back_Entry;

   function Is_Password_Provided (Item : Option_Set) return Boolean;

   function User_Password (Item : Option_Set) return String
     with
       Pre => Is_Password_Provided (Item);

   procedure Set_User_Password (Item     : in out Option_Set;
                                Password : Unbounded_String)
     with
       Pre => not Is_Password_Provided (Item),
       Post =>
         Is_Password_Provided (Item)
         and then User_Password (Item) = To_String (Password);

   function Is_Password_N_Char_Specified (Item : Option_Set) return Boolean;

   function Is_Password_N_Bits_Specified (Item : Option_Set) return Boolean;

   function Is_Password_Length_Specified (Item : Option_Set) return Boolean
   is (Is_Password_N_Bits_Specified (Item)
       or Is_Password_N_Char_Specified (Item));

   function Password_Nbits (Item : Option_Set) return Positive
     with
       Pre => Is_Password_N_Bits_Specified (Item);

   function Password_Nchars (Item : Option_Set) return Positive
     with
       Pre => Is_Password_N_Char_Specified (Item);

   procedure Set_Password_Nbits (Item   : in out Option_Set;
                                 N_Bits : Positive)
     with
       Pre => not Is_Password_Length_Specified (Item),
       Post =>
         (Is_Password_N_Bits_Specified (Item)
          and not Is_Password_N_Char_Specified (Item))
         and then Password_Nbits (Item) = N_Bits;

   procedure Set_Password_Nchars (Item    : in out Option_Set;
                                  N_Chars : Positive)
     with
       Pre => not Is_Password_Length_Specified (Item),
       Post =>
         (Is_Password_N_Char_Specified (Item)
          and not Is_Password_N_Bits_Specified (Item))
         and then Password_Nchars (Item) = N_Chars;

   function Is_Password_Spec_Defined (Item : Option_Set) return Boolean;

   function Password_Spec (Item : Option_Set) return String
     with
       Pre => Is_Password_Spec_Defined (Item);

   procedure Set_Password_Specs (Item  : in out Option_Set;
                                 Specs : Unbounded_String)
     with
       Pre =>
         not Is_Password_Spec_Defined (Item),
         Post =>
           Is_Password_Spec_Defined (Item)
           and then Password_Spec (Item) = To_String (Specs);

   function Source (Item : Option_Set) return Source_Name;

   procedure Use_Source (Item   : in out Option_Set;
                         Source : Source_Name);

   function Target (Item : Option_Set) return Target_Name;

   procedure Use_Target (Item   : in out Option_Set;
                         Target : Target_Name);

private
   type Option_Set is
      record
         To_Do                  : Command_Type;
         Back_Step              : Natural;
         Password_Len           : Natural;
         Password_N_Bits        : Natural;
         User_Provided_Password : Unbounded_String;
         Output_Target          : Target_Name;
         Specs                  : Unbounded_String;
         Use_Standard_Input     : Boolean;
      end record;
end Clortho.Command_Line.Option_Sets;
