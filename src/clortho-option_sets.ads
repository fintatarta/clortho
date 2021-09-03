with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Clortho.Flagged_Types;       use Clortho.Flagged_Types;
with Clortho.Commands;            use Clortho.Commands;

with Generic_Flagged_Types;

package Clortho.Option_Sets is
   package Flagged_Commands is
     new Generic_Flagged_Types (Root_Type     => Command_Type,
                                Default_Value => Get_Password);

   subtype Flagged_Command is Flagged_Commands.Flagged_Type;

   function Is_Defined (X : Flagged_Command) return Boolean
                        renames Flagged_Commands.Is_Defined;

   function Is_Defined (X : Flagged_String) return Boolean
                        renames Flagged_Strings.Is_Defined;

   function Is_Defined (X : Flagged_Natural) return Boolean
                        renames Flagged_Naturals.Is_Defined;

   function Is_Defined (X : Flagged_Positive) return Boolean
                        renames Flagged_Positives.Is_Defined;

   type Char_Length is new Flagged_Positive;

   type Entropy is new Flagged_Positive;

   type Option_Set is private;

   procedure Set_Default_Value (Item : out Option_Set);

   function Action (Item : Option_Set) return Flagged_Command;

   function Action (Item : Option_Set) return Command_Type
     with
       Pre => Flagged_Commands.Is_Defined (Action (Item));

   procedure Do_Get_Password (Item : in out Option_Set;
                              N    : Natural)
     with
       Pre => not Is_Defined (Action (Item)),
       Post =>
         Is_Defined (Action (Item))
         and (Action (Item) = Get_Password
              and then Password_Version (Item) = N);

   function Password_Version (Item : Option_Set) return Natural
     with
       Pre => Is_Defined (Action (Item)) and then Action (Item) = Get_Password;

   procedure Do_Create_Entry (Item : in out Option_Set)
     with
       Pre => not Is_Defined (Action (Item)),
       Post =>
         Is_Defined (Action (Item))
         and Action (Item) = Create_Entry;

   procedure Do_Renew_Password (Item : in out Option_Set)
     with
       Pre => not Is_Defined (Action (Item)),
       Post =>
         Is_Defined (Action (Item))
         and Action (Item) = Renew_Password;

   procedure Do_Vacuum (Item : in out Option_Set)
     with
       Pre => not Is_Defined (Action (Item)),
       Post =>
         Is_Defined (Action (Item))
         and Action (Item) = Vacuum_Entry;

   procedure Do_Full_Vacuum (Item : in out Option_Set)
     with
       Pre => not Is_Defined (Action (Item)),
       Post =>
         Is_Defined (Action (Item))
         and Action (Item) = Vacuum_All;

   procedure Do_Delete (Item : in out Option_Set)
     with
       Pre => not Is_Defined (Action (Item)),
       Post =>
         Is_Defined (Action (Item))
         and Action (Item) = Delete_Entry;

   procedure Do_Roll_Back (Item : in out Option_Set)
     with
       Pre => not Is_Defined (Action (Item)),
       Post =>
         Is_Defined (Action (Item))
         and Action (Item) = Roll_Back_Entry;

   function User_Password (Item : Option_Set) return Flagged_String;

   function User_Password (Item : Option_Set) return Unbounded_String
   is (Flagged_Strings.Value (User_Password (Item)))
     with
       Pre => Is_Defined (User_Password (Item));

   procedure Set_User_Password (Item     : in out Option_Set;
                                Password : Unbounded_String)
     with
       Pre => not Is_Defined (User_Password (Item)),
       Post =>
         Is_Defined (User_Password (Item))
         and then User_Password (Item) = Password;

   function Password_Nbits (Item : Option_Set) return Entropy;

   function Password_Nchars (Item : Option_Set) return Char_Length;

   function Is_Password_Length_Specified (Item : Option_Set) return Boolean
   is (Is_Defined (Password_Nbits (Item))
       or Is_Defined (Password_Nchars (Item)));

   procedure Set_Password_Nbits (Item   : in out Option_Set;
                                 N_Bits : Positive)
     with
       Pre => not Is_Password_Length_Specified (Item),
       Post =>
         (Is_Defined (Password_Nbits (Item))
          and not Is_Defined (Password_Nchars (Item)))
         and then Flagged_Positives.Value
           (Flagged_Positive (Password_Nbits (Item))) = N_Bits;

   procedure Set_Password_Nchars (Item    : in out Option_Set;
                                  N_Chars : Positive)
     with
       Pre => not Is_Defined (Password_Nbits (Item)),
       Post =>
         Is_Defined (Password_Nbits (Item))
         and then Flagged_Positives.Value
           (Flagged_Positive (Password_Nchars (Item))) = N_Chars;

   function Password_Spec (Item : Option_Set) return Flagged_String;

   procedure Set_Password_Specs (Item  : in out Option_Set;
                                 Specs : Unbounded_String)
     with
       Pre =>
         not Is_Defined (Password_Spec (Item)),
         Post =>
           Is_Defined (Password_Spec (Item))
           and then Flagged_Strings.Value (Password_Spec (Item)) = Specs;

   function Source (Item : Option_Set) return Source_Name;

   procedure Use_Source (Item   : in out Option_Set;
                         Source : Source_Name);

   function Target (Item : Option_Set) return Target_Name;

   procedure Use_Target (Item   : in out Option_Set;
                         Target : Target_Name);

private
   type Option_Set is
      record
         To_Do                  : Flagged_Command;
         Back_Step              : Natural;
         Password_Len           : Char_Length;
         Password_N_Bits        : Entropy;
         User_Provided_Password : Flagged_String;
         Output_Target          : Target_Name;
         Specs                  : Flagged_String;
         Use_Standard_Input     : Boolean;
      end record;
end Clortho.Option_Sets;
