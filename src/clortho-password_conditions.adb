pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;
package body Clortho.Password_Conditions is
   pragma SPARK_Mode;

   ------------
   -- Create --
   ------------

   function Create
     (Prohibited : Strings.Maps.Character_Set := Strings.Maps.Null_Set)
      return Condition_Type
   is
   begin
      return Condition_Type'(Prohibited            => Prohibited,
                             Elementary_Conditions => (others => Bogus_Condition),
                             First_Free            => Simple_Condition_ID'First);
   end Create;

   -------------------
   -- Allowed_Chars --
   -------------------

   function Allowed_Chars
     (Condition : Condition_Type) return Strings.Maps.Character_Set
   is
      Result : Strings.Maps.Character_Set := Null_Set;
   begin
      for I in Condition.Elementary_Conditions'First .. Condition.First_Free - 1 loop
         Result := Result or Condition.Elementary_Conditions (I).Set;
      end loop;

      return Result;
   end Allowed_Chars;

   --------------------
   -- Prohibited_Set --
   --------------------

   function Prohibited_Set
     (Condition : Condition_Type) return Strings.Maps.Character_Set
   is (Condition.Prohibited);

   --------------
   -- To_Limit --
   --------------

   function To_Limit (N : Repetition_Count) return Repetition_Limit
   is (Repetition_Limit (N));

   ----------
   -- "<=" --
   ----------

   overriding function "<=" (X, Y : Repetition_Limit) return Boolean
   is (Integer (X) <= Integer (Y));

   -------------------
   -- Add_Condition --
   -------------------

   procedure Add_Condition
     (To  : in out Condition_Type;
      Set : Strings.Maps.Character_Set;
      Min : Repetition_Limit;
      Max : Repetition_Limit)
   is
      New_Cond : constant Simple_Condition := Simple_Condition'(Set => Set and not To.Prohibited,
                                                                Min => Min,
                                                                Max => Max);
   begin
      if Max < Min then
         raise Bad_Limits;
      end if;

      pragma Warnings (Off, "condition can only be True if invalid values present");
      if To.First_Free > To.Elementary_Conditions'Last then
         --  This should never happen
         raise Condition_Full;
      end if;

      pragma Warnings (On, "condition can only be True if invalid values present");

      for I in To.Elementary_Conditions'First .. To.First_Free - 1 loop
         if (New_Cond.Set and To.Elementary_Conditions (I).Set) /= Strings.Maps.Null_Set then
            raise Not_A_Partition;
         end if;
      end loop;

      To.Elementary_Conditions (To.First_Free) := New_Cond;
      To.First_Free := To.First_Free + 1;
   end Add_Condition;

   -------------------
   -- Add_Mandatory --
   -------------------

   procedure Add_Mandatory (To  : in out Condition_Type;
                            Set : Strings.Maps.Character_Set)
   is
   begin
      Put_Line ("[[" & To_Sequence (Set) & "]]");
      Put_Line ("<<" & To_Sequence (Allowed_Chars (To)) & ">>");

      Add_Condition (To  => To,
                     Set => Set,
                     Min => To_Limit (1),
                     Max => Infinity);
   end Add_Mandatory;

   ------------------
   -- Add_Optional --
   ------------------

   procedure Add_Optional (To  : in out Condition_Type;
                           Set : Strings.Maps.Character_Set)
   is
   begin
      Add_Condition (To  => To,
                     Set => Set,
                     Min => To_Limit (0),
                     Max => Infinity);
   end Add_Optional;

   -----------
   -- Match --
   -----------

   function Match (Item      : String;
                   Condition : Condition_Type)
                   return Boolean
   is

      Char_To_Id_Map : array (Character) of Extended_Condition_ID := (others => No_Id);

      procedure Fill_Char_To_Id_Map;

      procedure Fill_Char_To_Id_Map is
      begin
         for I in Condition.Elementary_Conditions'First .. Condition.First_Free - 1 loop
            declare
               Seq : constant String := To_Sequence (Condition.Elementary_Conditions (I).Set);
            begin
               for C of Seq loop
                  if Char_To_Id_Map (C) /= No_Id then
                     raise Not_A_Partition;
                  end if;

                  Char_To_Id_Map (C) := I;
               end loop;
            end;
         end loop;

         for C in Char_To_Id_Map'Range loop
            if (Char_To_Id_Map (C) = No_Id) /= (Is_In (C, Condition.Prohibited)) then
               raise Not_A_Partition;
            end if;
         end loop;
      end Fill_Char_To_Id_Map;

      subtype Valid_Id is Simple_Condition_ID range Simple_Condition_ID'First .. Condition.First_Free - 1;

      Counters : array (Valid_Id) of Repetition_Limit := (others => 0);

   begin
      Fill_Char_To_Id_Map;

      for C of Item loop
         if Char_To_Id_Map (C) = No_Id then
            return False;
         else
            Counters (Char_To_Id_Map (C)) := Counters (Char_To_Id_Map (C)) + 1;
         end if;
      end loop;

      for ID in Valid_Id loop
         if Counters (ID) < Condition.Elementary_Conditions (ID).Min
           or Counters (ID) > Condition.Elementary_Conditions (ID).Max
         then
            return False;
         end if;
      end loop;

      return True;
   end Match;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid
     (Prob       : Strings.Maps.Character_Set;
      Elementary : Simple_Condition_Array;
      Free       : Simple_Condition_ID)
      return Boolean
   is
   begin
      for I in Elementary'First .. Free - 1 loop
         --  Every element in the first segment must be a valid one with a non empty set
         if
           Elementary (I).Set = Ada.Strings.Maps.Null_Set
           or Elementary (I).Min = Infinity
           or Elementary (I).Max < Elementary (I).Min
         then
            return False;
         end if;

         --  The set must have no intersection with the prohibited set
         if (Prob and Elementary (I).Set) /= Ada.Strings.Maps.Null_Set then
            return False;
         end if;

         for J in I + 1 .. Free - 1 loop
            --  No intersection with the other sets
            if (Elementary (J).Set and Elementary (I).Set) /= Ada.Strings.Maps.Null_Set then
               return False;
            end if;
         end loop;
      end loop;

      for I in Free .. Elementary'Last loop
         --  The unused segment must contain only bogus conditions
         if
           Elementary (I).Set /= Ada.Strings.Maps.Null_Set
           or Elementary (I).Min /= Infinity
         then
            return False;
         end if;
      end loop;

      --  Passed all the checks!  Yeah!
      return True;
   end Is_Valid;

end Clortho.Password_Conditions;
