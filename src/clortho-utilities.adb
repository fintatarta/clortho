pragma Ada_2012;
package body Clortho.Utilities is
   pragma SPARK_Mode;

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With (Item : String; Tail : String) return Boolean is
   begin
      if Item'Length < Tail'Length then
         return False;
      end if;

      pragma Assert (Item'Length > 0);

      declare
         From : constant Positive := Item'Last - Tail'Length + 1;
         S    : constant String := Item (From .. Item'Last) with Ghost;
      begin
         pragma Assert (S'Length = Tail'Length);

         return Item (From .. Item'Last) = Tail;
      end;
   end Ends_With;

end Clortho.Utilities;
