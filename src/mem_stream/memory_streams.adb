pragma Ada_2012;
package body Memory_Streams is
   ------------
   -- Create --
   ------------

   function Create (Data : Stream_Element_Array) return Memory_Stream is

   begin
      return Result : Memory_Stream := Create (Data'Length) do
         Write (Result, Data);
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Size : Stream_Element_Count) return Memory_Stream
   is (Memory_Stream'(Root_Stream_Type with
         Size          => Size,
       Buffer        => (others => Stream_Element'First),
       Next_To_Read  => 1,
       Next_To_Write => 1));


   ------------
   -- Resize --
   ------------

   function Resize
     (Stream : Memory_Stream; Size : Stream_Element_Count) return Memory_Stream
   is
   begin
      return Result : Memory_Stream := Create (Size) do
         Write (Result, Dump (Stream));
      end return;
   end Resize;

   ----------------
   -- Free_Space --
   ----------------

   function Free_Space (Stream : Memory_Stream) return Stream_Element_Count
   is (Stream.Buffer'Last - Stream.Next_To_Write + 1);

   --------------
   -- Capacity --
   --------------

   function Capacity (Stream : Memory_Stream) return Stream_Element_Offset
   is (Stream.Buffer'Length);

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Memory_Stream;
      Item   : out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      Last_To_Read  : Stream_Element_Offset;
      Last_Readable : constant Stream_Element_Offset := Stream.Next_To_Write - 1;
   begin
      if
        Item'Length = 0
        or Last_Readable < Stream.Next_To_Read
      then
         Last := Item'First - 1;
         return;
      end if;

      pragma Assert (Item'Length > 0);

      Last_To_Read := Stream.Next_To_Read + Item'Length - 1;


      if Last_To_Read > Last_Readable then
         Last_To_Read := Last_Readable;
      end if;

      pragma Assert (Last_To_Read >= Stream.Next_To_Read);
      pragma Assert (Last_To_Read <= Last_Readable);

      declare
         To_Transfer : constant Stream_Element_Count :=
                         Last_To_Read - Stream.Next_To_Read + 1;
      begin
         Last := Item'First + To_Transfer - 1;

         pragma Assert (Last <= Item'Last);
         pragma Assert (Last - Item'First = Last_To_Read - Stream.Next_To_Read);

         Item (Item'First .. Last) :=
           Stream.Buffer (Stream.Next_To_Read .. Last_To_Read);

         Stream.Next_To_Read := Last_To_Read + 1;
      end;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Memory_Stream; Item : in Stream_Element_Array)
   is
      Last : Stream_Element_Offset;
   begin
      if Free_Space (Stream) < Item'Length then
         raise Capacity_Error;
      end if;

      Last := Stream.Next_To_Write + Item'Length - 1;

      pragma Assert (Last < Stream.Buffer'Last);

      Stream.Buffer (Stream.Next_To_Write .. Last) := Item;
      Stream.Next_To_Write := Last + 1;
   end Write;

   ----------
   -- Dump --
   ----------

   function Dump (Stream : Memory_Stream) return Stream_Element_Array
   is (Stream.Buffer (Stream.Buffer'First .. Stream.Next_To_Write - 1));

end Memory_Streams;
