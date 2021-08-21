pragma Ada_2012;
with Clortho.Random.Source;
with Ada.Numerics.Elementary_Functions;

package body Clortho.Random is

   ----------
   -- Fill --
   ----------

   procedure Fill (Data : out Random_Data;
                   Max  : Positive;
                   Min  : Positive := 1)
   is
      --
      --  It is worth spending a couple of words about the algorithm since
      --  we need to generate values that belong to an alphabet whose size
      --  is not necessarily a power of 2 using a source that produces random
      --  bits.
      --
      --  This function relies on a low-level function that returns random
      --  words (let B the word length).  Let S be the alphabet size.  The
      --  idea is to transform the random B-bit word in a K-length string
      --  of symbols belonging to the alphabet by doing a "base K" expansion.
      --
      --  Since we have B bit in the random word, K must satisfy
      --
      --     S**K <= 2**B
      --
      --  that is equivalent to
      --
      --      K <= B/log2(S)
      --
      --  Therefore, the best K is K_max = floor(B / log2(S))
      --
      --  The number of K_max-element alphabet strings is
      --
      --      N = S ** K_max
      --
      --  Any random bit word >= N must be discarded because it does not
      --  correspond to any alphabet word.  The number of bits necessary
      --  to represent integers between 0 and N-1 is equal to
      --
      --     B_0 = ceil(log2(N)) = ceil(K_max * log2(S))
      --
      --  If B_0 < B, any excess bit can be forced to zero. The resulting
      --  B_0 bit word will be uniformly distributed anyway.
      --
      --  The probabilty of getting a "good" B_0-bit word is
      --
      --      N / 2**B_0
      --        = (2 ** log2 (N)) / (2 ** ceil (log2 (N)))
      --        = (1/2) ** (ceil(log2 (N)) - log2 (N))
      --        >= 1/2
      --
      --  since ceil(x)-x <= 1.  This means that, on average, we will discard
      --  at most one word out of two.
      --
      use Source;

      use Ada.Numerics.Elementary_Functions;

      Alphabet_Size : constant Positive := Natural (Max) - Natural (Min) + 1;
      Alphabet_Bits : constant Float := Log (Float (Alphabet_Size), 2.0);

      Block_Size    : constant Positive :=
                        Positive (Float'Floor (Float (Word'Size) / Alphabet_Bits));

      Useful_Bits   : constant Positive :=
                        Positive (Float'Ceiling (Float (Block_Size) * Alphabet_Bits));

      Truncation_Modulus : constant Word := 2 ** Useful_Bits;

      subtype Block_Type is Random_Data (1 .. Block_Size);

      type Buffer_Type is
         record
            Cursor : Positive;
            Buffer : Block_Type;
         end record;

      function Is_Empty (B : Buffer_Type) return Boolean
      is (B.Cursor > B.Buffer'Last);

      function Next (B : in out Buffer_Type) return Positive
        with
          Pre => not Is_Empty (B),
          Post => B.Cursor = B.Cursor'Old + 1;

      function Next (B : in out Buffer_Type) return Positive
      is
      begin
         B.Cursor := B.Cursor + 1;
         return B.Buffer (B.Cursor - 1);
      end Next;

      procedure Refill (B : out Buffer_Type)
        with
          Post => B.Cursor = B.Buffer'First;

      procedure Refill (B : out Buffer_Type)
      is
      begin
         null;
      end Refill;

      Buffer        : Buffer_Type;
      Cursor   : Positive := Data'First;
      Random_Word   : Source.Word;
   begin
      Refill (Buffer);
      while Cursor <= Data'Last loop
         if Is_Empty (Buffer) then
            Refill (Buffer);
         end if;

         pragma Assert (not Is_Empty (Buffer));

         Data (Cursor) := Next (Buffer);
         Cursor := Cursor + 1;
      end loop;
   end Fill;

end Clortho.Random;
