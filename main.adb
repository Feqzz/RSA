with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Command_Line;

procedure Main is

   p : Long_Long_Integer;
   q : Long_Long_Integer;
   phi : Long_Long_Integer;
   n : Long_Long_Integer;
   e : Long_Long_Integer;
   d : Long_Long_Integer;
   message : Ada.Strings.Unbounded.Unbounded_String;
   encryptedMessageArr : Array (1 .. 100) of Integer;
   decryptedMessageArr : Array (1 .. 100) of Integer;

   -------------------
   -- IsPrimeNumber --
   -------------------

   function IsPrimeNumber (N : Long_Long_Integer) return Boolean
   is
      isPrime : Boolean := true;
   begin

      if N = 0 or N = 1 then
         return false;
      end if;

      for i in 1 .. N / 2 loop
         if (N mod (i + 1)) = 0 then
            isPrime := false;
            exit;
         end if;
      end loop;

      return isPrime;
   end IsPrimeNumber;

   -----------
   -- IsOdd --
   -----------

   function IsOdd (N : Long_Long_Integer) return Boolean
   is
   begin
      return (N mod 2 = 1);
   end IsOdd;

   ---------
   -- gcd --
   ---------

   function gcd (A, B : Long_Long_Integer) return Long_Long_Integer
   is
   begin
      if A = 0 then
         return B;
      end if;

      return gcd(B mod A, A);
   end gcd;

   ------------------------
   -- GetPrivateExponent --
   ------------------------

   function GetPrivateExponent (Phi, E : Long_Long_Integer) return Long_Long_Integer
   is
      K : Long_Long_Integer := 1;
      ans : Float;
   begin

      loop
         ans := ((Float(K)*Float(Phi)) + 1.0) / Float(E);
         if not (ans = Float(Integer(ans))) then
            K := K + 1;
         else
            return ((K*Phi) + 1) / E;
         end if;
      end loop;
   end GetPrivateExponent;

   ---------------------
   -- DecimalToBinary --
   ---------------------

   function DecimalToBinary (N : Natural) return String
   is
      ret : Ada.Strings.Unbounded.Unbounded_String;
   begin

      if N < 2 then
         return "1";
      else
         Ada.Strings.Unbounded.Append(ret, Ada.Strings.Unbounded.To_Unbounded_String(decimalToBinary (N / 2)));
         Ada.Strings.Unbounded.Append(ret, Ada.Strings.Fixed.Trim(Integer'Image(N mod 2), Ada.Strings.Left));
      end if;
      return Ada.Strings.Unbounded.To_String(ret);
   end decimalToBinary;

   -------------------------------
   -- FastModularExponentiation --
   -------------------------------

   function FastModularExponentiation (b, exp, m : Natural) return Integer
   is
      x : Integer := 1;
      power : Integer;
      str : String := DecimalToBinary (exp);
   begin

      power := b mod m;

      for i in 0 .. (str'Length - 1) loop
         if str(str'Last - i) = '1' then
            x := (x * power) mod m;
         end if;

         power := (power*power) mod m;
      end loop;

      return x;
   end FastModularExponentiation;

   -------------
   -- Encrypt --
   -------------

   function Encrypt (M, N, E : Long_Long_Integer) return Long_Long_Integer
   is
   begin
      return Long_Long_Integer(FastModularExponentiation(Integer(M), Integer(E), Integer(N)));
   end Encrypt;

   -------------
   -- Decrypt --
   -------------

   function Decrypt (C, D, N : Long_Long_Integer) return Long_Long_Integer
   is
   begin
      return Long_Long_Integer(FastModularExponentiation(Integer(C), Integer(D), Integer(N)));
   end Decrypt;

begin

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line("You forgot to pass in all the arguments! Try --help.");
      return;
   end if;

   if Ada.Command_Line.Argument(1) = "--help" then
      Ada.Text_IO.Put_Line("Argument order: p, q, e, Message (String. Max 100 characters.)");
      return;
   end if;

   if Ada.Command_Line.Argument_Count < 4 then
      Ada.Text_IO.Put_Line("You forgot to pass in all the arguments! Try --help.");
      return;
   end if;

   p := Long_Long_Integer'Value(Ada.Command_Line.Argument(1));
   q := Long_Long_Integer'Value(Ada.Command_Line.Argument(2));
   e := Long_Long_Integer'Value(Ada.Command_Line.Argument(3));
   message := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Command_Line.Argument(4));

   if not (IsPrimeNumber(p) and IsPrimeNumber(q)) then
      Ada.Text_IO.Put_Line("p and q has to be prime numbers.");
      return;
   end if;

   n := p * q;
   phi := (p - 1) * (q - 1);


   if not (IsOdd(e) and (gcd(e, phi) = 1) and e > 1) then
      Ada.Text_IO.Put_Line("e has to be 1 < e < λ(n) and gcd(e, λ(n)) = 1");
      return;
   end if;

   d := GetPrivateExponent(phi, e);

   for I in 1 .. Ada.Strings.Unbounded.Length(message) loop
      encryptedMessageArr(I) := Integer(Encrypt(Character'Pos(Ada.Strings.Unbounded.Element(message, I)), n, e));
   end loop;

   for I in 1 .. Ada.Strings.Unbounded.Length(message) loop
      decryptedMessageArr(I) := Integer(Decrypt(Long_Long_Integer(encryptedMessageArr(I)), d, n));
   end loop;

   Ada.Text_IO.Put("p =");
   Ada.Text_IO.Put_Line(Long_Long_Integer'Image(p));

   Ada.Text_IO.Put("q =");
   Ada.Text_IO.Put_Line(Long_Long_Integer'Image(q));

   Ada.Text_IO.Put("e =");
   Ada.Text_IO.Put_Line(Long_Long_Integer'Image(e));

   Ada.Text_IO.Put("n =");
   Ada.Text_IO.Put_Line(Long_Long_Integer'Image(n));

   Ada.Text_IO.Put("phi =");
   Ada.Text_IO.Put_Line(Long_Long_Integer'Image(phi));

   Ada.Text_IO.Put("d =");
   Ada.Text_IO.Put_Line(Long_Long_Integer'Image(d));

   Ada.Text_IO.Put("message = ");

   for I in 1 .. Ada.Strings.Unbounded.Length(message) loop
      Ada.Text_IO.Put(Ada.Strings.Unbounded.Element(message, I));
   end loop;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("encrypted message = ");
   for I in 1 .. Ada.Strings.Unbounded.Length(message) loop
      -- To remove ASCII values below 33.
      if (encryptedMessageArr(I) mod 127) < 33 then
         Ada.Text_IO.Put(Character'Val((encryptedMessageArr(I) mod 127) + 33));
      else
         Ada.Text_IO.Put(Character'Val(encryptedMessageArr(I) mod 127));
      end if;
   end loop;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("decrypted message = ");
   for I in 1 .. Ada.Strings.Unbounded.Length(message) loop
      Ada.Text_IO.Put(Character'Val(decryptedMessageArr(I)));
   end loop;

   null;
end Main;
