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
   message : Long_Long_Integer;
   encryptedMessage : Long_Long_Integer;
   decryptedMessage : Long_Long_Integer;

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

   function FastModularExponentiation (b, exp, m : Long_Long_Integer) return Long_Long_Integer
   is
      x : Long_Long_Integer := 1;
      power : Long_Long_Integer;
      str : String := DecimalToBinary (Integer(exp));
   begin

      power := b mod m;

      for i in 0 .. (str'Length - 1) loop
         if str(str'Last - i) = '1' then
            x := (x * power) mod m;
         end if;

         power := (power*power) mod m;
         --power := FastModularExponentiation(power, power, m);
      end loop;

      return x;
   end FastModularExponentiation;

   -------------
   -- Encrypt --
   -------------

   function Encrypt (M, N, E : Long_Long_Integer) return Long_Long_Integer
   is
   begin
      return FastModularExponentiation(M, E, N);
   end Encrypt;

   -------------
   -- Decrypt --
   -------------

   function Decrypt (C, D, N : Long_Long_Integer) return Long_Long_Integer
   is
   begin
      return FastModularExponentiation(C, D, N);
   end Decrypt;

begin

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line("You forgot to pass in all the arguments! Try --help.");
      return;
   end if;

   	if Ada.Command_Line.Argument(1) = "--help" then

		Ada.Text_IO.Put_Line("A simple CLI RSA program.");
   		Ada.Text_IO.New_Line;
   		Ada.Text_IO.Put_Line("Usage: ");
   		Ada.Text_IO.Put_Line("    rsa <p> <q> <e> <message>");
   		Ada.Text_IO.Put_Line("    rsa encrypt <n> <e> <message>");
   		Ada.Text_IO.Put_Line("    rsa decrypt <d> <n> <message>");
      	return;
   	end if;

   if Ada.Command_Line.Argument(1) = "encrypt" then
      n := Long_Long_Integer'Value(Ada.Command_Line.Argument(2));
      e := Long_Long_Integer'Value(Ada.Command_Line.Argument(3));
      message := Long_Long_Integer'Value(Ada.Command_Line.Argument(4));
      encryptedMessage := Encrypt(message, n, e);
      Ada.Text_IO.Put("encrypted message = ");
      Ada.Text_IO.Put_Line(Long_Long_Integer'Image(encryptedMessage));
      return;
   end if;

   if Ada.Command_Line.Argument(1) = "decrypt" then
      d := Long_Long_Integer'Value(Ada.Command_Line.Argument(2));
      n := Long_Long_Integer'Value(Ada.Command_Line.Argument(3));
      encryptedMessage := Long_Long_Integer'Value(Ada.Command_Line.Argument(4));
      decryptedMessage := Decrypt(message, d, n);
      Ada.Text_IO.Put("decrypted message = ");
      Ada.Text_IO.Put_Line(Long_Long_Integer'Image(decryptedMessage));
      return;
   end if;

   if Ada.Command_Line.Argument_Count < 4 then
      Ada.Text_IO.Put_Line("You forgot to pass in all the arguments! Try --help.");
      return;
   end if;

   p := Long_Long_Integer'Value(Ada.Command_Line.Argument(1));
   q := Long_Long_Integer'Value(Ada.Command_Line.Argument(2));
   e := Long_Long_Integer'Value(Ada.Command_Line.Argument(3));
   message := Long_Long_Integer'Value(Ada.Command_Line.Argument(4));

   if not (IsPrimeNumber(p) and IsPrimeNumber(q)) then
      Ada.Text_IO.Put_Line("p and q has to be prime numbers.");
      return;
   end if;

   n := p * q;

   if message >= n - 1 then 
      Ada.Text_IO.Put_Line("The message has to be smaller than n");
      return;
   end if;

   phi := (p - 1) * (q - 1);

   if not (IsOdd(e) and (gcd(e, phi) = 1) and e > 1) then
      Ada.Text_IO.Put_Line("e has to be 1 < e < λ(n) and gcd(e, λ(n)) = 1");
      return;
   end if;

   d := GetPrivateExponent(phi, e);

   encryptedMessage := Encrypt(message, n, e);

   decryptedMessage := Decrypt(encryptedMessage, d, n);

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
   Ada.Text_IO.Put_Line(Long_Long_Integer'Image(message));

   Ada.Text_IO.Put("encrypted message = ");
   Ada.Text_IO.Put_Line(Long_Long_Integer'Image(encryptedMessage));

   Ada.Text_IO.Put("decrypted message = ");
   Ada.Text_IO.Put_Line(Long_Long_Integer'Image(decryptedMessage));

   null;
end Main;
