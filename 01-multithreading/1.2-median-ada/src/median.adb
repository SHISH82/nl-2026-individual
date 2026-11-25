with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Strings;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Real_Time;                     use Ada.Real_Time;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Float_Random;
with System.Multiprocessors;

procedure Median is

   type Float_Array is array (Positive range <>) of Float;
   type Float_Access is access Float_Array;

   package Float_Vectors is new Ada.Containers.Vectors (Positive, Float);

   Stop : exception;

   Data   : Float_Access;
   Result : Float_Access;
   Check  : Float_Access;
   N      : Natural := 0;
   W      : Positive := 3;
   T      : Positive := Positive (System.Multiprocessors.Number_Of_CPUs);

   procedure Fail (Msg : String) with No_Return;

   procedure Fail (Msg : String) is
   begin
      Put_Line (Standard_Error, "Ошибка: " & Msg);
      raise Stop;
   end Fail;

   procedure Usage with No_Return;

   procedure Usage is
   begin
      Put_Line (Standard_Error, "Использование:");
      Put_Line (Standard_Error, "  median <файл> <окно> [задачи] [-o выходной_файл]");
      Put_Line (Standard_Error, "  median gen <количество>");
      raise Stop;
   end Usage;

   function To_Positive (S : String; Name : String) return Positive is
   begin
      return Positive'Value (S);
   exception
      when Constraint_Error =>
         Fail (Name & " должно быть положительным целым числом");
   end To_Positive;

   function Image (X : Float) return String is
      Buf : String (1 .. 40);
   begin
      if X = Float'Floor (X) and then abs X < 1.0E9 then
         return Trim (Integer'Image (Integer (X)), Ada.Strings.Left);
      end if;
      Ada.Float_Text_IO.Put (Buf, X, Aft => 3, Exp => 0);
      declare
         S    : constant String := Trim (Buf, Ada.Strings.Both);
         Last : Natural := S'Last;
      begin
         while S (Last) = '0' loop
            Last := Last - 1;
         end loop;
         if S (Last) = '.' then
            Last := Last - 1;
         end if;
         return S (S'First .. Last);
      end;
   end Image;

   function Is_Space (C : Character) return Boolean is
   begin
      return C = ' ' or else C = ASCII.HT or else C = ASCII.CR;
   end Is_Space;

   procedure Read_Signal (Path : String; Values : in out Float_Vectors.Vector) is
      F     : File_Type;
      Count : Natural := 0;
   begin
      begin
         Open (F, In_File, Path);
      exception
         when others =>
            Fail ("не удалось открыть файл " & Path);
      end;
      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
            I    : Integer := Line'First;
            J    : Integer;
         begin
            while I <= Line'Last loop
               if Is_Space (Line (I)) then
                  I := I + 1;
               else
                  J := I;
                  while J < Line'Last and then not Is_Space (Line (J + 1)) loop
                     J := J + 1;
                  end loop;
                  Count := Count + 1;
                  begin
                     Values.Append (Float'Value (Line (I .. J)));
                  exception
                     when Constraint_Error =>
                        Close (F);
                        Fail ("не число в позиции" & Natural'Image (Count)
                              & ": " & Line (I .. J));
                  end;
                  I := J + 1;
               end if;
            end loop;
         end;
      end loop;
      Close (F);
   end Read_Signal;

   function Median_At (Input : Float_Array; I : Positive) return Float is
      K   : constant Natural := (W - 1) / 2;
      Win : Float_Array (1 .. W);
      J   : Integer;
      X   : Float;
      P   : Natural;
   begin
      for D in -K .. K loop
         J := I + D;
         if J < Input'First then
            J := Input'First;
         elsif J > Input'Last then
            J := Input'Last;
         end if;
         Win (D + K + 1) := Input (J);
      end loop;
      for A in 2 .. W loop
         X := Win (A);
         P := A - 1;
         while P >= 1 and then Win (P) > X loop
            Win (P + 1) := Win (P);
            P := P - 1;
         end loop;
         Win (P + 1) := X;
      end loop;
      return Win (K + 1);
   end Median_At;

   task type Worker is
      entry Start (First, Last : Positive);
   end Worker;

   task body Worker is
      A, B : Positive;
   begin
      accept Start (First, Last : Positive) do
         A := First;
         B := Last;
      end Start;
      for I in A .. B loop
         Result (I) := Median_At (Data.all, I);
      end loop;
   end Worker;

   procedure Run_Parallel is
      Workers : array (1 .. T) of Worker;
   begin
      for K in 1 .. T loop
         Workers (K).Start ((K - 1) * N / T + 1, K * N / T);
      end loop;
   end Run_Parallel;

   procedure Generate is
      Count : Positive;
      G     : Ada.Numerics.Float_Random.Generator;
      X     : Float;
   begin
      if Argument_Count /= 2 then
         Usage;
      end if;
      Count := To_Positive (Argument (2), "количество");
      Ada.Numerics.Float_Random.Reset (G);
      for I in 1 .. Count loop
         X := 10.0 * Sin (Float (I) / 50.0);
         if Ada.Numerics.Float_Random.Random (G) < 0.05 then
            if Ada.Numerics.Float_Random.Random (G) < 0.5 then
               X := X + 50.0;
            else
               X := X - 50.0;
            end if;
         end if;
         Put_Line (Image (Float'Rounding (X * 100.0) / 100.0));
      end loop;
   end Generate;

   procedure Write_Result (Path : String) is
      F : File_Type;
   begin
      begin
         Create (F, Out_File, Path);
      exception
         when others =>
            Fail ("не удалось записать файл " & Path);
      end;
      for I in 1 .. N loop
         Put_Line (F, Image (Result (I)));
      end loop;
      Close (F);
   end Write_Result;

   function Ms (D : Duration) return String is
      Buf : String (1 .. 20);
   begin
      Ada.Float_Text_IO.Put (Buf, Float (D) * 1000.0, Aft => 2, Exp => 0);
      return Trim (Buf, Ada.Strings.Both);
   end Ms;

   Path     : Unbounded_String;
   Out_Path : Unbounded_String;
   Pos      : Natural := 0;
   I        : Positive := 1;
   Values   : Float_Vectors.Vector;
   Start    : Time;
   Time1    : Duration;
   Time2    : Duration;

begin
   if Argument_Count = 0 then
      Usage;
   end if;
   if Argument (1) = "gen" then
      Generate;
      return;
   end if;

   while I <= Argument_Count loop
      if Argument (I) = "-o" then
         if I = Argument_Count then
            Usage;
         end if;
         I := I + 1;
         Out_Path := To_Unbounded_String (Argument (I));
      else
         Pos := Pos + 1;
         case Pos is
            when 1 =>
               Path := To_Unbounded_String (Argument (I));
            when 2 =>
               declare
                  V : Integer;
               begin
                  V := Integer'Value (Argument (I));
                  if V <= 0 or else V mod 2 = 0 then
                     raise Constraint_Error;
                  end if;
                  W := V;
               exception
                  when Constraint_Error =>
                     Fail ("размер окна должен быть нечётным положительным числом");
               end;
            when 3 =>
               T := To_Positive (Argument (I), "число задач");
            when others =>
               Usage;
         end case;
      end if;
      I := I + 1;
   end loop;
   if Pos < 2 then
      Usage;
   end if;

   Read_Signal (To_String (Path), Values);
   if Values.Is_Empty then
      Fail ("сигнал пуст");
   end if;

   N := Natural (Values.Length);
   Data := new Float_Array (1 .. N);
   Result := new Float_Array (1 .. N);
   Check := new Float_Array (1 .. N);
   for K in 1 .. N loop
      Data (K) := Values (K);
   end loop;
   if T > N then
      T := N;
   end if;

   Start := Clock;
   for K in 1 .. N loop
      Check (K) := Median_At (Data.all, K);
   end loop;
   Time1 := To_Duration (Clock - Start);

   Start := Clock;
   Run_Parallel;
   Time2 := To_Duration (Clock - Start);

   if Length (Out_Path) > 0 then
      Write_Result (To_String (Out_Path));
      Put_Line ("результат записан в " & To_String (Out_Path));
   elsif N <= 50 then
      for K in 1 .. N loop
         Put (Image (Result (K)));
         if K < N then
            Put (" ");
         end if;
      end loop;
      New_Line;
   else
      Put_Line ("сигнал слишком длинный для консоли, укажите -o <файл>");
   end if;

   Put_Line ("значений:" & Natural'Image (N) & ", окно:" & Positive'Image (W));
   Put_Line ("последовательно: " & Ms (Time1) & " мс");
   Put_Line ("параллельно, задач" & Positive'Image (T) & ": " & Ms (Time2) & " мс");
   if Check.all = Result.all then
      Put_Line ("результаты совпадают: да");
   else
      Put_Line ("результаты совпадают: нет");
   end if;

exception
   when Stop =>
      Set_Exit_Status (Failure);
end Median;
